#!/usr/bin/env python3

import ast
import importlib.util
import json
import os
import pathlib
import subprocess
import tempfile
import unittest
from types import SimpleNamespace
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
LAUNCHER = ROOT / "scripts/bookmark-manager"
CONFIG = ROOT / ".config/qtile/config.py"
DEV_ID = "aocepclkpgckjeikiphffdlileoaceec"
STORE_ID = "jpgfhlaplofoaempbhliigmjbpofeghk"


class BookmarkLauncherTest(unittest.TestCase):
    def setUp(self):
        worktrees = next((path for path in ROOT.parents if path.name == ".worktrees"), None)
        if worktrees is None:
            common = subprocess.check_output(
                ["git", "rev-parse", "--path-format=absolute", "--git-common-dir"],
                cwd=ROOT, text=True, timeout=2).strip()
            worktrees = pathlib.Path(common).parent / ".worktrees"
        temporary_root = worktrees / ".tmp"
        temporary_root.mkdir(parents=True, exist_ok=True)
        directory = tempfile.TemporaryDirectory(prefix="bookmark-", dir=temporary_root)
        self.addCleanup(directory.cleanup)
        self.base = pathlib.Path(directory.name)
        self.home = self.base / "home"
        self.home.mkdir()
        self.bin = self.base / "bin"
        self.bin.mkdir()
        self.chrome = self.bin / "fake chrome"
        self.log = self.base / "chrome.json"
        self.chrome.write_text(
            "#!/usr/bin/env python3\nimport json, os, pathlib, sys\n"
            'pathlib.Path(os.environ["TEST_CHROME_LOG"]).write_text(json.dumps(sys.argv[1:]))\n'
            'raise SystemExit(int(os.environ.get("TEST_CHROME_STATUS", "0")))\n')
        self.chrome.chmod(0o755)
        self.mode = self.bin / "rofi-chrome-mode"
        self.mode.symlink_to(ROOT / "scripts/rofi-chrome-mode")
        self.state = self.base / "state"
        (self.state / "rofi-chrome").mkdir(parents=True)
        self.env = {key: value for key, value in os.environ.items()
                    if not key.startswith("ABUSH_")}
        self.env.update(HOME=str(self.home), XDG_STATE_HOME=str(self.state),
                        PATH=f"{self.bin}:{os.defpath}", ABUSH_CHROME_BIN=str(self.chrome),
                        TEST_CHROME_LOG=str(self.log))

    def launch(self, *arguments, **environment):
        self.log.unlink(missing_ok=True)
        return subprocess.run([LAUNCHER, *arguments], env={**self.env, **environment},
                              capture_output=True, text=True, timeout=3)

    def assert_launch(self, result, profile, extension_id=DEV_ID):
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(self.log.read_text()), [
            f"--user-data-dir={profile}", "--no-default-browser-check",
            f"--app=chrome-extension://{extension_id}/bookmarks.html",
        ])
        self.assertFalse((self.home / ".config/google-chrome-bookmarks").exists())

    def test_default_uses_the_main_profile_with_its_existing_extension_and_bookmarks(self):
        profile = self.home / ".config/google-chrome-main"
        (profile / "Default").mkdir(parents=True)
        sentinel = profile / "Default/Bookmarks"
        sentinel.write_text('{"existing":"personal library"}')
        self.assert_launch(self.launch(), profile)
        self.assertEqual(sentinel.read_text(), '{"existing":"personal library"}')

    def test_dev_and_store_modes_open_only_their_exact_manager_page(self):
        for mode, extension_id in (("dev", DEV_ID), ("store", STORE_ID)):
            with self.subTest(mode=mode):
                (self.state / "rofi-chrome/mode").write_text(mode + "\n")
                self.assert_launch(self.launch(), self.home / ".config/google-chrome-main",
                                   extension_id)

    def test_main_profile_override_is_shared_without_a_bookmark_override(self):
        profile = self.base / "main profile with spaces"
        self.assert_launch(self.launch(ABUSH_CHROME_PROFILE=str(profile)), profile)

    def test_explicit_bookmark_profile_override_takes_precedence(self):
        profile = self.base / "explicit bookmark profile"
        self.assert_launch(self.launch(ABUSH_CHROME_PROFILE=str(self.base / "main"),
                                       ABUSH_BOOKMARK_PROFILE=str(profile)), profile)

    def test_empty_profile_overrides_use_the_main_default(self):
        self.assert_launch(self.launch(ABUSH_CHROME_PROFILE="", ABUSH_BOOKMARK_PROFILE=""),
                           self.home / ".config/google-chrome-main")

    def replace_mode(self, output, status=0):
        self.mode.unlink(missing_ok=True)
        self.mode.write_text("#!/usr/bin/env python3\nimport sys\n"
                             f"print({output!r})\nraise SystemExit({status})\n")
        self.mode.chmod(0o755)

    def test_invalid_or_failed_mode_resolution_never_launches_chrome(self):
        for output, status in (("", 0), ("other", 0), (DEV_ID + "/other.html", 0),
                               (DEV_ID + "\n" + STORE_ID, 0), (DEV_ID, 7)):
            with self.subTest(output=output, status=status):
                self.replace_mode(output, status)
                result = self.launch()
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse(self.log.exists(), "invalid identity must fail before launch")

    def test_fixed_launcher_rejects_urls_and_flags_instead_of_forwarding_them(self):
        for argument in ("https://example.test/", "--app=https://example.test/"):
            with self.subTest(argument=argument):
                result = self.launch(argument)
                self.assertEqual(result.returncode, 2)
                self.assertIn("usage:", result.stderr)
                self.assertFalse(self.log.exists())

    def test_chrome_failure_is_not_reported_as_success_or_retried(self):
        result = self.launch(TEST_CHROME_STATUS="9")
        self.assertEqual(result.returncode, 9)
        self.assertEqual(len(json.loads(self.log.read_text())), 3)


@unittest.skipUnless(importlib.util.find_spec("libqtile"), "Qtile is not installed")
class BookmarkScratchpadTest(unittest.TestCase):
    def setUp(self):
        import libqtile.backend.base  # Initialize Qtile's circular window/scratchpad imports.
        from libqtile import config, lazy, scratchpad

        self.events = []
        self.hooks = mock.Mock()
        patch = mock.patch.object(scratchpad, "hook", self.hooks)
        patch.start()
        self.addCleanup(patch.stop)
        self.ns = dict(Match=config.Match, MatchAny=config.MatchAny, ScratchPad=config.ScratchPad,
                       DropDown=config.DropDown, Qtile=object, QtileWindow=OfflineWindow,
                       DropDownToggler=scratchpad.DropDownToggler, hook=self.hooks, lazy=lazy.lazy)
        tree = ast.parse(CONFIG.read_text())
        names = {"_scratchpad_window_alive", "_drop_scratchpad_dropdown",
                 "_adopt_scratchpad_window", "_hide_exclusive_scratchpads", "toggle_scratchpad",
                 "register_scratchpad_window", "recover_scratchpad_dropdowns",
                 "before_window_created", "after_window_created", "window_killed"}
        nodes = []
        for node in tree.body:
            if isinstance(node, ast.Assign) and any(
                    isinstance(target, ast.Name) and target.id in {
                        "scratchpad_matches", "exclusive_scratchpads"} for target in node.targets):
                nodes.append(node)
            elif isinstance(node, ast.FunctionDef) and node.name in names:
                node.decorator_list = []
                nodes.append(node)
        exec(compile(ast.fix_missing_locations(ast.Module(nodes, [])), str(CONFIG), "exec"),
             self.ns)
        groups = next(node.value for node in tree.body if isinstance(node, ast.Assign)
                      and any(isinstance(target, ast.Name) and target.id == "groups"
                              for target in node.targets))
        configured = eval(compile(ast.Expression(groups.elts[0]), str(CONFIG), "eval"), self.ns)
        self.ns["groups"] = [configured]
        self.scratchpad = scratchpad.ScratchPad("scratchpad", configured.dropdowns)
        self.manager = SimpleNamespace(
            windows_map={}, groups_map={"scratchpad": self.scratchpad},
            current_window=None, spawn=mock.Mock(return_value=4242),
            core=SimpleNamespace(name="x11", conn=SimpleNamespace(
                atoms={"_NET_WM_STATE_SKIP_TASKBAR": 99})),
            config=SimpleNamespace(floating_layout=SimpleNamespace(border_width=0)),
            current_screen=SimpleNamespace(dx=0, dy=44, dwidth=1920, dheight=1036,
                                           x=0, y=0, width=1920, height=1080))
        self.scratchpad.qtile = self.manager
        for name in ("e", "f", "c"):
            self.manager.groups_map[name] = SimpleNamespace(name=name,
                screen=self.manager.current_screen)
        self.manager.current_group = self.manager.groups_map["e"]
        self.ns.update(qtile=self.manager, shell=SimpleNamespace(
            register=mock.Mock(return_value=False), on_client_killed=mock.Mock()))

    def window(self, instance=None, wid=22, role="pop-up"):
        return OfflineWindow(self.manager, self.events, wid,
                             (instance or f"{DEV_ID}__bookmarks.html", "Google-chrome"), role)

    def toggle(self):
        call = self.ns["toggle_scratchpad"]("bookmarks")
        self.assertEqual(call.name, "function")
        call.args[0](self.manager)

    def state(self):
        dropdown = self.scratchpad.dropdowns.get("bookmarks")
        return (dropdown.window.wid if dropdown else None,
                dropdown.window.group.name if dropdown else None,
                dropdown.shown if dropdown else False,
                tuple(self.scratchpad._spawned), self.manager.spawn.call_count)

    def test_exact_native_app_identities_match_without_claiming_other_chrome_windows(self):
        match = self.ns["scratchpad_matches"]["bookmarks"]
        for instance in (f"{DEV_ID}__bookmarks.html", f"{STORE_ID}__bookmarks.html",
                         "bookmark-manager"):
            with self.subTest(instance=instance):
                self.assertTrue(match.compare(self.window(instance)))
        for instance in ("google-chrome", f"{DEV_ID}__popup.html", f"{STORE_ID}__download.html",
                         f"{DEV_ID}__bookmarks.html.evil", "other__bookmarks.html",
                         "AbushBrowser", "ahonjbfnmpjgjgaabbppofphidfomgcj__work.html"):
            with self.subTest(instance=instance):
                self.assertFalse(match.compare(self.window(instance)))

    def test_spawn_manage_hide_and_show_on_another_desktop_keeps_one_window(self):
        trace = [self.state()]
        self.toggle()
        trace.append(self.state())
        self.manager.spawn.assert_called_once_with("bookmark-manager")
        window = self.window()
        self.ns["before_window_created"](window)
        self.assertEqual(self.events, [], "the pre-manage hook must not move or focus Chrome")
        self.manager.windows_map[window.wid] = window
        self.scratchpad.on_client_new(window)
        self.ns["after_window_created"](window)
        trace.append(self.state())
        self.assertEqual(window.window.set_property.call_count, 1)
        self.assertFalse(any(event[0].startswith("generic") for event in self.events),
                         "generic Chrome popup sizing must not overwrite bookmark geometry")
        self.assertEqual((window.width, window.height), (1536, 932))
        window.has_focus = True
        self.toggle()
        trace.append(self.state())
        window.has_focus = False
        self.manager.current_group = self.manager.groups_map["f"]
        self.events.clear()
        self.toggle()
        trace.append(self.state())
        self.assertEqual(self.events, [
            ("group", 22, "f"), ("place", 22, 192, 95, 1536, 932),
            ("group", 22, "f"), ("front", 22), ("front", 22), ("focus", 22, True),
        ])
        self.assertEqual(trace, [
            (None, None, False, (), 0), (None, None, False, ("bookmarks",), 1),
            (22, "e", True, (), 1), (22, "scratchpad", False, (), 1),
            (22, "f", True, (), 1),
        ])
        self.manager.spawn.assert_called_once()

    def test_reload_and_adoption_reuse_an_exact_existing_manager(self):
        window = self.window(f"{STORE_ID}__bookmarks.html")
        self.manager.windows_map[window.wid] = window
        self.ns["recover_scratchpad_dropdowns"]()
        self.assertEqual(self.state(), (22, "scratchpad", False, (), 0))
        self.ns["recover_scratchpad_dropdowns"]()
        self.assertEqual(window.window.set_property.call_count, 1)
        self.toggle()
        self.assertEqual(self.state(), (22, "e", True, (), 0))
        self.manager.spawn.assert_not_called()
        self.ns["window_killed"](window)
        self.manager.windows_map.pop(window.wid)
        self.assertEqual(self.state(), (None, None, False, (), 0))
        self.toggle()
        self.assertEqual(self.state(), (None, None, False, ("bookmarks",), 1))

    def test_existing_untracked_manager_is_adopted_before_spawning(self):
        window = self.window()
        self.manager.windows_map[window.wid] = window
        self.toggle()
        self.assertEqual(self.state(), (22, "e", True, (), 0))
        self.manager.spawn.assert_not_called()

    def test_managed_fallback_registers_bookmarks_and_skips_generic_geometry(self):
        window = self.window()
        self.manager.windows_map[window.wid] = window
        self.ns["after_window_created"](window)
        self.assertEqual(self.state(), (22, "e", True, (), 0))
        self.assertFalse(any(event[0].startswith("generic") for event in self.events))

    def test_stale_manager_binding_is_dropped_before_one_new_spawn(self):
        window = self.window()
        self.manager.windows_map[window.wid] = window
        self.ns["after_window_created"](window)
        self.manager.windows_map.pop(window.wid)
        self.toggle()
        self.assertEqual(self.state(), (None, None, False, ("bookmarks",), 1))
        self.toggle()
        self.assertEqual(self.manager.spawn.call_count, 1)

    def test_unrelated_windows_do_not_satisfy_the_bookmark_spawn_waiter(self):
        self.toggle()
        for instance in ("google-chrome", f"{DEV_ID}__download.html", "AbushBrowser"):
            window = self.window(instance)
            self.ns["before_window_created"](window)
            self.scratchpad.on_client_new(window)
            self.assertEqual(self.state(), (None, None, False, ("bookmarks",), 1))
            self.assertEqual(self.events, [])


class OfflineWindow:
    def __init__(self, manager, events, wid, classes, role):
        self.qtile, self.events, self.wid = manager, events, wid
        self.classes, self.role = classes, role
        self.name = "Bookmarks"
        self.group = manager.groups_map["f"]
        self.has_focus = False
        self.floating = True
        self.x, self.y, self.width, self.height = 0, 0, 800, 600
        self.bordercolor = "000000"
        self.window = mock.Mock()
        self.window.get_property.return_value = []
        self.window.get_name.return_value = self.name

    def get_wm_class(self):
        return self.classes

    def get_wm_instance_class(self):
        return self.classes[0]

    def get_wm_role(self):
        return self.role

    def get_pid(self):
        return 4242

    def togroup(self, group=None):
        group = group or self.qtile.current_group.name
        self.group = self.qtile.groups_map[group]
        self.events.append(("group", self.wid, group))
        if group == "scratchpad":
            self.has_focus = False

    def place(self, x, y, width, height, *_args, **_kwargs):
        self.x, self.y, self.width, self.height = x, y, width, height
        self.events.append(("place", self.wid, x, y, width, height))

    def bring_to_front(self):
        self.events.append(("front", self.wid))

    def focus(self, warp=False):
        self.events.append(("focus", self.wid, warp))
        self.has_focus = True

    def enable_floating(self):
        self.events.append(("generic-floating", self.wid))

    def set_size_floating(self, *size):
        self.events.append(("generic-size", *size))

    def set_position_floating(self, *position):
        self.events.append(("generic-position", *position))


if __name__ == "__main__":
    unittest.main()
