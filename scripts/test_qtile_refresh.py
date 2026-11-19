#!/usr/bin/env python3
"""Offline refresh tests: real Qtile config types, no manager/backend connection."""

import copy
import importlib.util
import json
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from libqtile.config import DropDown, Match, MatchAny

from qtile_test_support import FakeWindow, OfflineQtile, window_effects


ROOT = Path(__file__).resolve().parents[1]
CONFIG = ROOT / ".config/qtile"
TEMP_ROOT = ROOT / ".worktrees/.tmp" if (ROOT / ".git").is_dir() else ROOT.parent / ".tmp"
SPEC = importlib.util.spec_from_file_location(
    "qtile_declarations_under_test", CONFIG / "amos_qtile/declarations.py"
)
declarations = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(declarations)
SOURCE = (CONFIG / "scratchpads.toml").read_text()


class NoEffects:
    def __getattr__(self, name):
        raise AssertionError(f"unexpected window/GUI effect: {name}")


class RefreshTests(unittest.TestCase):
    def setUp(self):
        TEMP_ROOT.mkdir(parents=True, exist_ok=True)
        self.temp = tempfile.TemporaryDirectory(prefix="qtile-refresh-test-",
                                                dir=TEMP_ROOT)
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        (self.root / "config.py").write_text("# offline configuration\n")
        package = self.root / "amos_qtile"
        package.mkdir()
        (package / "__init__.py").write_text("")
        self.path = self.root / "scratchpads.toml"
        self.path.write_text(SOURCE)
        snapshot = declarations.load(self.path)
        self.window = NoEffects()
        self.toggler = SimpleNamespace(window=self.window, visible=True, command="old")
        self.scratchpad = SimpleNamespace(
            _dropdownconfig={dd.name: dd for dd in declarations.build_dropdowns(snapshot)},
            _spawned={}, dropdowns={"ioa": self.toggler},
        )
        self.manager = SimpleNamespace(
            config=SimpleNamespace(file_path=str(self.root / "config.py")),
            groups_map={"scratchpad": self.scratchpad},
            _amos_runtime={
                "declarations": snapshot,
                "source_digest": declarations.capture_sources(self.root),
            },
        )

    def edit(self, old, new):
        self.path.write_text(self.path.read_text().replace(old, new))

    def assert_rejected_unchanged(self, expected=None):
        configs = self.scratchpad._dropdownconfig
        runtime = copy.deepcopy(self.manager._amos_runtime)
        pending = self.scratchpad._spawned.copy()
        result = declarations.apply(
            self.manager, expected or declarations.plan(self.manager)["source_digest"] or "0" * 64
        )
        self.assertFalse(result["ok"], result)
        self.assertFalse(result["applied"])
        self.assertTrue(result["errors"])
        self.assertIs(configs, self.scratchpad._dropdownconfig)
        self.assertEqual(runtime, self.manager._amos_runtime)
        self.assertEqual(pending, self.scratchpad._spawned)
        self.assertIs(self.scratchpad.dropdowns["ioa"], self.toggler)
        self.assertEqual(vars(self.toggler), {
            "window": self.window, "visible": True, "command": "old",
        })
        json.dumps(result)
        return result

    def test_exact_original_declarations(self):
        dropdowns = declarations.build_dropdowns(declarations.load())
        expected = [
            ("ioa", "systemctl --user start ioagui.service", .26, .224, .8, .35, 1, True),
            ("tdesktop", "/opt/telegram/Telegram", .1, .1, .8, .85, 1, False),
            ("webchat", "runchat", .1, .1, .8, .85, 1, False),
            ("chatgpt", "runai", .1, .1, .8, .85, 1, False),
            ("bookmarks", "bookmark-manager", .1, .05, .8, .9, 1, False),
            ("stardict", "kitty -T stardict -e dict.sh", .25, .1, .5, .8, .75, True),
            ("stalonetray", "tray", .1, 0, .8, .35, .9, True),
        ]
        for dd, values in zip(dropdowns, expected, strict=True):
            self.assertIsInstance(dd, DropDown)
            self.assertEqual((dd.name, dd.command, dd.x, dd.y, dd.width, dd.height,
                              dd.opacity, dd.on_focus_lost_hide), values)
            self.assertIs(dd.warp_pointer, True)

    def test_matchers_are_real_and_exact(self):
        matches = declarations.build_matches(declarations.load())
        self.assertIsInstance(matches["ioa"], Match)
        self.assertIsInstance(matches["bookmarks"], MatchAny)
        for name, field, value in [
            ("ioa", "title", "iOA"),
            ("tdesktop", "wm_class", "TelegramDesktop"),
            ("webchat", "wm_class", "webchat"),
            ("chatgpt", "wm_class", "chatgpt"),
            ("stardict", "title", "stardict"),
            ("stalonetray", "title", "stalonetray"),
            ("bookmarks", "wm_class", "bookmark-manager"),
            ("bookmarks", "wm_instance_class", "aocepclkpgckjeikiphffdlileoaceec__bookmarks.html"),
            ("bookmarks", "wm_instance_class", "jpgfhlaplofoaempbhliigmjbpofeghk__bookmarks.html"),
        ]:
            with self.subTest(name=name, value=value):
                def window(text):
                    return SimpleNamespace(
                        name=text if field == "title" else "other",
                        get_wm_class=lambda: (text, "other") if field == "wm_instance_class"
                        else ("other", text) if field == "wm_class" else ("other", "other"),
                    )
                self.assertTrue(matches[name].compare(window(value)))
                self.assertFalse(matches[name].compare(window(value + "-extra")))

    def test_refresh_only_swaps_future_configs_and_is_idempotent(self):
        old_configs = self.scratchpad._dropdownconfig
        pending = self.scratchpad._spawned
        pending["webchat"] = Match(title="pending unrelated launch")
        self.edit('command = "systemctl --user start ioagui.service"', 'command = "ioa-new"')
        self.edit('title = "iOA"', 'title = "iOA.new"')
        plan = declarations.plan(self.manager)
        self.assertTrue(plan["ok"], plan)
        self.assertEqual(plan["changes"], [{"name": "ioa", "fields": ["command", "match"]}])
        result = declarations.apply(self.manager, plan["source_digest"])
        self.assertTrue(result["applied"], result)
        self.assertTrue(result["changed"])
        self.assertIsNot(old_configs, self.scratchpad._dropdownconfig)
        self.assertEqual(old_configs["ioa"].command, "systemctl --user start ioagui.service")
        self.assertEqual(self.scratchpad._dropdownconfig["ioa"].command, "ioa-new")
        self.assertIs(self.scratchpad._spawned, pending)
        self.assertIs(self.scratchpad.dropdowns["ioa"], self.toggler)
        self.assertIs(self.toggler.window, self.window)
        self.assertTrue(self.toggler.visible)
        self.assertEqual(self.toggler.command, "old")
        configs = self.scratchpad._dropdownconfig
        again = declarations.apply(self.manager, plan["source_digest"])
        self.assertTrue(again["ok"])
        self.assertFalse(again["changed"])
        self.assertIs(configs, self.scratchpad._dropdownconfig)
        json.dumps(result)

    def test_changed_pending_rejects_entire_apply(self):
        waiter = Match(title="pending")
        self.scratchpad._spawned["ioa"] = waiter
        self.edit('command = "systemctl --user start ioagui.service"', 'command = "ioa-new"')
        self.edit('command = "runchat"', 'command = "webchat-new"')
        result = self.assert_rejected_unchanged()
        self.assertEqual(result["pending"], ["ioa"])
        self.assertIs(self.scratchpad._spawned["ioa"], waiter)

    def test_structural_fields_deferred_and_no_partial_command_update(self):
        for field, before, after in [
            ("x", "x = 0.26", "x = 0.25"),
            ("opacity", "opacity = 1.0", "opacity = 0.5"),
            ("on_focus_lost_hide", "on_focus_lost_hide = true", "on_focus_lost_hide = false"),
            ("warp_pointer", "warp_pointer = true", "warp_pointer = false"),
        ]:
            with self.subTest(field=field):
                self.path.write_text(SOURCE.replace(before, after, 1).replace(
                    'command = "runchat"', 'command = "new"'))
                result = self.assert_rejected_unchanged()
                self.assertTrue(result["requires_full_reload"])
                self.assertEqual(result["deferred_full_reload"], [
                    {"name": "ioa", "fields": [field]},
                ])

    def test_tray_matcher_is_not_a_future_only_change(self):
        self.edit('title = "stalonetray"', 'title = "different-existing-window"')
        result = self.assert_rejected_unchanged()
        self.assertTrue(result["requires_full_reload"])
        self.assertEqual(result["deferred_full_reload"], [
            {"name": "stalonetray", "fields": ["match"]},
        ])

    def test_name_and_order_require_full_reload(self):
        self.edit('name = "ioa"', 'name = "renamed"')
        self.assertTrue(self.assert_rejected_unchanged()["requires_full_reload"])
        self.path.write_text(SOURCE)
        blocks = SOURCE.split("[[dropdowns]]")
        blocks[1], blocks[2] = blocks[2], blocks[1]
        self.path.write_text("[[dropdowns]]".join(blocks))
        self.assertTrue(self.assert_rejected_unchanged()["requires_full_reload"])

    def test_python_changed_added_or_deleted_requires_full_reload(self):
        path = self.root / "amos_qtile/unknown.py"
        path.write_text("raise RuntimeError('must never execute')\n")
        result = self.assert_rejected_unchanged()
        self.assertTrue(result["requires_full_reload"])
        self.assertIn(str(path), result["changed_sources"])
        path.unlink()
        (self.root / "config.py").write_text("raise RuntimeError('must never execute')\n")
        self.assertTrue(self.assert_rejected_unchanged()["requires_full_reload"])
        self.manager._amos_runtime["source_digest"] = declarations.capture_sources(self.root)
        (self.root / "amos_qtile/__init__.py").unlink()
        self.assertTrue(self.assert_rejected_unchanged()["requires_full_reload"])
        (self.root / "config.py").unlink()
        self.assertTrue(self.assert_rejected_unchanged()["requires_full_reload"])

    def test_stale_digest_and_recheck_reject_without_mutation(self):
        digest = declarations.plan(self.manager)["source_digest"]
        self.edit('command = "runchat"', 'command = "new"')
        self.assertIn("Stale", " ".join(self.assert_rejected_unchanged(digest)["errors"]))
        digest = declarations.plan(self.manager)["source_digest"]
        original = declarations.build_dropdowns
        def race(snapshot):
            self.path.write_text(SOURCE)
            return original(snapshot)
        with patch.object(declarations, "build_dropdowns", side_effect=race):
            self.assertIn("during validation", " ".join(
                self.assert_rejected_unchanged(digest)["errors"]))

    def test_python_recheck_rejects_without_mutation(self):
        self.edit('command = "runchat"', 'command = "new"')
        digest = declarations.plan(self.manager)["source_digest"]
        original = declarations.build_dropdowns

        def race(snapshot):
            (self.root / "amos_qtile/added.py").write_text("# concurrent structural edit\n")
            return original(snapshot)

        with patch.object(declarations, "build_dropdowns", side_effect=race):
            self.assertIn("during validation", " ".join(
                self.assert_rejected_unchanged(digest)["errors"]))

    def test_oversized_declarations_are_rejected_before_application(self):
        variants = [SOURCE + "#" * (64 * 1024),
                    SOURCE.replace('command = "tray"', 'command = "' + "x" * 4097 + '"'),
                    SOURCE.replace('match = [{title = "iOA"}]',
                                   'match = [' + ','.join('{title = "iOA"}' for _ in range(17))
                                   + ']')]
        for source in variants:
            with self.subTest(length=len(source)):
                self.path.write_text(source)
                self.assert_rejected_unchanged()

    def test_invalid_data_and_syntax_have_no_effects(self):
        variants = [
            SOURCE + "\n[broken", SOURCE.replace("x = 0.26", "x = true"),
            SOURCE.replace("x = 0.26", "x = nan"), SOURCE.replace("x = 0.26", "x = inf"),
            SOURCE.replace("x = 0.26", "x = -0.1"),
            SOURCE.replace("height = 0.35", "height = 0.0"),
            SOURCE.replace("opacity = 1.0", "opacity = 1.1"),
            SOURCE.replace("warp_pointer = true", 'warp_pointer = "true"'),
            SOURCE.replace('name = "tdesktop"', 'name = "ioa"'),
            SOURCE.replace('name = "ioa"', 'name = ""'),
            SOURCE.replace('command = "tray"', 'command = ["tray"]'),
            SOURCE.replace('match = [{title = "iOA"}]', "match = []"),
            SOURCE.replace('title = "iOA"', 'func = "arbitrary"'),
            SOURCE.replace('title = "iOA"', 'title = 12'),
            SOURCE.replace("x = 0.26", "unknown = 3"),
        ]
        for index, source in enumerate(variants):
            with self.subTest(index=index):
                self.path.write_text(source)
                self.assertFalse(declarations.plan(self.manager)["ok"])
                self.assert_rejected_unchanged()


class SourceIdentityTests(unittest.TestCase):
    def test_running_source_identity_tracks_config_symlink_paths(self):
        import sys
        TEMP_ROOT.mkdir(parents=True, exist_ok=True)
        root = Path(self.enterContext(tempfile.TemporaryDirectory(
            prefix="qtile-source-link-", dir=TEMP_ROOT)))
        target = root / "target"
        target.mkdir()
        link = root / "linked"
        link.symlink_to(target, target_is_directory=True)
        path = link / "config.py"
        source = 'import sys\n__source_code__ = sys._getframe().f_code\nvalue = 1\n'
        path.write_text(source)
        namespace = {"__file__": str(path)}
        exec(compile(source, str(path), "exec"), namespace)
        self.assertEqual(declarations.capture_running_sources(link, namespace),
                         declarations.capture_sources(link))

    def test_running_source_identity_rejects_timestamp_stale_bytecode(self):
        import os
        import py_compile
        import sys
        TEMP_ROOT.mkdir(parents=True, exist_ok=True)
        root = Path(self.enterContext(tempfile.TemporaryDirectory(
            prefix="qtile-source-identity-", dir=TEMP_ROOT)))
        path = root / "config.py"
        old = 'import sys\n__source_code__ = sys._getframe().f_code\nvalue = True \n'
        new = old.replace("True ", "False")
        path.write_text(old)
        timestamp = path.stat().st_mtime_ns
        py_compile.compile(str(path), doraise=True,
                           invalidation_mode=py_compile.PycInvalidationMode.TIMESTAMP)
        path.write_text(new)
        os.utime(path, ns=(timestamp, timestamp))
        spec = importlib.util.spec_from_file_location("offline_stale_config", path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        self.assertTrue(module.value, "fixture must execute stale timestamp bytecode")
        with self.assertRaisesRegex(ValueError, "differs from executed"):
            declarations.capture_running_sources(root, vars(module))

    def test_source_edit_after_evaluation_cannot_become_a_loaded_baseline(self):
        TEMP_ROOT.mkdir(parents=True, exist_ok=True)
        root = Path(self.enterContext(tempfile.TemporaryDirectory(
            prefix="qtile-source-race-", dir=TEMP_ROOT)))
        path = root / "config.py"
        source = 'import sys\n__source_code__ = sys._getframe().f_code\nvalue = 1\n'
        path.write_text(source)
        namespace = {"__file__": str(path)}
        exec(compile(source, str(path), "exec"), namespace)
        self.assertEqual(declarations.capture_running_sources(root, namespace),
                         declarations.capture_sources(root))
        path.write_text(source.replace("value = 1", "value = 2"))
        with self.assertRaisesRegex(ValueError, "differs from executed"):
            declarations.capture_running_sources(root, namespace)


class RuntimeRefreshTests(unittest.TestCase):
    def setUp(self):
        self.fixture = self.enterContext(OfflineQtile())
        self.fixture.load()
        self.manager = self.fixture.manager
        self.manager.rebuild()
        self.runtime = self.fixture.module("runtime")
        TEMP_ROOT.mkdir(parents=True, exist_ok=True)
        self.root = Path(self.enterContext(tempfile.TemporaryDirectory(
            prefix="qtile-runtime-refresh-", dir=TEMP_ROOT)))
        (self.root / "config.py").write_text("# offline configuration\n")
        (self.root / "scratchpads.toml").write_text(SOURCE)
        self.manager.config.file_path = str(self.root / "config.py")
        self.runtime.get_state(self.manager)["source_digest"] = declarations.capture_sources(
            self.root)
        self.notifications = []
        self.enterContext(patch.object(self.runtime, "send_notification",
                                      side_effect=lambda *args: self.notifications.append(args)))
        self.manager.trace.clear()

    def shortcut(self, modifiers):
        keys = [key for key in self.manager.config.keys
                if key.key == "0" and set(key.modifiers) == set(modifiers)]
        self.assertEqual(len(keys), 1)
        self.assertEqual(len(keys[0].commands), 1)
        return keys[0].commands[0]

    def test_shortcuts_separate_refresh_from_explicit_full_reload(self):
        refresh = self.shortcut(["mod4"])
        full = self.shortcut(["mod4", "shift"])
        self.assertEqual(refresh.name, "function")
        self.assertIs(refresh.args[0], self.runtime.refresh_config)
        self.assertEqual(full.name, "reload_config")
        self.assertEqual(full.selectors, [])
        self.assertEqual(full.args, ())
        result = refresh.args[0](self.manager)
        self.assertTrue(result["ok"], result)
        self.assertEqual(self.notifications, [
            ("Qtile configuration", "No supported declaration changes")])
        self.assertEqual(window_effects(self.manager), [])
        self.assertFalse(any(event[0].startswith("native.") for event in self.manager.trace))

    def test_shortcut_rejections_explain_structural_and_pending_failures(self):
        path = self.root / "scratchpads.toml"
        scratchpad = self.manager.groups_map["scratchpad"]
        original = scratchpad._dropdownconfig
        for reason in ("structural", "pending"):
            with self.subTest(reason=reason):
                scratchpad._spawned.clear()
                if reason == "structural":
                    path.write_text(SOURCE.replace("x = 0.26", "x = 0.25", 1))
                else:
                    scratchpad._spawned["ioa"] = Match(title="pending")
                    path.write_text(SOURCE.replace(
                        'command = "systemctl --user start ioagui.service"',
                        'command = "offline-new"'))
                self.notifications.clear()
                result = self.shortcut(["mod4"]).args[0](self.manager)
                self.assertFalse(result["ok"], result)
                self.assertNotEqual(result.get("applied"), True)
                self.assertIs(scratchpad._dropdownconfig, original)
                self.assertEqual(len(self.notifications), 1)
                title, message = self.notifications[0]
                self.assertEqual(title, "Qtile configuration")
                for error in result["errors"]:
                    self.assertIn(error, message)
                if reason == "structural":
                    self.assertIn("Super+Shift+0", message)
                else:
                    self.assertEqual(result["pending"], ["ioa"])
                    self.assertIn("ioa", message)
                self.assertEqual(window_effects(self.manager), [])
                self.assertFalse(any(event[0].startswith("native.")
                                     for event in self.manager.trace))

    def test_actual_refresh_discovery_uses_current_dropdown_not_module_defaults(self):
        path = self.root / "scratchpads.toml"
        path.write_text(SOURCE.replace('title = "iOA"', 'title = "iOA.refreshed"'))
        old = FakeWindow(self.manager, wid=701, name="iOA")
        new = FakeWindow(self.manager, wid=702, name="iOA.refreshed")
        before = (old.snapshot(), new.snapshot())
        result = self.runtime.refresh_config(self.manager)
        self.assertTrue(result["applied"], result)
        self.assertEqual(result["changes"], [{"name": "ioa", "fields": ["match"]}])
        self.runtime.get_state(self.manager)["initializing"] = True
        from libqtile import hook
        hook.fire("client_managed", old)
        self.assertNotIn("ioa", self.manager.groups_map["scratchpad"].dropdowns)
        hook.fire("client_managed", new)
        dropdown = self.manager.groups_map["scratchpad"].dropdowns["ioa"]
        self.assertIs(dropdown.window, new)
        self.assertEqual((old.snapshot(), new.snapshot()), before)
        self.assertEqual(window_effects(self.manager), [])


if __name__ == "__main__":
    unittest.main()
