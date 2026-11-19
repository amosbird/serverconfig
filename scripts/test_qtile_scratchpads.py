#!/usr/bin/env python3

import pathlib
import unittest
from types import SimpleNamespace
from unittest import mock

from qtile_test_support import FakeWindow, OfflineQtile, window_effects

ROOT = pathlib.Path(__file__).resolve().parents[1]
TELEGRAM = ROOT / "scripts/tdesktop-open"


class QtileScratchpadTest(unittest.TestCase):
    def setUp(self):
        self.fixture = self.enterContext(OfflineQtile())
        self.config = self.fixture.load()
        self.manager = self.fixture.manager
        self.manager.rebuild()
        self.module = self.fixture.module("scratchpads")
        self.runtime = self.fixture.module("runtime")
        self.manager.trace.clear()

    def test_all_dropdown_shortcuts_use_unified_raise_and_focus_toggle(self):
        shortcuts = {"1": "ioa", "4": "stalonetray", "8": "chatgpt", "9": "webchat",
                     "0": "tdesktop", "minus": "stardict", "b": "bookmarks"}
        for key, name in shortcuts.items():
            with self.subTest(key=key):
                binding = [binding for binding in self.config.keys
                           if binding.key == key and set(binding.modifiers) == {"control", "mod1"}]
                self.assertEqual(len(binding), 1)
                scratchpad = self.manager.groups_map["scratchpad"]
                with mock.patch.object(scratchpad, "dropdown_toggle") as toggle:
                    call = binding[0].commands[0]
                    self.assertEqual(call.name, "function")
                    call.args[0](self.manager)
                    toggle.assert_called_once_with(name)

    def test_toggle_adopts_existing_window_instead_of_hanging_on_systemd_spawn(self):
        window = FakeWindow(self.manager, name="iOA")
        self.module.toggle_scratchpad("ioa").args[0](self.manager)
        self.assertIs(self.manager.groups_map["scratchpad"].dropdowns["ioa"].window, window)
        self.assertFalse(any(event[0] == "spawn" for event in self.manager.trace))
        self.assertIn("window.focus", [event[0] for event in self.manager.trace])

    def test_reload_recovers_orphan_dropdown_windows_before_state_is_saved(self):
        window = FakeWindow(self.manager, name="iOA")
        before = window.snapshot()
        self.module.recover_windows(self.manager)
        dropdown = self.manager.groups_map["scratchpad"].dropdowns["ioa"]
        self.assertIs(dropdown.window, window)
        self.assertTrue(dropdown.shown)
        self.assertEqual(window.snapshot(), before)
        self.assertEqual(window_effects(self.manager), [])

    def test_the_tray_row_stays_centered_however_many_icons_it_holds(self):
        window = FakeWindow(self.manager, name="stalonetray")
        window._place = mock.Mock(side_effect=window.place)
        patches = self.fixture.module("patches")
        for width in (96, 288, 480):
            window.width = width
            patches.center_tray(window)
            self.assertEqual((window.x, window.y), ((1920 - width) // 2, (1080 - 555) // 2))
            window._place.reset_mock()
            patches.center_tray(window)
            window._place.assert_not_called()
        # Native hints can change the requested size: center the actual resulting size.
        def native_place(x, y, width, height, *_args, **_kwargs):
            window.x, window.y = x, y
            window.width, window.height = min(width, 288), min(height, 96)
        window._place.side_effect = native_place
        patches.new_place(window, 0, 0, 500, 300, 8, "red")
        self.assertEqual((window.x, window.y, window.width, window.height), (816, 492, 288, 96))
        window.group = self.manager.screens[1].group
        patches.center_tray(window)
        self.assertEqual((window.x, window.y), (2736, 492))

    def test_tray_drops_the_fcitx5_xembed_icon(self):
        source = (ROOT / "scripts/tray").read_text()
        self.assertIn("exec stalonetray --icon-size=96 --kludges=force_icons_size", source)
        self.assertNotIn("fcitx5-remote", source)
        self.assertIn("ignore_classes fcitx", (ROOT / ".stalonetrayrc").read_text())
        dropdown = next(group for group in self.config.groups
                        if group.name == "scratchpad").dropdowns
        self.assertEqual(next(dd.command for dd in dropdown if dd.name == "stalonetray"), "tray")
        self.assertIn('run "tray" systemctl --user start tray.service',
                      (ROOT / "scripts/startup").read_text())
        unit = (ROOT / "systemd/tray.service").read_text()
        self.assertIn("ExecStart=/home/amos/scripts/tray", unit)
        self.assertIn("Restart=always", unit)
        self.assertIn("systemd/tray.service", (ROOT / "restore.sh").read_text())

    def test_bookmark_manager_is_a_reusable_scratchpad(self):
        source = (ROOT / "scripts/bookmark-manager").read_text()
        for expected in ('--app="$url"', "rofi-chrome-mode id",
                         'chrome-extension://$extension_id/bookmarks.html',
                         'profile=${ABUSH_BOOKMARK_PROFILE:-${ABUSH_CHROME_PROFILE:-',
                         '$HOME/.config/google-chrome-main', 'exec "$chrome"'):
            self.assertIn(expected, source)
        for forbidden in ('google-chrome-bookmarks', '--class=', '/home/amos/scripts/chromium'):
            self.assertNotIn(forbidden, source)
        chromium = (ROOT / "scripts/chromium").read_text()
        for expected in ('if [[ ${1:-} == --current-group ]]; then', 'focus_group=false',
                         'flags and app launches require a dedicated profile', 'if $focus_group; then'):
            self.assertIn(expected, chromium)
        self.assertLess(chromium.index('exec python3 "$root/scripts/ordinary-open-client.py" "$url"'),
                        chromium.rindex("focus_main_group"))
        self.assertEqual(self.manager.groups_map["scratchpad"]._dropdownconfig["bookmarks"].command,
                         "bookmark-manager")

    def test_bookmark_toggle_moves_hidden_window_to_current_group_before_showing(self):
        window = FakeWindow(self.manager, name="iOA", group="scratchpad")
        self.module.recover_windows(self.manager)
        self.module.toggle_scratchpad("ioa").args[0](self.manager)
        effects = window_effects(self.manager)
        move = next(index for index, event in enumerate(effects) if event[0] == "window.togroup")
        place = next(index for index, event in enumerate(effects) if event[0] == "window.place")
        self.assertLess(move, place)
        self.assertEqual(window.group.name, "h")
        self.assertTrue(window.has_focus)

    def test_matching_bookmark_window_is_registered_before_generic_popup_geometry(self):
        from test_bookmark_manager import OfflineWindow, DEV_ID
        events = []
        self.enterContext(mock.patch.object(self.module, "QtileWindow", OfflineWindow))
        window = OfflineWindow(self.manager, events, 101,
                               (f"{DEV_ID}__bookmarks.html", "Google-chrome"), "pop-up")
        self.manager.windows_map[101] = window
        self.runtime.after_window_created(window)
        self.assertFalse(any(event[0].startswith("generic") for event in events))
        self.assertIs(self.manager.groups_map["scratchpad"].dropdowns["bookmarks"].window, window)

    def test_shell_is_recovered_after_reload_without_spawning_duplicates(self):
        first = FakeWindow(self.manager, name="urxvt_scratchpad")
        focused = FakeWindow(self.manager, 102, name="urxvt_scratchpad", focused=True)
        before = (first.snapshot(), focused.snapshot())
        self.module.recover_windows(self.manager)
        self.module.recover_windows(self.manager)
        self.assertIs(self.module.shell_holder(self.manager).shell.window, focused)
        self.assertEqual((first.snapshot(), focused.snapshot()), before)
        self.assertEqual(self.manager.trace, [])

    def test_shell_spawn_action_runs_only_after_window_is_managed(self):
        holder = self.module.shell_holder(self.manager)
        holder.toggle_left()
        holder.toggle_left()
        self.assertEqual(sum(event[0] == "spawn" for event in self.manager.trace), 1)
        window = FakeWindow(self.manager, name="urxvt_scratchpad", focused=True)
        self.assertEqual(window_effects(self.manager), [])
        self.runtime.after_window_created(window)
        self.assertIsNone(holder._spawned)
        self.assertEqual((window.x, window.y, window.width, window.height), (0, 0, 960, 1080))
        self.assertEqual(window.group.name, "h")

    def test_a_shell_spawned_to_be_shown_is_not_toggled_away_again(self):
        holder = self.module.shell_holder(self.manager)
        holder.toggle_left()
        window = FakeWindow(self.manager, name="urxvt_scratchpad", focused=True)
        window.x, window.float_x, window.width, window.height = 0, 0, 960, 1080
        self.runtime.after_window_created(window)
        self.assertEqual(window.group.name, "h")
        holder.toggle_left()
        self.assertEqual(window.group.name, "scratchpad")
        holder.toggle_left()
        self.assertEqual(window.group.name, "h")

    def test_shell_right_and_tiled_spawn_requests_show_once_after_manage(self):
        for mode, action, floating, x in ((1, "toggle_right", True, 960),
                                          (0, "show_shell", False, 111)):
            with self.subTest(mode=mode):
                holder = self.module.ShellHolder()
                self.manager.groups_map["h"].toscreen = mock.Mock()
                getattr(holder, action)()
                window = FakeWindow(self.manager, wid=200 + mode, name="urxvt_scratchpad",
                                    focused=True)
                self.assertTrue(holder.register(window))
                self.assertIsNone(holder._spawned)
                self.assertEqual((window.group.name, window.floating, window.x), ("h", floating, x))
                self.assertFalse(holder.register(window))

    def test_a_screen_qtile_invented_without_an_output_is_thrown_away(self):
        invalid = SimpleNamespace()
        valid = SimpleNamespace(group=object())
        native = mock.Mock(return_value="processed")
        manager = SimpleNamespace(screens=[invalid, valid], current_screen=invalid,
                                  _process_screens_unpatched=native)
        result = self.fixture.module("patches").process_screens(manager, True)
        self.assertEqual(result, "processed")
        self.assertEqual(manager.screens, [valid])
        self.assertFalse(hasattr(manager, "current_screen"))
        native.assert_called_once_with(True)

    def test_shell_moves_to_current_group_and_marks_it_floating_before_resizing(self):
        window = FakeWindow(self.manager, group="scratchpad")
        self.module.Shell(window).show_left()
        names = [event[0] for event in window_effects(self.manager)]
        self.assertLess(names.index("window.togroup"), names.index("window.enable_floating"))
        self.assertLess(names.index("window.enable_floating"), names.index("window.place"))

    def test_floating_shell_is_not_left_in_tiled_window_list(self):
        window = FakeWindow(self.manager)
        self.manager.groups_map["h"].toscreen = mock.Mock()
        shell = self.module.Shell(window)
        shell.show_tiled()
        self.assertFalse(window.floating)
        shell.show_left()
        self.assertTrue(window.floating)
        self.assertEqual(window.group.name, "h")

    def test_floating_geometry_hooks_use_public_floating_api(self):
        for classes, title, size in ((["copyq"], "copyq", (2000, 1200)),
                                     (["kitty"], "float", (2000, 1200)),
                                     (["kitty"], "dtpick", (400, 120))):
            with self.subTest(title=title):
                window = mock.Mock()
                window.get_wm_class.return_value = classes
                window.window.get_name.return_value = title
                self.runtime.before_window_created(window)
                window.enable_floating.assert_called_once_with()
                window.set_size_floating.assert_called_once_with(*size)
                window.center.assert_called_once_with()
                calls = window.mock_calls
                self.assertLess(calls.index(mock.call.enable_floating()),
                                calls.index(mock.call.set_size_floating(*size)))

    def test_telegram_uri_handler_does_not_use_eval_or_private_qtile_api(self):
        launcher = TELEGRAM.read_text()
        self.assertNotIn("eval", launcher)
        self.assertNotIn("show_telegram", launcher)
        self.assertIn('exec /opt/telegram/Telegram -- "$@"', launcher)


if __name__ == "__main__":
    unittest.main()
