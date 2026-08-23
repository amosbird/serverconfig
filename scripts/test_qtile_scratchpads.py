#!/usr/bin/env python3

import pathlib
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
CONFIG = ROOT / ".config/qtile/config.py"
TELEGRAM = ROOT / "scripts/tdesktop-open"


class QtileScratchpadTest(unittest.TestCase):
    def setUp(self):
        self.config = CONFIG.read_text()

    def test_all_dropdown_shortcuts_use_unified_raise_and_focus_toggle(self):
        shortcuts = {
            "1": "ioa",
            "4": "stalonetray",
            "8": "chatgpt",
            "9": "stardict",
            "0": "tdesktop",
            "minus": "webchat",
        }
        for key, name in shortcuts.items():
            self.assertIn(
                f'Key([ctrl, alt], "{key}", toggle_scratchpad("{name}"))',
                self.config,
            )
        self.assertNotIn("def toggle_chatgpt", self.config)
        self.assertIn("def toggle_scratchpad(name):", self.config)
        self.assertIn("dropdown.window.bring_to_front()", self.config)
        self.assertIn("dropdown.window.focus(warp=True)", self.config)

    def test_reload_recovers_orphan_dropdown_windows_before_state_is_saved(self):
        self.assertIn("def register_scratchpad_window(window, hide=True):", self.config)
        self.assertIn("register_scratchpad_window(client)", self.config)
        self.assertIn("def recover_scratchpad_dropdowns():", self.config)
        self.assertIn("DropDownToggler(window, scratchpad.name, config)", self.config)
        self.assertIn("@hook.subscribe.startup", self.config)
        for name in (
            "ioa",
            "tdesktop",
            "webchat",
            "chatgpt",
            "stardict",
            "stalonetray",
            "bookmarks",
        ):
            self.assertIn(f'"{name}": Match(', self.config)

    def test_bookmark_manager_is_a_reusable_scratchpad(self):
        launcher = ROOT / "scripts/bookmark-manager"
        self.assertTrue(launcher.exists())
        source = launcher.read_text()
        self.assertIn('--app="$url"', source)
        self.assertIn("rofi-chrome-mode id", source)
        self.assertIn('chrome-extension://$extension_id/bookmarks.html', source)
        self.assertNotIn("--no-startup-window", source)
        self.assertNotIn("/json/version", source)
        self.assertNotIn("--class", source)
        self.assertIn("/home/amos/scripts/chromium", source)
        self.assertIn(
            'Key([ctrl, alt], "b", toggle_scratchpad("bookmarks"))',
            self.config,
        )
        self.assertIn("jpgfhlaplofoaempbhliigmjbpofeghk__bookmarks.html", self.config)
        self.assertIn("aocepclkpgckjeikiphffdlileoaceec__bookmarks.html", self.config)
        self.assertIn('"bookmark-manager",', self.config)
        self.assertEqual(self.config.count('Key([ctrl, alt], "b",'), 1)
        self.assertNotIn('Key([ctrl, alt], "b", lazy.spawn("scanqrcode"))', self.config)

    def test_matching_bookmark_window_is_registered_before_generic_popup_geometry(self):
        managed = self.config.split("def after_window_created(client):", 1)[1]
        self.assertIn('if scratchpad_matches["bookmarks"].compare(client):', managed)
        self.assertIn('register_scratchpad_window(client, hide=False)', managed)
        self.assertLess(
            managed.index('if scratchpad_matches["bookmarks"].compare(client):'),
            managed.index('client.get_wm_role() == "pop-up"'),
        )

    def test_shell_is_recovered_after_reload_without_spawning_duplicates(self):
        self.assertIn("def recover_shell():", self.config)
        self.assertIn("def recover(self, windows):", self.config)
        self.assertIn('window.name == "urxvt_scratchpad"', self.config)
        self.assertIn("window.kill()", self.config)
        self.assertIn("shell.recover(qtile.windows_map.values())", self.config)
        self.assertIn("def register(self, window):", self.config)
        self.assertIn("shell.register(client)", self.config)
        self.assertIn("if self.shell is not None or self._spawned is not None:", self.config)
        self.assertNotIn("hook.subscribe.client_new(self.on_client_new)", self.config)

    def test_shell_spawn_action_runs_only_after_window_is_managed(self):
        register = self.config.split("def register(self, window):", 1)[1].split(
            "def recover(self, windows):", 1
        )[0]
        self.assertIn("self.shell = Shell(window)", register)
        self.assertIn("self._show(mode)", register)
        managed = self.config.split("def after_window_created(client):", 1)[1]
        self.assertIn("shell.register(client)", managed)

    def test_shell_moves_to_current_group_and_marks_it_floating_before_resizing(self):
        show_float = self.config.split("def show_float(self, x, y):", 1)[1].split(
            "def show_tiled(self):", 1
        )[0]
        self.assertIn("win.togroup(win.qtile.current_group.name)", show_float)
        self.assertIn("win.enable_floating()", show_float)
        self.assertIn("win.place(", show_float)
        self.assertNotIn("win._float_state", show_float)
        self.assertNotIn("FloatStates", self.config)
        self.assertNotIn("mark_floating(", self.config)
        self.assertNotIn("win.set_size_floating", show_float)
        self.assertLess(
            show_float.index("win.togroup(win.qtile.current_group.name)"),
            show_float.index("win.enable_floating()"),
        )
        self.assertLess(
            show_float.index("win.enable_floating()"),
            show_float.index("win.place("),
        )

    def test_floating_shell_is_not_left_in_tiled_window_list(self):
        self.assertIn("win.disable_floating()", self.config)
        self.assertIn("win.enable_floating()", self.config)

    def test_floating_geometry_hooks_use_public_floating_api(self):
        hooks = self.config.split("def before_window_created(client):", 1)[1].split(
            "class ConditionalBorderColor", 1
        )[0]
        for marker in (
            'if "copyq" in client.get_wm_class():',
            'client.window.get_name() == "float"',
            'client.window.get_name() == "dtpick"',
            'client.get_wm_role() == "pop-up"',
        ):
            section = hooks.split(marker, 1)[1].split("elif ", 1)[0]
            self.assertIn("enable_floating()", section)

    def test_telegram_uri_handler_does_not_use_eval_or_private_qtile_api(self):
        launcher = TELEGRAM.read_text()
        self.assertNotIn("eval", launcher)
        self.assertNotIn("show_telegram", launcher)
        self.assertIn('exec /opt/telegram/Telegram -- "$@"', launcher)
        self.assertNotIn("def show_telegram", self.config)
        self.assertNotIn("scratchpad._dropdownconfig", self.config)
        self.assertNotIn("scratchpad._spawn(", self.config)


if __name__ == "__main__":
    unittest.main()
