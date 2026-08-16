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
        self.assertIn("--no-startup-window", source)
        self.assertIn("/json/version", source)
        self.assertIn("for _ in {1..100}", source)
        self.assertIn("url=chrome-extension://aocepclkpgckjeikiphffdlileoaceec/bookmarks.html", source)
        self.assertIn('--app="$url"', source)
        self.assertIn("--remote-debugging-port=9222", source)
        self.assertIn("--load-extension=", source)
        self.assertNotIn("--class", source)
        self.assertNotIn("scripts/chromium", source)
        self.assertIn(
            'Key([ctrl, alt], "b", toggle_scratchpad("bookmarks"))',
            self.config,
        )
        self.assertIn('"bookmarks": Match(wm_class="aocepclkpgckjeikiphffdlileoaceec__bookmarks.html")', self.config)
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
