#!/usr/bin/env python3

import pathlib
import unittest

ROOT = pathlib.Path(__file__).parents[1]
SCRIPT = ROOT / "scripts/togglewemeet"
CONFIG = ROOT / ".config/qtile/config.py"


class ToggleWemeetTest(unittest.TestCase):
    def test_shows_group_before_starting_or_activating_wemeet(self):
        script = SCRIPT.read_text()
        show_group = script.index('client.group["2"].toscreen()')
        process_check = script.index('pgrep", "-x", "wemeetapp"')
        launch = script.index('subprocess.Popen(["wemeet"]')
        self.assertLess(show_group, process_check)
        self.assertLess(show_group, launch)

    def test_does_not_force_map_wemeet_internal_windows(self):
        script = SCRIPT.read_text()
        self.assertNotIn("xdotool", script)
        self.assertNotIn("focus_by_name", script)

    def test_wemeet_group_uses_max_layout_for_auxiliary_windows(self):
        self.assertIn('Group("2", layout="max")', CONFIG.read_text())

    def test_qtile_binding_still_uses_togglewemeet(self):
        self.assertIn(
            'Key([ctrl, alt], "2", lazy.spawn("togglewemeet"))', CONFIG.read_text()
        )


if __name__ == "__main__":
    unittest.main()
