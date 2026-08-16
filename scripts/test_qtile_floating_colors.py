#!/usr/bin/env python3

import pathlib
import unittest

CONFIG = pathlib.Path(__file__).resolve().parents[1] / ".config/qtile/config.py"


class FloatingBorderColorsTest(unittest.TestCase):
    def test_designed_floating_windows_use_rainbow_colors(self):
        config = CONFIG.read_text()
        expected = {
            "ioa": "#F94144",
            "tdesktop": "#70A288",
            "webchat": "#2A9D8F",
            "chatgpt": "#84A98C",
            "bookmarks": "#C44536",
            "stardict": "#E83E8C",
            "stalonetray": "#F2CC8F",
        }
        for name, color in expected.items():
            self.assertIn(f'(scratchpad_matches["{name}"], "{color}")', config)

        self.assertIn('(Match(wm_class="Chromium", role="pop-up"), "#6A994E")', config)
        self.assertIn('(Match(wm_class="copyq"), "#E9C46A")', config)
        self.assertNotIn('(Match(wm_class="kitty", title="dtpick"), "#C34A36")', config)

    def test_borderless_utility_windows_have_zero_width(self):
        config = CONFIG.read_text()
        self.assertIn("border_width=ConditionalBorderWidth(", config)
        self.assertIn('(Match(wm_class="kitty", title="float"), 0)', config)
        self.assertIn('(Match(wm_class="kitty", title="dtpick"), 0)', config)

    def test_reload_keeps_conditional_border_width_numeric(self):
        config = CONFIG.read_text()
        self.assertIn("class ConditionalBorderWidth(int):", config)
        self.assertIn('hasattr(borderwidth, "get_border_for_window")', config)
        self.assertIn("if not isinstance(old, int):", config)
        self.assertNotIn("isinstance(borderwidth, ConditionalBorderWidth)", config)

    def test_undesigned_floating_windows_keep_original_yellow(self):
        config = CONFIG.read_text()
        self.assertIn('default="#FFB300"', config)
        self.assertIn("floating_border_colors = ConditionalBorderColor(", config)
        self.assertIn("border_focus=floating_border_colors", config)
        self.assertIn("border_normal=floating_border_colors", config)


if __name__ == "__main__":
    unittest.main()
