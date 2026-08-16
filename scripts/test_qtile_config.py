#!/usr/bin/env python3

import pathlib
import unittest

CONFIG = pathlib.Path(__file__).resolve().parents[1] / ".config/qtile/config.py"


class QtileConfigTest(unittest.TestCase):
    def test_chatgpt_shortcut_uses_recoverable_scratchpad(self):
        config = CONFIG.read_text()
        self.assertIn('Key([ctrl, alt], "8", toggle_scratchpad("chatgpt"))', config)
        self.assertIn('"chatgpt": Match(wm_class="chatgpt")', config)
        self.assertIn("def recover_scratchpad_dropdowns():", config)
        self.assertIn("DropDownToggler(window, scratchpad.name, config)", config)
        self.assertIn("dropdown.window.focus(warp=True)", config)


if __name__ == "__main__":
    unittest.main()
