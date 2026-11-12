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

    def test_ctrl_alt_9_scratchpad_scan_skips_internal_qtile_windows(self):
        config = CONFIG.read_text()
        self.assertIn('Key([ctrl, alt], "9", toggle_scratchpad("webchat"))', config)
        self.assertIn("from libqtile.backend.base import Window as QtileWindow", config)
        self.assertIn("if isinstance(window, QtileWindow) and match.compare(window)", config)
        self.assertIn(
            "def register_scratchpad_window(window, hide=True):\n"
            "    if not isinstance(window, QtileWindow):\n"
            "        return",
            config,
        )


if __name__ == "__main__":
    unittest.main()
