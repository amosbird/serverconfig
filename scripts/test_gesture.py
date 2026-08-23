#!/usr/bin/env python3
import pathlib
import unittest


GESTURE = pathlib.Path(__file__).with_name("gesture")


class GestureTest(unittest.TestCase):
    def test_chrome_scroll_gestures_use_extension_shortcuts(self):
        script = GESTURE.read_text()
        up = script.split("swipe3up()", 1)[1].split("swipe3down()", 1)[0]
        down = script.split("swipe3down()", 1)[1].split("swipe4left()", 1)[0]
        chrome_up = up.split("Google-chrome|Chromium)", 1)[1].split(";;", 1)[0]
        chrome_down = down.split("Google-chrome|Chromium)", 1)[1].split(";;", 1)[0]
        self.assertIn("xdotool key --window $id alt+shift+Home", chrome_up)
        self.assertIn("xdotool key --window $id alt+shift+End", chrome_down)
        self.assertNotIn("xdotool key --window $id Home", chrome_up)
        self.assertNotIn("xdotool key --window $id End", chrome_down)


if __name__ == "__main__":
    unittest.main()
