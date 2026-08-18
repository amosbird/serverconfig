#!/usr/bin/env python3

import pathlib
import unittest

CONFIG = pathlib.Path(__file__).parents[1] / ".config/qtile/config.py"


class QtileVolumeTest(unittest.TestCase):
    def test_volume_keys_use_pipewire_volume_wrapper(self):
        config = CONFIG.read_text()
        self.assertNotIn('lazy.spawn("pamixer --', config)
        for command in ("volume mute", "volume down", "volume up"):
            self.assertIn(command, config)


if __name__ == "__main__":
    unittest.main()
