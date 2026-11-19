#!/usr/bin/env python3

import unittest

from qtile_test_support import OfflineQtile


class QtileVolumeTest(unittest.TestCase):
    def setUp(self):
        self.fixture = self.enterContext(OfflineQtile())
        self.config = self.fixture.load()

    def test_volume_keys_use_pipewire_volume_wrapper(self):
        expected = {"XF86AudioMute": "volume mute", "XF86AudioLowerVolume": "volume down",
                    "XF86AudioRaiseVolume": "volume up"}
        for key, command in expected.items():
            bindings = [binding for binding in self.config.keys if binding.key == key]
            self.assertEqual(len(bindings), 1)
            self.assertEqual(bindings[0].modifiers, [])
            call = bindings[0].commands[0]
            self.assertEqual((call.name, call.args), ("spawn", (command,)))
        self.assertFalse(any(call.name == "spawn" and call.args
                             and str(call.args[0]).startswith("pamixer --")
                             for binding in self.config.keys for call in binding.commands))

    def test_sound_control_uses_pavucontrol(self):
        bindings = [binding for binding in self.config.keys
                    if binding.key == "v" and binding.modifiers == ["mod3"]]
        self.assertEqual(len(bindings), 1)
        call = bindings[0].commands[0]
        self.assertEqual((call.name, call.args), ("spawn", ("pavucontrol",)))
        self.assertFalse(any(call.name == "spawn" and call.args == ("rofisound",)
                             for binding in self.config.keys for call in binding.commands))


if __name__ == "__main__":
    unittest.main()
