#!/usr/bin/env python3

import pathlib
import unittest

ROOT = pathlib.Path(__file__).parents[1]
RESTORE = ROOT / "restore.sh"
WIREPLUMBER = ROOT / ".config/wireplumber/wireplumber.conf.d/51-bluetooth.conf"
BLUETOOTH = ROOT / "scripts/rofibluetooth-blocks"


class PipeWireMigrationTest(unittest.TestCase):
    def test_restore_installs_pipewire_stack_not_pulseaudio(self):
        restore = RESTORE.read_text()
        self.assertIn(
            "pipewire-audio pipewire-alsa pipewire-pulse wireplumber", restore
        )
        self.assertNotIn("pacman -S --needed --noconfirm pulseaudio-bluetooth", restore)
        self.assertIn("systemctl --user enable --now", restore)
        self.assertIn(
            "pipewire.service pipewire-pulse.service wireplumber.service", restore
        )

    def test_wireplumber_owns_bluetooth_profiles_without_autoswitch(self):
        config = WIREPLUMBER.read_text()
        self.assertIn("bluetooth.autoswitch-to-headset-profile = false", config)
        self.assertIn("device.restore-profile = false", config)
        self.assertIn("bluez5.roles", config)
        self.assertIn("hfp_ag", config)
        self.assertIn("bluez5.auto-connect = [ a2dp_sink hfp_hf hfp_ag ]", config)
        self.assertIn("a2dp_sink", config)
        self.assertIn("hfp_hf", config)
        self.assertIn('device.profile = "a2dp-sink"', config)
        self.assertIn('node.name = "~bluez_output.*"', config)
        self.assertIn("priority.session = 1200", config)

    def test_rofi_does_not_implement_audio_profile_or_stream_routing(self):
        script = BLUETOOTH.read_text()
        self.assertNotIn("route_audio", script)
        self.assertNotIn("pactl", script)
        self.assertNotIn("wpctl", script)
        self.assertNotIn("a2dp_sink", script)


if __name__ == "__main__":
    unittest.main()
