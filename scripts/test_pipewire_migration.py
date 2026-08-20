#!/usr/bin/env python3

import pathlib
import unittest

ROOT = pathlib.Path(__file__).parents[1]
RESTORE = ROOT / "restore.sh"
WIREPLUMBER = ROOT / ".config/wireplumber/wireplumber.conf.d/51-bluetooth.conf"
QTILE = ROOT / ".config/qtile/config.py"


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

    def test_wireplumber_is_the_only_automatic_bluetooth_profile_owner(self):
        config = WIREPLUMBER.read_text()
        self.assertIn("bluetooth.autoswitch-to-headset-profile = true", config)
        self.assertIn("bluetooth.use-persistent-storage = true", config)
        self.assertIn('bluetooth.profile-preference = "quality"', config)
        self.assertIn("device.restore-profile = false", config)
        self.assertIn("node.restore-default-targets = true", config)
        self.assertIn("bluez5.roles", config)
        self.assertIn("bluez5.auto-connect = [ a2dp_sink hfp_hf hfp_ag ]", config)
        self.assertIn("a2dp_sink", config)
        self.assertIn("hfp_hf", config)
        self.assertIn('device.profile = "a2dp-sink"', config)
        self.assertIn('node.name = "~bluez_output.*"', config)
        self.assertIn("priority.session = 1200", config)

    def test_restore_clears_runtime_setting_overrides(self):
        restore = RESTORE.read_text()
        self.assertIn("wpctl settings -d bluetooth.autoswitch-to-headset-profile", restore)
        self.assertIn("wpctl settings -d device.restore-profile", restore)

    def test_restore_installs_native_bluetooth_menu(self):
        restore = RESTORE.read_text()
        self.assertIn("bzmenu-bin", restore)

    def test_qtile_launches_native_bluetooth_menu(self):
        self.assertIn(
            'lazy.spawn("bzmenu --launcher rofi --interactive")', QTILE.read_text()
        )


if __name__ == "__main__":
    unittest.main()
