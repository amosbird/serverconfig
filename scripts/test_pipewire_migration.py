#!/usr/bin/env python3

import pathlib
import unittest

ROOT = pathlib.Path(__file__).parents[1]
RESTORE = ROOT / "restore.sh"
WIREPLUMBER = ROOT / ".config/wireplumber/wireplumber.conf.d/51-bluetooth.conf"
BLUETOOTH_AUDIO = ROOT / "scripts/bluetooth-audio-default"
BLUETOOTH_AUDIO_SERVICE = ROOT / "systemd/bluetooth-audio-default.service"
SCO_WATCHDOG = ROOT / "scripts/bluetooth-sco-watchdog"
SCO_WATCHDOG_SERVICE = ROOT / "systemd/bluetooth-sco-watchdog.service"
QTILE = ROOT / ".config/qtile/config.py"


class PipeWireMigrationTest(unittest.TestCase):
    def test_restore_enables_pipewire_stack_not_pulseaudio(self):
        restore = RESTORE.read_text()
        self.assertNotRegex(
            restore,
            r"(?m)^\s+pipewire-audio pipewire-alsa pipewire-pulse wireplumber$",
        )
        self.assertNotIn("pacman -S --needed --noconfirm pulseaudio-bluetooth", restore)
        self.assertIn("systemctl --user enable --now", restore)
        self.assertIn(
            "pipewire.service pipewire-pulse.service wireplumber.service", restore
        )

    def test_wireplumber_prefers_bluetooth_microphones(self):
        config = WIREPLUMBER.read_text()
        self.assertIn('node.name = "~alsa_input.*"', config)
        self.assertIn("priority.session = 2000", config)

    def test_wireplumber_moves_streams_when_default_changes(self):
        config = WIREPLUMBER.read_text()
        self.assertIn("linking.follow-default-target = true", config)
        self.assertIn("node.stream.restore-target = false", config)

    def test_bluetooth_default_service_routes_existing_app_streams(self):
        script = BLUETOOTH_AUDIO.read_text()
        service = BLUETOOTH_AUDIO_SERVICE.read_text()
        restore = RESTORE.read_text()
        self.assertIn('self.ops.call("set-default-sink"', script)
        self.assertIn('self.ops.call("set-default-source"', script)
        self.assertIn('self.ops.call("set-sink-mute", name, "0")', script)
        self.assertIn('self.ops.call("set-source-mute", name, "0")', script)
        self.assertIn('self.ops.call("move-sink-input"', script)
        self.assertIn('self.ops.call("move-source-output"', script)
        self.assertIn('["pactl", "subscribe"]', script)
        self.assertIn("ExecStart=/home/amos/scripts/bluetooth-audio-default", service)
        self.assertIn("enable --now bluetooth-audio-default.service", restore)
        self.assertIn("wpctl settings -d node.stream.restore-target", restore)

    def test_wireplumber_keeps_bluetooth_in_hfp_msbc(self):
        config = WIREPLUMBER.read_text()
        self.assertIn("bluetooth.autoswitch-to-headset-profile = false", config)
        self.assertIn("bluetooth.use-persistent-storage = false", config)
        self.assertIn("device.restore-profile = false", config)
        self.assertIn("node.restore-default-targets = false", config)
        self.assertIn("bluez5.roles = [ hfp_hf hfp_ag ]", config)
        self.assertNotIn("bluez5.codecs", config)
        self.assertIn("bluez5.enable-msbc = true", config)
        self.assertIn("bluez5.auto-connect = [ hfp_hf hfp_ag ]", config)
        self.assertNotIn("a2dp_sink", config)
        self.assertNotIn("a2dp-sink", config)
        self.assertIn('node.name = "~bluez_output.*"', config)
        self.assertIn('node.name = "~bluez_input.*"', config)
        self.assertIn("priority.session = 2500", config)

    def test_restore_clears_runtime_setting_overrides(self):
        restore = RESTORE.read_text()
        self.assertIn("wpctl settings -d bluetooth.autoswitch-to-headset-profile", restore)
        self.assertIn("wpctl settings -d device.restore-profile", restore)

    def test_sco_watchdog_is_installed_as_a_user_service(self):
        script = SCO_WATCHDOG.read_text()
        service = SCO_WATCHDOG_SERVICE.read_text()
        restore = RESTORE.read_text()
        self.assertNotIn("CONFERENCE_NAMES", script)
        self.assertIn("def has_active_capture(outputs, source_index):", script)
        self.assertIn("ExecStart=/home/amos/scripts/bluetooth-sco-watchdog", service)
        self.assertIn("enable --now bluetooth-sco-watchdog.service", restore)

    def test_restore_installs_native_bluetooth_menu(self):
        restore = RESTORE.read_text()
        self.assertIn("bzmenu-bin", restore)

    def test_qtile_launches_native_bluetooth_menu(self):
        self.assertIn(
            'lazy.spawn("bzmenu --launcher rofi --interactive")', QTILE.read_text()
        )


if __name__ == "__main__":
    unittest.main()
