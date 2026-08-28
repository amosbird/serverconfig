#!/usr/bin/env python3

import json
import os
import pathlib
import re
import subprocess
import unittest

ROOT = pathlib.Path(__file__).parents[1]
RESTORE = ROOT / "restore.sh"
WIREPLUMBER = ROOT / ".config/wireplumber/wireplumber.conf.d/51-bluetooth.conf"
PIPEWIRE_WEMEET = ROOT / ".config/pipewire/pipewire-pulse.conf.d/51-wemeet.conf"
QTILE = ROOT / ".config/qtile/config.py"
MANUAL_RELEASE = ROOT / "scripts/release-wemeet-audio"
OBSOLETE = (
    ROOT / "scripts/bluetooth-audio-default",
    ROOT / "systemd/bluetooth-audio-default.service",
    ROOT / "scripts/bluetooth-sco-watchdog",
    ROOT / "systemd/bluetooth-sco-watchdog.service",
)


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

    def test_unsafe_policy_daemons_are_removed_and_migrated(self):
        for path in OBSOLETE:
            self.assertFalse(path.exists(), path)

        restore = RESTORE.read_text()
        self.assertIn("systemctl --user disable --now", restore)
        self.assertIn("bluetooth-audio-default.service", restore)
        self.assertIn("bluetooth-sco-watchdog.service", restore)
        self.assertIn('rm -f "$HOME/.config/systemd/user/bluetooth-audio-default.service"', restore)
        self.assertIn('"$HOME/.config/systemd/user/bluetooth-sco-watchdog.service"', restore)
        self.assertIn("systemctl --user reset-failed", restore)
        self.assertNotIn("enable --now bluetooth-audio-default.service", restore)
        self.assertNotIn("enable --now bluetooth-sco-watchdog.service", restore)

    def test_no_audio_policy_component_uses_destructive_automation(self):
        source = "\n".join(
            path.read_text(errors="replace")
            for path in (
                *ROOT.glob("scripts/bluetooth-audio-*"),
                *ROOT.glob("scripts/bluetooth-sco-*"),
                MANUAL_RELEASE,
                *ROOT.glob("systemd/bluetooth-audio-*"),
                *ROOT.glob("systemd/bluetooth-sco-*"),
                *ROOT.glob(".config/wireplumber/**/*"),
                *ROOT.glob(".local/share/wireplumber/**/*"),
            )
            if path.is_file() and not path.name.startswith("test_")
        )
        forbidden = (
            "move-sink-input",
            "move-source-output",
            "parecord",
            "bluetoothctl disconnect",
            "bluetoothctl connect",
            "rfkill unblock",
            "set-card-profile",
            "pw-cli destroy",
        )
        for command in forbidden:
            self.assertNotIn(command, source, command)

    def test_wireplumber_is_the_single_routing_owner(self):
        config = WIREPLUMBER.read_text()
        self.assertIn("linking.follow-default-target = true", config)
        self.assertIn("node.restore-default-targets = false", config)
        self.assertNotIn("node.stream.restore-target = false", config)
        self.assertNotIn("node.passive", config)
        self.assertNotIn("pulse.idle.timeout", config)

    def test_freeclip_is_hfp_msbc_only(self):
        config = WIREPLUMBER.read_text()
        self.assertIn("bluetooth.autoswitch-to-headset-profile = false", config)
        self.assertIn("bluetooth.use-persistent-storage = false", config)
        self.assertIn("device.restore-profile = false", config)
        self.assertIn("bluez5.roles = [ hfp_hf hfp_ag ]", config)
        self.assertIn("bluez5.enable-msbc = true", config)
        self.assertIn('bluez5.hfphsp-backend = "native"', config)
        self.assertNotRegex(config, r"a2dp[_-]")

    def test_freeclip_output_does_not_outrank_local_speaker(self):
        config = WIREPLUMBER.read_text()
        output_rule = re.search(
            r'node\.name = "~bluez_output[^\n]+"(.*?)(?:\n  \}|\Z)',
            config,
            re.DOTALL,
        )
        self.assertIsNotNone(output_rule)
        self.assertNotIn("priority.session", output_rule.group(1))
        self.assertIn("session.suspend-timeout-seconds = 3", output_rule.group(1))

    def test_freeclip_specific_rules_only_cover_special_behavior(self):
        config = WIREPLUMBER.read_text()
        self.assertIn('device.name = "bluez_card.C0_DA_5E_EC_FB_7F"', config)
        self.assertIn('node.name = "~bluez_output.C0_DA_5E_EC_FB_7F.*"', config)
        self.assertIn('node.name = "bluez_input.C0:DA:5E:EC:FB:7F"', config)
        self.assertNotIn('device.name = "~bluez_card.*"', config)
        self.assertNotIn('node.name = "~bluez_output.*"', config)
        self.assertNotIn('node.name = "~bluez_input.*"', config)
        self.assertNotIn('node.name = "~alsa_input.*"', config)
        self.assertNotIn("priority.session", config)
        source_rule = re.search(
            r'node\.name = "bluez_input[^\n]+".*?session\.suspend-timeout-seconds = 3',
            config,
            re.DOTALL,
        )
        self.assertIsNotNone(source_rule)
        self.assertIn("session.suspend-timeout-seconds = 3", config)

    def test_wemeet_gets_native_pulse_s16_compatibility_quirk(self):
        config = PIPEWIRE_WEMEET.read_text()
        self.assertIn('application.process.binary = "wemeetapp"', config)
        self.assertIn("quirks = [ force-s16-info ]", config)
        self.assertNotIn("block-record-stream", config)
        self.assertNotIn("remove-capture-dont-move", config)

        if not os.environ.get("DBUS_SESSION_BUS_ADDRESS"):
            self.skipTest("PipeWire session is unavailable")
        normal = self._bluez_source_format({})
        wemeet = self._bluez_source_format(
            {
                "PULSE_PROP_application.process.binary": "wemeetapp",
                "PULSE_PROP_application.name": "Wemeet Quirk Test",
            }
        )
        if normal is None:
            self.skipTest("FreeClip source is unavailable")
        self.assertIn("float32le", normal)
        self.assertIn("s16le", wemeet)

    def test_wemeet_rule_only_suppresses_stored_target(self):
        config = WIREPLUMBER.read_text()
        self.assertIn('application.process.binary = "wemeetapp"', config)
        self.assertIn("state.restore-target = false", config)
        self.assertIn("cannot distinguish a quiet/listen-only meeting", config)
        self.assertIn("Never automate", config)
        self.assertNotIn("target.object =", config)

    def test_ctrl_f4_uses_wireplumber_native_mute(self):
        script = (ROOT / "scripts/microphone-mute").read_text()
        config = WIREPLUMBER.read_text()
        self.assertIn("target=@DEFAULT_AUDIO_SOURCE@", script)
        self.assertIn('wpctl set-mute "$target" toggle', script)
        self.assertNotIn("audio-mute-intent", script)
        self.assertFalse((ROOT / "scripts/audio-mute-intent").exists())
        self.assertFalse(
            (ROOT / ".local/share/wireplumber/scripts/90-system-microphone-mute.lua").exists()
        )
        self.assertNotIn("system-microphone-mute", config)

    def test_restore_clears_runtime_setting_overrides(self):
        restore = RESTORE.read_text()
        self.assertIn("wpctl settings -d bluetooth.autoswitch-to-headset-profile", restore)
        self.assertIn("wpctl settings -d device.restore-profile", restore)
        self.assertIn("wpctl settings -d node.stream.restore-target", restore)

    def test_restore_installs_native_bluetooth_menu(self):
        restore = RESTORE.read_text()
        self.assertIn("bzmenu-bin", restore)

    def test_qtile_launches_native_bluetooth_menu(self):
        self.assertIn(
            'lazy.spawn("bzmenu --launcher rofi --interactive")', QTILE.read_text()
        )


    @staticmethod
    def _bluez_source_format(extra_env):
        result = subprocess.run(
            ["pactl", "--format=json", "list", "sources"],
            capture_output=True,
            check=False,
            env={**os.environ, **extra_env},
            text=True,
        )
        if result.returncode:
            return None
        for source in json.loads(result.stdout):
            if source["name"].startswith("bluez_input."):
                return source["sample_specification"]
        return None


if __name__ == "__main__":
    unittest.main()
