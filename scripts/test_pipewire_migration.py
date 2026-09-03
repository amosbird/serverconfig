#!/usr/bin/env python3

import pathlib
import re
import unittest

ROOT = pathlib.Path(__file__).parents[1]
RESTORE = ROOT / "restore.sh"
WIREPLUMBER = ROOT / ".config/wireplumber/wireplumber.conf.d/51-bluetooth.conf"
WEMEET_LOOPBACK = ROOT / ".config/pipewire/pipewire.conf.d/51-wemeet-stable.conf"
WEMEET = ROOT / "scripts/wemeet"
QTILE = ROOT / ".config/qtile/config.py"
TLP = ROOT / "tlp/tlp.conf"
AUDIO_POLICY = (
    WIREPLUMBER,
    ROOT / "scripts/bluetooth-profile",
    ROOT / "scripts/volume",
    ROOT / "scripts/microphone-mute",
    ROOT / "scripts/audio-mute-led",
)
OBSOLETE = (
    ROOT / "scripts/bluetooth-audio-default",
    ROOT / "systemd/bluetooth-audio-default.service",
    ROOT / "scripts/bluetooth-sco-watchdog",
    ROOT / "systemd/bluetooth-sco-watchdog.service",
    ROOT / "scripts/audio-mute-state",
    ROOT / "scripts/rofisound",
    ROOT / "scripts/restartbluetooth",
    ROOT / "scripts/release-wemeet-audio",
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

    def test_policy_helpers_are_removed(self):
        for path in OBSOLETE:
            self.assertFalse(path.exists(), path)

        restore = RESTORE.read_text()
        self.assertIn(
            'rm -f "$HOME/.config/systemd/user/bluetooth-audio-default.service"',
            restore,
        )
        self.assertIn('"$HOME/.config/systemd/user/bluetooth-sco-watchdog.service"', restore)
        self.assertNotIn("enable --now bluetooth-audio-default.service", restore)
        self.assertNotIn("enable --now bluetooth-sco-watchdog.service", restore)

    def test_audio_policy_does_not_probe_or_repair_transports(self):
        source = "\n".join(path.read_text(errors="replace") for path in AUDIO_POLICY)
        forbidden = (
            "move-sink-input",
            "move-source-output",
            "parecord",
            "bluetoothctl disconnect",
            "bluetoothctl connect",
            "rfkill block",
            "rfkill unblock",
            "systemctl restart bluetooth",
            "pw-cli destroy",
        )
        for command in forbidden:
            self.assertNotIn(command, source, command)
        self.assertEqual(source.count("pactl set-card-profile"), 1)

    def test_tlp_does_not_disable_bluetooth(self):
        config = TLP.read_text()
        self.assertIn('DEVICES_TO_DISABLE_ON_BAT="nfc wwan"', config)
        self.assertNotRegex(config, r'DEVICES_TO_.*="[^"]*bluetooth')
        self.assertIn(
            'sudo install -Dm644 "$DIR"/tlp/tlp.conf /etc/tlp.conf',
            RESTORE.read_text(),
        )

    def test_wireplumber_keeps_only_the_required_setting_override(self):
        config = WIREPLUMBER.read_text()
        settings = re.search(r"wireplumber\.settings = \{(.*?)\}", config, re.DOTALL)
        self.assertIsNotNone(settings)
        self.assertEqual(
            settings.group(1).strip(),
            "bluetooth.autoswitch-to-headset-profile = false",
        )
        for setting in (
            "bluetooth.use-persistent-storage",
            "device.restore-profile",
            "linking.follow-default-target",
            "node.restore-default-targets",
            "node.stream.restore-target",
        ):
            self.assertNotIn(setting, config)

    def test_freeclip_supports_manual_a2dp_and_hfp_msbc(self):
        config = WIREPLUMBER.read_text()
        self.assertIn("bluez5.roles = [ a2dp_source hfp_hf hfp_ag ]", config)
        self.assertIn("bluez5.enable-msbc = true", config)
        self.assertNotIn("bluez5.hfphsp-backend", config)
        self.assertNotRegex(config, r"\b(?:a2dp_sink|bap[_-]|hsp[_-])")

    def test_freeclip_only_customization_is_idle_suspend(self):
        config = WIREPLUMBER.read_text()
        self.assertIn('node.name = "~bluez_output.C0_DA_5E_EC_FB_7F.*"', config)
        self.assertIn('node.name = "bluez_input.C0:DA:5E:EC:FB:7F"', config)
        self.assertEqual(config.count("update-props"), 1)
        self.assertEqual(config.count("session.suspend-timeout-seconds = 3"), 1)
        self.assertNotIn("bluez5.auto-connect", config)
        self.assertNotIn("priority.", config)
        self.assertNotIn("device.name", config)

    def test_no_application_specific_audio_policy(self):
        config = WIREPLUMBER.read_text()
        self.assertNotIn("stream.rules", config)
        self.assertNotIn("wemeet", config.lower())
        self.assertNotIn("state.restore-target", config)
        self.assertFalse((ROOT / "scripts/release-wemeet-audio").exists())

    def test_wemeet_uses_stable_pipewire_endpoints(self):
        config = WEMEET_LOOPBACK.read_text()
        self.assertEqual(config.count("name = libpipewire-module-loopback"), 2)
        for endpoint, media_class in (
            ("wemeet_output", "Audio/Sink"),
            ("wemeet_input", "Audio/Source"),
        ):
            self.assertIn(f'node.name = "{endpoint}"', config)
            self.assertIn(f"media.class = {media_class}", config)
        self.assertIn('node.name = "wemeet_output.backend"', config)
        self.assertIn('node.name = "wemeet_input.backend"', config)
        self.assertEqual(config.count("node.virtual = false"), 2)
        self.assertEqual(config.count("node.dont-fallback = true"), 2)
        self.assertEqual(config.count("node.linger = true"), 2)
        self.assertEqual(config.count("state.restore-props = false"), 4)

    def test_manual_profile_switch_routes_only_loopback_backends(self):
        script = (ROOT / "scripts/bluetooth-profile").read_text()
        self.assertIn('route_backend wemeet_output.backend', script)
        self.assertIn('route_backend wemeet_input.backend', script)
        self.assertIn("bluez_output.C0_DA_5E_EC_FB_7F.1", script)
        self.assertIn("bluez_input.C0:DA:5E:EC:FB:7F", script)
        self.assertIn("HiFi__Mic1__source", script)
        self.assertNotIn("pgrep", script)
        forbidden = (
            "move-sink-input",
            "move-source-output",
            "bluetoothctl",
            "rfkill",
            "systemctl restart",
            "parecord",
            "pw-cli destroy",
        )
        for command in forbidden:
            self.assertNotIn(command, script)

    def test_wemeet_bypasses_only_the_aur_pulse_hooks(self):
        script = WEMEET.read_text()
        pulse = "/usr/lib/libpulse.so.0"
        wrapper = "/usr/lib/wemeet/libwemeetwrap.so"
        self.assertIn(pulse, script)
        self.assertIn(wrapper, script)
        self.assertLess(script.index(pulse), script.index(wrapper))
        self.assertIn('exec /usr/bin/wemeet "$@"', script)
        self.assertNotIn("PULSE_SINK", script)
        self.assertNotIn("PULSE_SOURCE", script)

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

    def test_restore_clears_old_runtime_setting_overrides(self):
        restore = RESTORE.read_text()
        for setting in (
            "bluetooth.autoswitch-to-headset-profile",
            "bluetooth.use-persistent-storage",
            "device.restore-profile",
            "linking.follow-default-target",
            "node.stream.restore-target",
            "node.restore-default-targets",
        ):
            self.assertIn(f"wpctl settings -d {setting}", restore)

    def test_restore_installs_native_audio_tools(self):
        restore = RESTORE.read_text()
        self.assertIn("bzmenu-bin", restore)
        self.assertIn("pavucontrol", restore)

    def test_qtile_launches_native_bluetooth_menu(self):
        self.assertIn(
            'lazy.spawn("bzmenu --launcher rofi --interactive")', QTILE.read_text()
        )


if __name__ == "__main__":
    unittest.main()
