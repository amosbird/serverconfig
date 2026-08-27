#!/usr/bin/env python3

import pathlib
import re
import unittest

ROOT = pathlib.Path(__file__).parents[1]
RESTORE = ROOT / "restore.sh"
WIREPLUMBER = ROOT / ".config/wireplumber/wireplumber.conf.d/51-bluetooth.conf"
MUTE_HOOK = ROOT / ".local/share/wireplumber/scripts/90-system-microphone-mute.lua"
MUTE_INTENT = ROOT / "scripts/audio-mute-intent"
MANUAL_RELEASE = ROOT / "scripts/release-wemeet-audio"
QTILE = ROOT / ".config/qtile/config.py"
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

    def test_wemeet_rule_only_suppresses_stored_target(self):
        config = WIREPLUMBER.read_text()
        self.assertIn('application.process.binary = "wemeetapp"', config)
        self.assertIn("state.restore-target = false", config)
        self.assertIn("cannot distinguish a quiet/listen-only meeting", config)
        self.assertIn("Never automate", config)
        self.assertNotIn("target.object =", config)

    def test_mute_intent_hook_applies_to_every_physical_microphone(self):
        config = WIREPLUMBER.read_text()
        hook = MUTE_HOOK.read_text()
        self.assertIn("90-system-microphone-mute.lua", config)
        self.assertIn("requires = [ api.mixer ]", config)
        self.assertIn('StateMetadata ("audio-mute-intent")', hook)
        self.assertIn('state:get ("microphone")', hook)
        self.assertIn('intent ~= "0" and intent ~= "1"', hook)
        self.assertIn('"set-volume", id, { mute = intent == "1" }', hook)
        self.assertIn('"media.class", "=", "Audio/Source"', hook)
        self.assertIn('"device.id", "+"', hook)
        self.assertNotIn("bluez_input", hook)

    def test_ctrl_f4_and_mute_helper_are_the_only_durable_mute_writers(self):
        intent = MUTE_INTENT.read_text()
        self.assertIn("pw-metadata -n audio-mute-intent 0 microphone \"$state\"", intent)
        self.assertNotIn("Spa:String", intent)
        self.assertIn('Key([ctrl], "F4", lazy.spawn("microphone-mute"))', QTILE.read_text())
        callers = []
        for path in ROOT.glob("scripts/*"):
            if not path.is_file() or path == MUTE_INTENT or path.name.startswith("test_"):
                continue
            if re.search(r"(^|[ /])audio-mute-intent([ \"']|$)", path.read_text(errors="replace")):
                callers.append(path.name)
        self.assertEqual(callers, ["microphone-mute"])

    def test_manual_release_requires_confirmation_and_never_forces_cleanup(self):
        script = MANUAL_RELEASE.read_text()
        self.assertIn("[[ ! -t 0 || ! -t 1 ]]", script)
        self.assertIn("Restart Wemeet and release its audio?", script)
        self.assertIn('kill -TERM "${pids[@]}"', script)
        self.assertIn("no forced cleanup was attempted", script)
        self.assertNotIn("SIGKILL", script)
        self.assertNotIn("kill-sink-input", script)

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


if __name__ == "__main__":
    unittest.main()
