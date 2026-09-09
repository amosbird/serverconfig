#!/usr/bin/env python3

import pathlib
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).parents[1]
PROFILE = ROOT / "scripts/bluetooth-profile"
STABLE_AUDIO = ROOT / ".config/pipewire/pipewire.conf.d/51-freeclip-stable.conf"
SESSION = ROOT / ".config/wireplumber/scripts/freeclip-session.lua"
SESSION_COMPONENT = (
    ROOT / ".config/wireplumber/wireplumber.conf.d/52-freeclip-session.conf"
)
WEMEET = ROOT / "scripts/wemeet"
MUTE = ROOT / "scripts/microphone-mute"
LED_SYNC = ROOT / "scripts/audio-mute-led"
LED_SERVICE = ROOT / "systemd/audio-mute-led.service"
CONFIG = ROOT / ".config/qtile/config.py"


class AudioControlTest(unittest.TestCase):
    def test_qtile_ctrl_f4_toggles_default_microphone(self):
        config = CONFIG.read_text()
        self.assertIn('Key([ctrl], "F4", lazy.spawn("microphone-mute"))', config)
        self.assertIn(
            'Key([ctrl, shift], "F4", lazy.spawn("bluetooth-profile"))', config
        )
        self.assertNotIn('Key([ctrl], "F4", lazy.spawn("bluetooth-profile"))', config)

    def test_profile_cli_only_writes_session_intent(self):
        script = PROFILE.read_text()
        self.assertIn("pw-metadata -n default", script)
        self.assertIn("freeclip.session.desired-mode", script)
        self.assertIn("freeclip.session.request-id", script)
        self.assertIn('case ${1:-toggle}', script)
        for command in (
            "pactl set-card-profile",
            "pw-dump",
            "pw-link",
            "move-sink-input",
            "move-source-output",
            "bluetoothctl",
            "rfkill",
            "systemctl",
            "parecord",
        ):
            self.assertNotIn(command, script)

    def test_session_manager_owns_profile_and_backend_routing(self):
        script = SESSION.read_text()
        self.assertIn("bluez_card.C0_DA_5E_EC_FB_7F", script)
        self.assertIn("bluez_input.C0:DA:5E:EC:FB:7F", script)
        self.assertNotIn(
            'local FREECLIP_INPUT = "bluez_input.C0_DA_5E_EC_FB_7F.0"', script
        )
        self.assertIn("freeclip_stable_output.backend", script)
        self.assertIn("freeclip_stable_input.backend", script)
        self.assertIn('find_profile_node (FREECLIP_OUTPUT, profile)', script)
        self.assertIn('route_node (OUTPUT_BACKEND, output)', script)
        self.assertIn('metadata:set (backend["bound-id"], "target.object"', script)
        self.assertIn('device:set_param ("Profile", param)', script)
        self.assertIn("HFP_TRANSPORT_WAIT_STEPS = 20", script)
        self.assertIn("wait_hfp_output_running", script)
        self.assertIn('output["state"] == "running"', script)
        self.assertIn('backend["state"] ~= "running"', script)
        self.assertLess(
            script.index("wait_hfp_output_running (output, input"),
            script.index('route_input ("hfp", output, input)'),
        )
        self.assertIn('output["state"] == "error"', script)
        self.assertIn('new_state == "error"', script)
        self.assertIn("recovery_generation", script)
        self.assertIn("RECOVERY_COOLDOWN_MS = 10000", script)
        self.assertIn("LOCAL_FALLBACK", script)
        self.assertIn("Core.timeout_add", script)
        self.assertIn('find_node ("freeclip_stable_output"), 0.5', script)
        self.assertIn('set_node_volume (output, 1.0)', script)
        self.assertNotIn("set-sink-input-volume", script)
        for command in ("bluetoothctl", "rfkill", "systemctl", "parecord"):
            self.assertNotIn(command, script)

    def test_session_manager_is_loaded_after_metadata_and_bluez(self):
        config = SESSION_COMPONENT.read_text()
        self.assertIn("name = /home/amos/.config/wireplumber/scripts/freeclip-session.lua", config)
        self.assertIn("provides = custom.freeclip-session", config)
        self.assertIn("metadata.default", config)
        self.assertIn("monitor.bluez", config)
        self.assertIn("support.standard-event-source", config)

    def test_stable_endpoints_hide_physical_profile_churn(self):
        config = STABLE_AUDIO.read_text()
        self.assertEqual(config.count("name = libpipewire-module-loopback"), 2)
        self.assertIn('node.name = "freeclip_stable_output"', config)
        self.assertIn('node.name = "freeclip_stable_input"', config)
        self.assertIn('node.name = "freeclip_stable_output.backend"', config)
        self.assertIn('node.name = "freeclip_stable_input.backend"', config)
        self.assertEqual(config.count("node.dont-fallback = true"), 2)
        self.assertEqual(config.count("node.linger = true"), 2)

    def test_wemeet_is_pinned_to_stable_endpoints_without_routing(self):
        script = WEMEET.read_text()
        self.assertIn("PULSE_SINK=$stable_sink PULSE_SOURCE=$stable_source", script)
        self.assertIn('exec /usr/bin/wemeet "$@"', script)
        self.assertNotIn("bluetooth-profile --route", script)
        self.assertNotIn("LD_PRELOAD", script)

    def test_cli_commands_write_metadata(self):
        for command, expected in (("a2dp", '"a2dp"'), ("hfp", '"hfp"')):
            result, output = self._run_profile(command)
            self.assertEqual(result.returncode, 0)
            self.assertIn(
                f"freeclip.session.desired-mode {expected} Spa:String:JSON", output
            )
            self.assertIn("freeclip.session.request-id", output)

    def test_toggle_uses_manager_status(self):
        result, output = self._run_profile("toggle", mode="hfp")
        self.assertEqual(result.returncode, 0)
        self.assertIn('freeclip.session.desired-mode "a2dp"', output)

    def test_retry_sends_unique_request_without_changing_mode(self):
        result, output = self._run_profile("retry", mode="hfp")
        self.assertEqual(result.returncode, 0)
        self.assertNotIn("freeclip.session.desired-mode", output)
        self.assertIn('freeclip.session.command "retry"', output)
        self.assertIn("freeclip.session.request-id", output)

    @staticmethod
    def _run_profile(command, mode="a2dp"):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            log = path / "log"
            pw_metadata = path / "pw-metadata"
            pw_metadata.write_text(
                f'''#!/usr/bin/env bash
printf 'pw-metadata %s\n' "$*" >>{log}
if [[ $* == *freeclip.session.desired-mode ]]; then
    printf 'Found "default" metadata 1\n'
    printf '%s\n' "update: id:0 key:'freeclip.session.desired-mode' value:'\\\"{mode}\\\"' type:'Spa:String:JSON'"
fi
'''
            )
            pw_metadata.chmod(0o755)
            result = subprocess.run(
                [PROFILE, command],
                env={"PATH": f"{path}:/usr/bin"},
                check=False,
                capture_output=True,
                text=True,
            )
            return result, log.read_text() if log.exists() else ""

    def test_toggle_uses_default_source_and_syncs_led(self):
        script = MUTE.read_text()
        self.assertIn("target=@DEFAULT_AUDIO_SOURCE@", script)
        self.assertIn('wpctl set-mute "$target" toggle', script)
        self.assertIn('wpctl get-volume "$target"', script)
        self.assertNotIn('audio-mute-intent "$intent"', script)
        self.assertIn("audio-mute-led --once", script)
        self.assertNotIn("audio-mute-state microphone", script)
        self.assertNotIn("bluez", script)

    def test_ctrl_f4_is_system_level_not_freeclip_specific(self):
        script = MUTE.read_text()
        self.assertIn("@DEFAULT_AUDIO_SOURCE@", script)
        self.assertNotIn("bluez", script)

    def test_ctrl_f4_matches_ctrl_f1_native_wpctl_pattern(self):
        output = (ROOT / "scripts/volume").read_text()
        microphone = MUTE.read_text()
        self.assertIn('wpctl set-mute "$target" toggle', output)
        self.assertIn('target=@DEFAULT_AUDIO_SOURCE@', microphone)
        self.assertIn('wpctl set-mute "$target" toggle', microphone)
        self.assertNotIn("audio-mute-intent", microphone)
        self.assertNotIn("rollback", microphone)

    def test_led_follows_default_output_and_microphone_mute(self):
        result, speaker, microphone = self._sync_led("yes", "no")
        self.assertEqual(result.returncode, 0)
        self.assertEqual(speaker, "1")
        self.assertEqual(microphone, "0")

        result, speaker, microphone = self._sync_led("no", "yes")
        self.assertEqual(result.returncode, 0)
        self.assertEqual(speaker, "0")
        self.assertEqual(microphone, "1")

    def test_missing_default_microphone_turns_micmute_led_on(self):
        result, speaker, microphone = self._sync_led("no", "")
        self.assertEqual(result.returncode, 0)
        self.assertEqual(speaker, "0")
        self.assertEqual(microphone, "1")

    def test_led_sync_is_not_bluetooth_specific(self):
        script = LED_SYNC.read_text()
        self.assertIn("pactl get-sink-mute @DEFAULT_SINK@", script)
        self.assertIn("pactl get-source-mute @DEFAULT_SOURCE@", script)
        self.assertNotIn("bluez", script)
        self.assertNotIn("list cards", script)

    def test_restore_uses_current_led_service(self):
        restore = (ROOT / "restore.sh").read_text()
        self.assertIn("systemd/audio-mute-led.service", restore)
        self.assertFalse((ROOT / "scripts/audio-mute-state").exists())
        self.assertFalse((ROOT / "scripts/audio-mute-intent").exists())
        self.assertIn("enable --now audio-mute-led.service", restore)
        self.assertNotIn("bluetooth-profile-led.service", restore)
        self.assertNotIn("microphone-mute-led.service", restore)

    def test_led_service_tracks_pipewire_events(self):
        service = LED_SERVICE.read_text()
        sync = LED_SYNC.read_text()
        self.assertIn("ExecStart=/home/amos/scripts/audio-mute-led", service)
        self.assertIn("Restart=always", service)
        self.assertIn("pactl subscribe", sync)

    def test_default_led_globs_cover_speaker_and_microphone(self):
        script = LED_SYNC.read_text()
        self.assertIn("/sys/class/leds/*:mute/brightness", script)
        self.assertIn("/sys/class/leds/*:micmute/brightness", script)

    @staticmethod
    def _sync_led(sink_muted, source_muted):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            speaker = path / "speaker"
            microphone = path / "microphone"
            speaker.write_text("9")
            microphone.write_text("9")
            pactl = path / "pactl"
            pactl.write_text(
                f'''#!/usr/bin/env bash
if [[ $* == "get-sink-mute @DEFAULT_SINK@" ]]; then
    printf 'Mute: {sink_muted}\n'
elif [[ $* == "get-source-mute @DEFAULT_SOURCE@" ]]; then
    printf 'Mute: {source_muted}\n'
fi
'''
            )
            pactl.chmod(0o755)
            result = subprocess.run(
                [LED_SYNC, "--once"],
                env={
                    "PATH": f"{path}:/usr/bin",
                    "MUTE_LED_GLOB": str(speaker),
                    "MICMUTE_LED_GLOB": str(microphone),
                },
                check=False,
                capture_output=True,
                text=True,
            )
            return result, speaker.read_text(), microphone.read_text()


if __name__ == "__main__":
    unittest.main()
