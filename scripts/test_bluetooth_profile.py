#!/usr/bin/env python3

import pathlib
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).parents[1]
PROFILE = ROOT / "scripts/bluetooth-profile"
STABLE_AUDIO = ROOT / ".config/pipewire/pipewire.conf.d/51-freeclip-stable.conf"
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

    def test_profile_toggle_is_manual_and_transport_local(self):
        script = PROFILE.read_text()
        self.assertIn('pactl set-card-profile "$card" "$target"', script)
        self.assertIn("headset-head-unit", script)
        self.assertIn("a2dp-sink", script)
        self.assertIn('route_backend "$stable_input_backend" "$local_input"', script)
        self.assertIn('route_profile "$active"', script)
        self.assertIn("wait_backends", script)
        self.assertIn("wait_nodes_stable", script)
        self.assertIn("node_is_healthy", script)
        self.assertIn("failed; using local audio", script)
        self.assertIn('set_volume sink "$freeclip_output"', script)
        self.assertIn('set_volume source "$input"', script)
        self.assertNotIn("set-sink-input-volume", script)
        for command in (
            "move-sink-input",
            "move-source-output",
            "bluetoothctl",
            "rfkill",
            "systemctl",
            "parecord",
        ):
            self.assertNotIn(command, script)

    def test_stable_endpoints_hide_physical_profile_churn(self):
        config = STABLE_AUDIO.read_text()
        self.assertEqual(config.count("name = libpipewire-module-loopback"), 2)
        self.assertIn('node.name = "freeclip_stable_output"', config)
        self.assertIn('node.name = "freeclip_stable_input"', config)
        self.assertIn('node.name = "freeclip_stable_output.backend"', config)
        self.assertIn('node.name = "freeclip_stable_input.backend"', config)
        self.assertEqual(config.count("node.dont-fallback = true"), 2)
        self.assertEqual(config.count("node.linger = true"), 2)

    def test_wemeet_is_pinned_to_stable_endpoints(self):
        script = WEMEET.read_text()
        self.assertIn("PULSE_SINK=$stable_sink PULSE_SOURCE=$stable_source", script)
        self.assertIn('bluetooth-profile --route', script)
        self.assertIn('exec /usr/bin/wemeet "$@"', script)
        self.assertNotIn("LD_PRELOAD", script)

    def test_hfp_switches_to_a2dp(self):
        result, output = self._toggle("headset-head-unit", "a2dp-sink")
        self.assertEqual(result.returncode, 0)
        self.assertIn("set-card-profile bluez_card.C0_DA_5E_EC_FB_7F a2dp-sink", output)
        self.assertIn("pw-metadata -n default 202 target.object 603 Spa:Id", output)
        self.assertIn("pw-metadata -n default 201 target.object 604 Spa:Id", output)
        self.assertIn("pw-metadata -n default 201 target.object 601 Spa:Id", output)
        self.assertIn("A2DP", output)

    def test_a2dp_switches_to_best_hfp_profile(self):
        result, output = self._toggle("a2dp-sink", "headset-head-unit")
        self.assertEqual(result.returncode, 0)
        self.assertIn(
            "set-card-profile bluez_card.C0_DA_5E_EC_FB_7F headset-head-unit",
            output,
        )
        self.assertIn("pw-metadata -n default 202 target.object 602 Spa:Id", output)
        self.assertIn("pw-metadata -n default 201 target.object 601 Spa:Id", output)
        self.assertIn("HFP/mSBC", output)

    def test_missing_a2dp_profile_explains_that_reconnection_is_required(self):
        result, output = self._run_profile(
            "Name: bluez_card.C0_DA_5E_EC_FB_7F\n"
            "Active Profile: headset-head-unit\n",
            available_profiles=("off", "headset-head-unit"),
        )
        self.assertEqual(result.returncode, 0)
        self.assertNotIn("set-card-profile", output)
        self.assertIn("Reconnect FreeClip once", output)

    def test_profile_switch_failure_keeps_the_current_profile(self):
        result, output = self._run_profile(
            "Name: bluez_card.C0_DA_5E_EC_FB_7F\n"
            "Active Profile: headset-head-unit\n",
            fail_switch=True,
        )
        self.assertEqual(result.returncode, 0)
        self.assertIn("set-card-profile", output)
        self.assertIn("A2DP is unavailable", output)

    def test_disconnected_freeclip_does_not_touch_other_bluetooth_cards(self):
        result, output = self._run_profile(
            "Name: bluez_card.AA_BB\nActive Profile: a2dp-sink\n"
        )
        self.assertEqual(result.returncode, 0)
        self.assertNotIn("set-card-profile", output)
        self.assertIn("FreeClip 2 is not connected", output)

    @classmethod
    def _toggle(cls, active, target):
        cards = (
            "Name: bluez_card.C0_DA_5E_EC_FB_7F\n"
            f"Active Profile: {active}\n"
        )
        return cls._run_profile(cards, target)

    @staticmethod
    def _run_profile(cards, target=None, fail_switch=False, available_profiles=None):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            log = path / "log"
            state = path / "state"
            state.write_text(cards)
            profiles = available_profiles or ("off", "a2dp-sink", "headset-head-unit")
            profile_lines = "".join(f"    {profile}: Profile\n" for profile in profiles)
            switch = "exit 1" if fail_switch else (
                f'''printf 'Name: %s\\nActive Profile: %s\\n' "$2" "$3" >{state}'''
            )
            pactl = path / "pactl"
            pactl.write_text(
                f'''#!/usr/bin/env bash
printf 'pactl %s\\n' "$*" >>{log}
if [[ $* == "list cards" ]]; then
    cat {state}
    printf 'Profiles:\\n'
    printf '%s' '{profile_lines}'
elif [[ $* == "list sink-inputs" ]]; then
    printf 'Sink Input #701\\n'
    printf '\\tapplication.name = "Wemeet VoiceEngine"\\n'
elif [[ $1 == "set-card-profile" ]]; then
    {switch}
fi
'''
            )
            pactl.chmod(0o755)
            dunstify = path / "dunstify"
            dunstify.write_text(
                f'''#!/usr/bin/env bash
printf 'dunstify %s\\n' "$*" >>{log}
'''
            )
            dunstify.chmod(0o755)
            pw_dump = path / "pw-dump"
            pw_dump.write_text(
                """#!/usr/bin/env bash
cat <<'JSON'
[
  {"id":201,"info":{"props":{"node.name":"freeclip_stable_output.backend"}}},
  {"id":202,"info":{"props":{"node.name":"freeclip_stable_input.backend"}}},
  {"id":301,"info":{"props":{"node.name":"bluez_output.C0_DA_5E_EC_FB_7F.1","object.serial":"601"}}},
  {"id":302,"info":{"props":{"node.name":"bluez_input.C0:DA:5E:EC:FB:7F","object.serial":"602"}}},
  {"id":303,"info":{"props":{"node.name":"alsa_input.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Mic1__source","object.serial":"603"}}},
  {"id":304,"info":{"props":{"node.name":"alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Speaker__sink","object.serial":"604"}}}
]
JSON
"""
            )
            pw_dump.chmod(0o755)
            pw_metadata = path / "pw-metadata"
            pw_metadata.write_text(
                f'''#!/usr/bin/env bash
printf 'pw-metadata %s\n' "$*" >>{log}
'''
            )
            pw_metadata.chmod(0o755)
            pw_link = path / "pw-link"
            pw_link.write_text(
                """#!/usr/bin/env bash
cat <<'LINKS'
freeclip_stable_output.backend:output_FL
  |-> bluez_output.C0_DA_5E_EC_FB_7F.1:playback_FL
  |-> alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Speaker__sink:playback_FL
freeclip_stable_input.backend:input_MONO
  |<- bluez_input.C0:DA:5E:EC:FB:7F:capture_MONO
  |<- alsa_input.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Mic1__source:capture_FL
LINKS
"""
            )
            pw_link.chmod(0o755)
            result = subprocess.run(
                [PROFILE],
                env={"PATH": f"{path}:/usr/bin", "XDG_RUNTIME_DIR": directory},
                check=False,
                capture_output=True,
                text=True,
            )
            output = log.read_text() if log.exists() else ""
            if target is not None:
                assert f"Active Profile: {target}" in state.read_text()
            return result, output

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
