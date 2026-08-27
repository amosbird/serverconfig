#!/usr/bin/env python3

import pathlib
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).parents[1]
MUTE = ROOT / "scripts/microphone-mute"
LED_SYNC = ROOT / "scripts/audio-mute-led"
LED_SERVICE = ROOT / "systemd/audio-mute-led.service"
CONFIG = ROOT / ".config/qtile/config.py"


class MicrophoneMuteTest(unittest.TestCase):
    def test_qtile_ctrl_f4_toggles_default_microphone(self):
        config = CONFIG.read_text()
        self.assertIn('Key([ctrl], "F4", lazy.spawn("microphone-mute"))', config)
        self.assertNotIn('Key([ctrl], "F4", lazy.spawn("bluetooth-profile"))', config)

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

    def test_led_sync_is_not_bluetooth_specific(self):
        script = LED_SYNC.read_text()
        self.assertIn("pactl get-sink-mute @DEFAULT_SINK@", script)
        self.assertIn("pactl get-source-mute @DEFAULT_SOURCE@", script)
        self.assertNotIn("bluez", script)
        self.assertNotIn("list cards", script)

    def test_restore_uses_current_led_service(self):
        restore = (ROOT / "restore.sh").read_text()
        self.assertIn("systemd/audio-mute-led.service", restore)
        self.assertTrue((ROOT / "scripts/audio-mute-state").exists())
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
