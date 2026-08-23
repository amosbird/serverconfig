#!/usr/bin/env python3

import pathlib
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).parents[1]
SCRIPT = ROOT / "scripts/bluetooth-profile"
LED_SYNC = ROOT / "scripts/bluetooth-profile-led"
LED_SERVICE = ROOT / "systemd/bluetooth-profile-led.service"
CONFIG = ROOT / ".config/qtile/config.py"
WIREPLUMBER = ROOT / ".config/wireplumber/wireplumber.conf.d/51-bluetooth.conf"


class BluetoothProfileTest(unittest.TestCase):
    def test_restore_enables_led_sync_service(self):
        restore = (ROOT / "restore.sh").read_text()
        self.assertIn("systemd/bluetooth-profile-led.service", restore)
        self.assertIn("enable --now bluetooth-profile-led.service", restore)

    def test_led_sync_service_tracks_pipewire_events(self):
        service = LED_SERVICE.read_text()
        sync = LED_SYNC.read_text()
        self.assertIn("ExecStart=/home/amos/scripts/bluetooth-profile-led", service)
        self.assertIn("Restart=always", service)
        self.assertIn("pactl subscribe", sync)

    def test_led_sync_marks_microphone_unavailable_without_bluetooth(self):
        result, brightness = self._sync_led("Name: alsa_card.pci\nActive Profile: HiFi\n")
        self.assertEqual(result.returncode, 0)
        self.assertEqual(brightness, "1")

    def test_led_sync_marks_microphone_available_in_hfp(self):
        result, brightness = self._sync_led(
            "Name: bluez_card.AA_BB\nActive Profile: headset-head-unit\n"
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(brightness, "0")

    def test_led_sync_lights_when_hfp_microphone_is_muted(self):
        cards = "Name: bluez_card.AA_BB\nActive Profile: headset-head-unit\n"
        result, brightness = self._sync_led(cards, source_muted="yes")
        self.assertEqual(result.returncode, 0)
        self.assertEqual(brightness, "1")

    def test_led_sync_marks_microphone_unavailable_in_a2dp(self):
        result, brightness = self._sync_led(
            "Name: bluez_card.AA_BB\nActive Profile: a2dp-sink\n"
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(brightness, "1")

    def test_qtile_ctrl_f4_toggles_bluetooth_microphone_mute(self):
        config = CONFIG.read_text()
        script = SCRIPT.read_text()
        self.assertIn('Key([ctrl], "F4", lazy.spawn("bluetooth-profile"))', config)
        self.assertIn('pactl set-source-mute "$source" toggle', script)
        self.assertIn("bluetooth-profile-led --once", script)
        self.assertIn("Microphone muted", script)
        self.assertIn("Microphone active", script)
        self.assertNotIn("set-card-profile", script)

    def test_autoswitch_is_disabled(self):
        self.assertIn(
            "bluetooth.autoswitch-to-headset-profile = false",
            WIREPLUMBER.read_text(),
        )

    def test_inactive_bluetooth_reports_unavailable(self):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            log = path / "log"
            led = path / "brightness"
            led.write_text("0")
            self._mock(
                path / "pactl",
                f'''#!/usr/bin/env bash
printf '%s\n' "$*" >>{log}
if [[ $* == "list cards short" ]]; then
    printf '1\\talsa_card.pci\\talsa\\n'
fi
''',
            )
            self._mock(
                path / "dunstify",
                f"#!/usr/bin/env bash\nprintf 'notify %s\\n' \"$*\" >>{log}\n",
            )
            result = self._run(path, led)
            output = log.read_text()
            brightness = led.read_text()
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertEqual(result.stderr, "")
        self.assertIn("Bluetooth microphone unavailable", output)
        self.assertEqual(brightness, "0")

    def test_profile_toggle_does_not_write_led_state(self):
        self.assertNotIn("MICMUTE_LED_GLOB", SCRIPT.read_text())

    def test_default_led_glob_covers_current_keyboard_and_platform_leds(self):
        self.assertIn("/sys/class/leds/*micmute*/brightness", LED_SYNC.read_text())

    @staticmethod
    def _sync_led(cards, source_muted="no"):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            led = path / "brightness"
            led.write_text("9")
            pactl = path / "pactl"
            pactl.write_text(
                f'''#!/usr/bin/env bash
if [[ $* == "list cards" ]]; then
    printf '%s' '{cards}'
elif [[ $* == "list sources" ]]; then
    printf 'Name: bluez_input.AA_BB\nMute: {source_muted}\n'
fi
'''
            )
            pactl.chmod(0o755)
            result = subprocess.run(
                [LED_SYNC, "--once"],
                env={"PATH": f"{path}:/usr/bin", "MICMUTE_LED_GLOB": str(led)},
                check=False,
                capture_output=True,
                text=True,
            )
            return result, led.read_text()

    @staticmethod
    def _run(path, led):
        return subprocess.run(
            [SCRIPT],
            env={
                "PATH": f"{path}:/usr/bin",
                "MICMUTE_LED_GLOB": str(led),
            },
            check=False,
            capture_output=True,
            text=True,
        )

    @staticmethod
    def _mock(path, content):
        path.write_text(content)
        path.chmod(0o755)


if __name__ == "__main__":
    unittest.main()
