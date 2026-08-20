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

    def test_led_sync_retries_failed_autoswitch_once_per_capture_session(self):
        sync = LED_SYNC.read_text()
        self.assertIn('event == *"on source-output"*', sync)
        self.assertIn("recovery_attempted=1", sync)
        self.assertIn("recovery_attempted=0", sync)
        self.assertIn("bluetooth-profile", sync)

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

    def test_led_sync_marks_microphone_unavailable_in_a2dp(self):
        result, brightness = self._sync_led(
            "Name: bluez_card.AA_BB\nActive Profile: a2dp-sink\n"
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(brightness, "1")

    def test_qtile_binds_ctrl_f4_to_profile_toggle(self):
        config = CONFIG.read_text()
        self.assertIn('Key([ctrl], "F4", lazy.spawn("bluetooth-profile"))', config)

    def test_autoswitch_is_enabled(self):
        self.assertIn(
            "bluetooth.autoswitch-to-headset-profile = true",
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
        self.assertIn("Bluetooth profile unavailable", output)
        self.assertEqual(brightness, "0")

    def test_profile_toggle_does_not_write_led_state(self):
        self.assertNotIn("MICMUTE_LED_GLOB", SCRIPT.read_text())

    def test_default_led_glob_covers_current_keyboard_and_platform_leds(self):
        self.assertIn("/sys/class/leds/*micmute*/brightness", LED_SYNC.read_text())

    def test_prefers_the_active_bluetooth_card(self):
        result, output, _ = self._toggle(
            "a2dp-sink", "headset-head-unit", two_cards=True
        )
        self.assertEqual(result.returncode, 0)
        self.assertIn(
            "set-card-profile bluez_card.CONNECTED headset-head-unit", output
        )
        self.assertNotIn("set-card-profile bluez_card.OFF", output)

    def test_failed_profile_switch_reports_error(self):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            log = path / "log"
            led = path / "brightness"
            led.write_text("9")
            self._mock(
                path / "pactl",
                f'''#!/usr/bin/env bash
printf '%s\n' "$*" >>{log}
case "$*" in
"list cards") printf 'Name: bluez_card.AA_BB\nActive Profile: a2dp-sink\n' ;;
"set-card-profile "*) exit 1 ;;
esac
''',
            )
            self._mock(
                path / "dunstify",
                f"#!/usr/bin/env bash\nprintf 'notify %s\\n' \"$*\" >>{log}\n",
            )
            result = self._run(path, led)
            output = log.read_text()
        self.assertEqual(result.returncode, 0)
        self.assertIn("Bluetooth profile switch failed", output)

    def test_a2dp_switches_to_msbc_hfp(self):
        result, output, _ = self._toggle("a2dp-sink", "headset-head-unit")
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stderr, "")
        self.assertIn(
            "set-card-profile bluez_card.AA_BB headset-head-unit", output
        )
        self.assertIn("HFP/mSBC", output)

    def test_hfp_switches_to_a2dp(self):
        result, output, _ = self._toggle("headset-head-unit", "a2dp-sink")
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stderr, "")
        self.assertIn("set-card-profile bluez_card.AA_BB a2dp-sink", output)
        self.assertIn("A2DP", output)

    def _toggle(self, active, target, two_cards=False):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            log = path / "log"
            state = path / "state"
            led = path / "brightness"
            state.write_text(active)
            led.write_text("9")
            cards_short = (
                "1\\tbluez_card.OFF\\tmodule-bluez5-device.c\\n"
                "2\\tbluez_card.CONNECTED\\tmodule-bluez5-device.c\\n"
                if two_cards
                else "2\\tbluez_card.AA_BB\\tmodule-bluez5-device.c\\n"
            )
            card = "bluez_card.CONNECTED" if two_cards else "bluez_card.AA_BB"
            self._mock(
                path / "pactl",
                f'''#!/usr/bin/env bash
printf '%s\\n' "$*" >>{log}
case "$*" in
"list cards short") printf '{cards_short}' ;;
"list cards") printf 'Name: bluez_card.OFF\\nActive Profile: off\\nName: {card}\\nActive Profile: %s\\n' "$(cat {state})" ;;
"set-card-profile {card} {target}") printf '%s' '{target}' >{state} ;;
esac
''',
            )
            self._mock(
                path / "dunstify",
                f"#!/usr/bin/env bash\nprintf 'notify %s\\n' \"$*\" >>{log}\n",
            )
            result = self._run(path, led)
            return result, log.read_text(), led.read_text()

    @staticmethod
    def _sync_led(cards):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            led = path / "brightness"
            led.write_text("9")
            pactl = path / "pactl"
            pactl.write_text(
                f'''#!/usr/bin/env bash
[[ $* == "list cards" ]] && printf '%s' '{cards}'
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
