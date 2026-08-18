#!/usr/bin/env python3

import pathlib
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).parents[1]
SCRIPT = ROOT / "scripts/bluetooth-profile"
CONFIG = ROOT / ".config/qtile/config.py"
WIREPLUMBER = ROOT / ".config/wireplumber/wireplumber.conf.d/51-bluetooth.conf"


class BluetoothProfileTest(unittest.TestCase):
    def test_qtile_binds_ctrl_f4_to_profile_toggle(self):
        config = CONFIG.read_text()
        self.assertIn('Key([ctrl], "F4", lazy.spawn("bluetooth-profile"))', config)

    def test_autoswitch_is_disabled(self):
        self.assertIn(
            "bluetooth.autoswitch-to-headset-profile = false",
            WIREPLUMBER.read_text(),
        )

    def test_inactive_bluetooth_is_a_silent_noop(self):
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
        self.assertNotIn("notify", output)
        self.assertEqual(brightness, "0")

    def test_a2dp_switches_to_msbc_hfp_and_lights_led(self):
        result, output, brightness = self._toggle("a2dp-sink", "headset-head-unit")
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stderr, "")
        self.assertIn(
            "set-card-profile bluez_card.AA_BB headset-head-unit", output
        )
        self.assertIn("HFP/mSBC", output)
        self.assertEqual(brightness, "1")

    def test_hfp_switches_to_a2dp_and_turns_led_off(self):
        result, output, brightness = self._toggle("headset-head-unit", "a2dp-sink")
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stderr, "")
        self.assertIn("set-card-profile bluez_card.AA_BB a2dp-sink", output)
        self.assertIn("A2DP", output)
        self.assertEqual(brightness, "0")

    def _toggle(self, active, target):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            log = path / "log"
            state = path / "state"
            led = path / "brightness"
            state.write_text(active)
            led.write_text("9")
            self._mock(
                path / "pactl",
                f'''#!/usr/bin/env bash
printf '%s\\n' "$*" >>{log}
case "$*" in
"list cards short") printf '2\\tbluez_card.AA_BB\\tmodule-bluez5-device.c\\n' ;;
"list cards") printf 'Name: bluez_card.AA_BB\\nActive Profile: %s\\n' "$(cat {state})" ;;
"set-card-profile bluez_card.AA_BB {target}") printf '%s' '{target}' >{state} ;;
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
