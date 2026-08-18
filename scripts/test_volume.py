#!/usr/bin/env python3

import pathlib
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).parents[1]
SCRIPT = ROOT / "scripts/volume"
CONFIG = ROOT / ".config/qtile/config.py"


class VolumeTest(unittest.TestCase):
    def test_qtile_uses_volume_wrapper(self):
        config = CONFIG.read_text()
        for command in ("volume mute", "volume down", "volume up"):
            self.assertIn(command, config)
        self.assertNotIn("wpctl set-volume", config)
        self.assertNotIn("wpctl set-mute", config)

    def test_up_changes_volume_and_shows_replacing_progress_notification(self):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            log = path / "log"
            self._write_mock(
                path / "wpctl",
                f'''#!/usr/bin/env bash
printf 'wpctl %s\\n' "$*" >>{log!s}
if [[ $1 == get-volume ]]; then printf 'Volume: 0.53\\n'; fi
if [[ $1 == inspect ]]; then printf '    api.bluez5.profile = "a2dp-sink"\\n    api.bluez5.codec = "aac"\\n'; fi
''',
            )
            self._write_mock(
                path / "dunstify",
                f'''#!/usr/bin/env bash
printf 'dunstify %s\\n' "$*" >>{log!s}
''',
            )
            self._write_mock(
                path / "pactl",
                f'''#!/usr/bin/env bash
printf 'pactl %s\\n' "$*" >>{log!s}
if [[ $* == 'list sink-inputs short' ]]; then printf '361\\t292\\t360\\tPipeWire\\n'; fi
if [[ $* == info ]]; then printf 'Default Sink: bluez_output.C0_DA_5E_EC_FB_7F.1\\n'; fi
''',
            )
            result = subprocess.run(
                [SCRIPT, "up"], env={"PATH": f"{path}:/usr/bin"}, check=False
            )
            output = log.read_text()

        self.assertEqual(result.returncode, 0)
        self.assertIn(
            "wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 3%+", output
        )
        self.assertIn(
            "wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 3%+", output
        )
        self.assertIn("pactl set-sink-input-volume 361 53%", output)
        self.assertIn("--stack-tag volume", output)
        self.assertIn("int:value:53", output)
        self.assertIn("A2DP/AAC", output)
        self.assertIn("Volume 53%", output)

    def test_profile_labels_cover_hfp_and_non_bluetooth(self):
        script = SCRIPT.read_text()
        self.assertIn("HFP/", script)
        self.assertIn("Local", script)

    def test_mute_notification_distinguishes_mute_and_unmute(self):
        script = SCRIPT.read_text()
        self.assertIn("set-sink-input-mute", script)
        self.assertIn("Muted", script)
        self.assertIn("Unmuted", script)
        self.assertIn("string:state:$muted", script)

    @staticmethod
    def _write_mock(path, content):
        path.write_text(content)
        path.chmod(0o755)


if __name__ == "__main__":
    unittest.main()
