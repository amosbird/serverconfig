#!/usr/bin/env python3

import fcntl
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

    def test_up_changes_only_the_default_sink(self):
        result, output = self._run("up", "Volume: 0.53")

        self.assertEqual(result.returncode, 0)
        self.assertIn("wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 3%+", output)
        self.assertIn("wpctl get-volume @DEFAULT_AUDIO_SINK@", output)
        # A volume step cannot change the mute state, and the extra process
        # cost it added made held keys drop presses; audio-mute-led.service
        # keeps the LED in sync instead.
        self.assertNotIn("audio-mute-led", output)
        self.assertNotIn("pactl ", output)
        self.assertNotIn("wpctl inspect", output)
        self.assertIn("--stack-tag volume", output)
        self.assertIn("int:value:53", output)
        self.assertIn("Volume up", output)
        self.assertIn("Volume 53%", output)

    def test_up_unmutes_a_muted_sink(self):
        result, output = self._run("up", "Volume: 0.53 [MUTED]")

        self.assertEqual(result.returncode, 0)
        self.assertIn("wpctl set-mute @DEFAULT_AUDIO_SINK@ 0", output)
        self.assertNotIn("toggle", output)
        # The mute state did change here, so the LED needs the nudge.
        self.assertIn("audio-mute-led --once", output)
        self.assertIn("Volume up", output)
        self.assertIn("string:state:false", output)

    def test_mute_changes_only_the_default_sink(self):
        result, output = self._run("mute", "Volume: 0.53 [MUTED]")

        self.assertEqual(result.returncode, 0)
        self.assertIn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle", output)
        self.assertIn("audio-mute-led --once", output)
        self.assertNotIn("audio-mute-state", output)
        self.assertNotIn("pactl ", output)
        self.assertIn("Muted", output)
        self.assertIn("string:state:true", output)

    def test_invalid_action_fails_without_side_effects(self):
        result, output = self._run("invalid", "Volume: 0.53")

        self.assertEqual(result.returncode, 2)
        self.assertIn("usage: volume {up|down|mute}", result.stderr)
        self.assertEqual(output, "")

    def test_repeat_only_records_a_request_while_a_burst_runs(self):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            log = self._write_mocks(path, "Volume: 0.53")
            lock = path / "volume" / "lock"
            lock.parent.mkdir()
            lock.touch()

            with open(lock, "w") as handle:
                fcntl.flock(handle, fcntl.LOCK_EX)
                result = self._spawn(path, "up")

            self.assertEqual(result.returncode, 0)
            # The running burst owns the volume; this press only queued itself.
            self.assertFalse(log.exists())
            self.assertEqual((path / "volume" / "request").read_text(), "up")

    @classmethod
    def _run(cls, action, status):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory)
            log = cls._write_mocks(path, status)
            result = cls._spawn(path, action)
            output = log.read_text() if log.exists() else ""
            return result, output

    @staticmethod
    def _spawn(path, action):
        return subprocess.run(
            [SCRIPT, action],
            env={"PATH": f"{path}:/usr/bin", "XDG_RUNTIME_DIR": str(path)},
            check=False,
            capture_output=True,
            text=True,
        )

    @classmethod
    def _write_mocks(cls, path, status):
        log = path / "log"
        cls._write_mock(
            path / "wpctl",
            f'''#!/usr/bin/env bash
printf 'wpctl %s\\n' "$*" >>{log!s}
if [[ $1 == get-volume ]]; then printf '%s\\n' {status!r}; fi
''',
        )
        cls._write_mock(
            path / "audio-mute-led",
            f'''#!/usr/bin/env bash
printf 'audio-mute-led %s\\n' "$*" >>{log!s}
''',
        )
        cls._write_mock(
            path / "dunstify",
            f'''#!/usr/bin/env bash
printf 'dunstify %s\\n' "$*" >>{log!s}
''',
        )
        cls._write_mock(
            path / "pactl",
            f'''#!/usr/bin/env bash
printf 'pactl %s\\n' "$*" >>{log!s}
''',
        )
        return log

    @staticmethod
    def _write_mock(path, content):
        path.write_text(content)
        path.chmod(0o755)


if __name__ == "__main__":
    unittest.main()
