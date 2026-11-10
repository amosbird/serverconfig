#!/usr/bin/env python3

import os
import pathlib
import shutil
import subprocess
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts/startabush"


class StartAbushTest(unittest.TestCase):
    def setUp(self):
        self.temporary = ROOT / ".tmp" / f"startabush-{os.getpid()}-{id(self):x}"
        home = self.temporary / "home"
        (home / "scripts").mkdir(parents=True)
        (home / "git/abush/target/release").mkdir(parents=True)
        self.marker = self.temporary / "relay-args"
        self.abush_args = self.temporary / "abush-args"
        self.abush_environment = self.temporary / "abush-environment"
        self._script(home / "scripts/abush-browser-local.py", f'''case "${{1:-}}" in
--wait-ready)
    for _ in 1 2 3 4 5 6 7 8 9 10; do
        [[ -f {self.marker} ]] && exit 0
        sleep 0.01
    done
    exit 1
    ;;
esac
printf started > {self.marker}
''')
        self._script(home / "git/abush/target/release/abush",
                     f"printf '%s\\n' \"$@\" > {self.abush_args}\n"
                     f"printf '%s\\n' \"$ABUSH_LOG\" > {self.abush_environment}\n")
        self.bin = self.temporary / "bin"
        self.bin.mkdir()
        self._script(self.bin / "pass", "printf ''\n")
        self.home = home

    def tearDown(self):
        shutil.rmtree(self.temporary)

    @staticmethod
    def _script(path, body):
        path.write_text("#!/usr/bin/env bash\n" + body)
        path.chmod(0o755)

    def run_startabush(self, *args, check=True, environment=None):
        return subprocess.run(
            [str(SCRIPT), *args], check=check,
            env={**os.environ, "HOME": str(self.home),
                 "PATH": f"{self.bin}:{os.environ['PATH']}", **(environment or {})},
            stdout=subprocess.DEVNULL, stderr=subprocess.PIPE,
            timeout=5,
        )

    def test_local_is_detected_after_other_flags(self):
        self.run_startabush("--other", "--local")
        self.assertEqual(self.marker.read_text(), "started")
        self.assertEqual(self.abush_args.read_text().splitlines(), ["--other", "--local"])
        self.assertEqual(self.abush_environment.read_text().strip(), "/tmp/abush.log")

    def test_explicit_log_is_exported_before_publisher_readiness(self):
        log = self.temporary / "browser.log"
        self.run_startabush("--local", environment={"ABUSH_LOG": str(log)})
        self.assertEqual(self.abush_environment.read_text().strip(), str(log))

    def test_publisher_readiness_failure_blocks_abush(self):
        self._script(self.home / "scripts/abush-browser-local.py", '''case "${1:-}" in
--wait-ready) exit 1 ;;
esac
exit 1
''')
        result = self.run_startabush("--local", check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(self.abush_args.exists())
        self.assertIn(b"local browser transport failed", result.stderr)

    def test_local_option_value_is_not_mistaken_for_flag(self):
        self.run_startabush("--remote-web-port", "--local")
        self.assertFalse(self.marker.exists())

    def test_subcommand_arguments_are_not_tui_flags(self):
        self.run_startabush("ask", "--local")
        self.assertFalse(self.marker.exists())


if __name__ == "__main__":
    unittest.main()
