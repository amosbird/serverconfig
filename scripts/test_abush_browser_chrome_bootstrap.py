#!/usr/bin/env python3

import os
import pathlib
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts/abush-browser-chrome-bootstrap"


class ChromeBootstrapTest(unittest.TestCase):
    def run_bootstrap(self, create_profile):
        temporary_root = ROOT.parent.parent / ".tmp"
        temporary_root.mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=temporary_root) as directory:
            home = pathlib.Path(directory)
            if create_profile:
                (home / ".config/google-chrome-main").mkdir(parents=True)
            fake_chrome = home / "fake-chrome"
            output = home / "argv"
            fake_chrome.write_text("#!/usr/bin/env bash\nprintf '%s\\n' \"$@\" > \"$OUTPUT\"\n")
            fake_chrome.chmod(0o755)
            result = subprocess.run(
                [SCRIPT],
                env={**os.environ, "HOME": str(home), "ABUSH_CHROME_BIN": str(fake_chrome),
                     "OUTPUT": str(output)},
                capture_output=True,
                text=True,
                check=False,
            )
            return result, output.read_text().splitlines() if output.exists() else [], home

    def test_existing_main_profile_starts_without_url_focus_remote_or_cdp(self):
        result, argv, _ = self.run_bootstrap(True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(any(argument.startswith("--user-data-dir=") for argument in argv))
        self.assertIn("--silent-debugger-extension-api", argv)
        self.assertIn("--no-startup-window", argv)
        self.assertNotIn("--restore-last-session", argv)
        self.assertFalse(any("remote-debugging-port" in argument for argument in argv))
        self.assertFalse(any("SingletonCookie" in argument for argument in argv))
        self.assertFalse(any(argument.startswith("http") for argument in argv))

    def test_missing_profile_fails_without_creating_it(self):
        result, argv, home = self.run_bootstrap(False)
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(argv)
        self.assertFalse((home / ".config/google-chrome-main").exists())


if __name__ == "__main__":
    unittest.main()
