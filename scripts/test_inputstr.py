#!/usr/bin/env python3

import os
import subprocess
import tempfile
import time
import unittest
from pathlib import Path


SCRIPT = Path(__file__).with_name("inputstr")


class InputstrTest(unittest.TestCase):
    def test_forwards_unicode_without_blocking(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            output = directory / "input"
            copyq = directory / "copyq"
            copyq.write_text(
                "#!/usr/bin/env bash\n"
                'printf "%s" "${@: -1}" >"$INPUTSTR_TEST_OUTPUT"\n'
                "sleep 1\n"
            )
            copyq.chmod(0o755)
            value = "中文 🐦\nline\t\\"
            env = os.environ | {
                "PATH": f"{directory}:{os.environ['PATH']}",
                "INPUTSTR_TEST_OUTPUT": str(output),
                "XDG_RUNTIME_DIR": directory,
            }

            started = time.monotonic()
            subprocess.run([SCRIPT, value], env=env, check=True)
            elapsed = time.monotonic() - started

            self.assertLess(elapsed, 0.5)
            actual = None
            for _ in range(50):
                if output.exists():
                    actual = output.read_text()
                    if actual == value:
                        break
                time.sleep(0.02)
            self.assertEqual(actual, value)

    def test_qtile_defers_direct_input_until_hyper_release(self):
        config = SCRIPT.parent.parent / ".config" / "qtile" / "config.py"
        source = config.read_text()
        self.assertIn("def defer_inputstr", source)
        self.assertIn('Key([super_r], "0", defer_inputstr("0", "0.0.0.0"))', source)
        self.assertIn('Key([super_r], "1", defer_inputstr("1", "127.0.0.1"))', source)
        self.assertIn('Key([super_r], "4", defer_inputstr("4", "amosbird@gmail.com"))', source)
        self.assertIn('defer_inputstr("k", "$(pass show scripts/otp | bash)", shell=True)', source)
        self.assertIn("xcffib.xproto.KeyReleaseEvent", source)
        self.assertIn("xcffib.xproto.EventMask.KeyRelease", source)
        self.assertIn("QueryKeymap().reply().keys", source)
        self.assertIn("call_later(0.01, flush_pending_inputstr", source)
        self.assertIn("xcbq.keysyms[key]", source)
        self.assertIn('xcbq.keysyms["super_r"]', source)
        self.assertNotIn('xkeysyms.keysyms["Super_R"]', source)
        self.assertIn("pending_inputstr = (trigger_keycodes, command, shell)", source)
        self.assertIn("_, command, shell = pending_inputstr", source)
        self.assertIn("pending_inputstr = None", source)
        self.assertNotIn("for command, shell in pending", source)

    def test_copyq_worker_uses_existing_fill_text_transaction(self):
        source = SCRIPT.read_text()
        self.assertIn('copyq fillText -- "$@"', source)
        self.assertNotIn("queryKeyboardModifiers", source)
        self.assertNotIn("copy(oldClipboard)", source)


if __name__ == "__main__":
    unittest.main()
