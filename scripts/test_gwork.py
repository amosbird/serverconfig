#!/usr/bin/env python3

import os
import pathlib
import subprocess
import tempfile
import unittest


ROOT = pathlib.Path(__file__).parents[1]
SCRIPT = ROOT / "scripts/gwork.sh"


class GworkTest(unittest.TestCase):
    def run_gwork(self, local_hostname):
        with tempfile.TemporaryDirectory() as tmp:
            bin_dir = pathlib.Path(tmp)
            log = bin_dir / "commands.log"
            commands = {
                "hostname": f"printf '%s\\n' {local_hostname}\n",
                "ssh": f"printf 'ssh %s\\n' \"$*\" >> {log}\n",
                "uuidgen": "printf 'test-uuid\\n'\n",
                "kitty": f"printf 'kitty %s\\n' \"$*\" >> {log}\n",
                "qtile": ":\n",
            }
            for name, body in commands.items():
                path = bin_dir / name
                path.write_text("#!/bin/sh\n" + body)
                path.chmod(0o755)

            env = os.environ | {"PATH": f"{bin_dir}:/usr/bin:/bin"}
            subprocess.run([SCRIPT], check=True, env=env)
            return log.read_text().splitlines()

    def test_abx_connects_to_qtw(self):
        commands = self.run_gwork("abx1gen3-1")
        self.assertTrue(commands[0].startswith("ssh 100.91.94.87 "))
        self.assertIn(" ssh -t 100.91.94.87 ", commands[1])

    def test_qtw_connects_to_abx(self):
        commands = self.run_gwork("qtw-1")
        self.assertTrue(commands[0].startswith("ssh 100.88.203.53 "))
        self.assertIn(" ssh -t 100.88.203.53 ", commands[1])


if __name__ == "__main__":
    unittest.main()
