#!/usr/bin/env python3

import os
import pathlib
import subprocess
import tempfile
import unittest


ROOT = pathlib.Path(__file__).parents[1]
SCRIPT = ROOT / "scripts/killwork"


class KillworkTest(unittest.TestCase):
    def test_only_kills_remote_command_owned_by_sshd(self):
        with tempfile.TemporaryDirectory() as tmp:
            bin_dir = pathlib.Path(tmp)
            log = bin_dir / "kill.log"
            bash_env = bin_dir / "bash_env"
            bash_env.write_text(
                f'kill() {{ printf \'%s\\n\' "$*" >> "{log}"; }}\n'
            )
            commands = {
                "pkill": "exit 1\n",
                "pgrep": "printf '200\\n300\\n'\n",
                "ps": """
args=$*
while [ "$#" -gt 0 ]; do
    [ "$1" = "-p" ] && { pid=$2; break; }
    shift
done
case "$args" in
    *ppid=*)
        case $pid in 200) echo 150;; 150) echo 100;; 300) echo 250;; 250) echo 1;; esac
        ;;
    *comm=*)
        case $pid in 150) echo bash;; 100) echo sshd;; 250) echo startx;; 1) echo systemd;; esac
        ;;
esac
""",
            }
            for name, body in commands.items():
                path = bin_dir / name
                path.write_text("#!/bin/sh\n" + body)
                path.chmod(0o755)

            env = os.environ | {
                "BASH_ENV": str(bash_env),
                "PATH": f"{bin_dir}:/usr/bin:/bin",
            }
            subprocess.run([SCRIPT], check=True, env=env)
            self.assertEqual(log.read_text().splitlines(), ["150"])


if __name__ == "__main__":
    unittest.main()
