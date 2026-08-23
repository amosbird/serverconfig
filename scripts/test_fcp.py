#!/usr/bin/env python3

import os
import pathlib
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
FCP = ROOT / "scripts/fcp"


class FileCopyTest(unittest.TestCase):
    def run_fcp(self, *paths):
        with tempfile.TemporaryDirectory() as directory:
            directory = pathlib.Path(directory)
            log = directory / "copyq.log"
            copyq = directory / "copyq"
            copyq.write_text(
                "#!/usr/bin/env bash\nprintf '%s\\0' \"$@\" >\"$COPYQ_LOG\"\n"
            )
            copyq.chmod(0o755)
            env = {**os.environ, "PATH": f"{directory}:{os.environ['PATH']}", "COPYQ_LOG": str(log)}
            result = subprocess.run([str(FCP), *map(str, paths)], env=env, capture_output=True)
            args = log.read_bytes().split(b"\0")[:-1] if log.exists() else []
            return result, [arg.decode() for arg in args]

    def test_copies_canonical_file_paths_in_one_uri_list(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = pathlib.Path(directory)
            first = directory / "file one"
            second = directory / "-file-two"
            first.touch()
            second.touch()
            result, args = self.run_fcp(first, second)
        self.assertEqual(result.returncode, 0, result.stderr.decode())
        self.assertEqual(args, ["copyUriList", f"{first.resolve()}\n{second.resolve()}"])

    def test_does_not_replace_clipboard_when_no_valid_file_was_given(self):
        result, args = self.run_fcp("/does/not/exist")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(args, [])


if __name__ == "__main__":
    unittest.main()
