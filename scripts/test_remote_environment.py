#!/usr/bin/env python3

import pathlib
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]
GREMOTE = ROOT / "scripts" / "gremote2.sh"
TSTART = ROOT / "scripts" / "tstart.sh"


class RemoteEnvironmentTest(unittest.TestCase):
    def test_preview_boot_uses_the_same_remote_environment_as_tstart(self):
        gremote = GREMOTE.read_text()
        environment = 'source "$HOME/.config/environment.sh"'
        path = 'source "$HOME/.config/path.sh" remote'

        self.assertIn(environment, gremote)
        self.assertIn(path, gremote)
        self.assertLess(gremote.index(environment), gremote.index(path))
        self.assertLess(
            gremote.index(path),
            gremote.index('"$HOME/git/abush/target/release/abush"'),
        )

    def test_tstart_still_owns_the_canonical_remote_path_profile(self):
        tstart = TSTART.read_text()
        self.assertIn('source "$HOME/.config/environment.sh"', tstart)
        self.assertIn('source "$HOME/.config/path.sh" remote', tstart)


if __name__ == "__main__":
    unittest.main()
