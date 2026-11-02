#!/usr/bin/env python3

import pathlib
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]
GREMOTE = ROOT / "scripts" / "gremote2.sh"
TSTART = ROOT / "scripts" / "tstart.sh"


class RemoteEnvironmentTest(unittest.TestCase):
    def test_gremote_does_not_probe_or_start_abush_before_the_main_session(self):
        gremote = GREMOTE.read_text()

        self.assertNotIn('target/release/abush', gremote)
        self.assertNotIn('abush_web_socket_status', gremote)
        self.assertNotIn('GREMOTE_REQUIRE_PREVIEW', gremote)
        self.assertNotIn('continuing without the remote Abush TUI', gremote)

    def test_main_ssh_owns_the_forward_and_tstart_owns_abush_startup(self):
        gremote = GREMOTE.read_text()
        tstart = TSTART.read_text()

        self.assertIn(
            '-L "127.0.0.1:${ABUSH_WEB_FORWARD_PORT}:${REMOTE_ABUSH_WEB_SOCKET}"',
            gremote,
        )
        self.assertIn('--abush-web-forward-port "$ABUSH_WEB_FORWARD_PORT"', gremote)
        self.assertIn('source "$HOME/.config/environment.sh"', tstart)
        self.assertIn('source "$HOME/.config/path.sh" remote', tstart)
        self.assertIn('exec startabush --remote-web-port "$2"', tstart)


if __name__ == "__main__":
    unittest.main()
