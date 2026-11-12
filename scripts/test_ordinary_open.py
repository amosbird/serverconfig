#!/usr/bin/env python3

import importlib.util
import json
import os
import pathlib
import queue
import socket
import subprocess
import sys
import threading
import time
import unittest
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
HOST_PATH = ROOT / "chromium/external-links/host.py"
spec = importlib.util.spec_from_file_location("ordinary_open_host", HOST_PATH)
host = importlib.util.module_from_spec(spec)
spec.loader.exec_module(host)


class OrdinaryOpenSocketTest(unittest.TestCase):
    def setUp(self):
        self.runtime = ROOT / ".tmp" / f"ordinary-open-{os.getpid()}-{id(self):x}"
        self.runtime.mkdir(parents=True, mode=0o700)
        self.path = self.runtime / "ordinary-open.sock"
        self.outbound = queue.Queue()
        self.server = host.OrdinaryOpenServer(
            self.outbound, path=self.path, timeout=1,
        )
        self.server.start()

    def tearDown(self):
        self.server.close()
        import shutil
        shutil.rmtree(self.runtime)

    def request(self, request_id="request-one"):
        return {
            "version": 1,
            "request_id": request_id,
            "operation": "ordinary_open",
            "url": "https://example.test/path",
            "focus": True,
        }

    def connect(self, request):
        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        client.settimeout(2)
        client.connect(str(self.path))
        client.sendall(host.encode_ordinary_open_message(request))
        return client

    def test_exact_request_correlation_and_permissions(self):
        client = self.connect(self.request())
        event = self.outbound.get(timeout=1)
        self.assertEqual(event, {
            "version": 1,
            "event": "ordinary_open",
            "service": "ordinary_open",
            "payload": self.request(),
        })
        completion = host.ordinary_open_response("request-one", "applied")
        self.server.complete(completion)
        self.assertEqual(host.read_ordinary_open_message(client), completion)
        client.close()
        self.assertEqual(self.path.stat().st_mode & 0o777, 0o600)
        self.assertEqual(self.path.parent.stat().st_mode & 0o777, 0o700)

    def test_duplicate_request_reuses_one_event_and_terminal_completion(self):
        first = self.connect(self.request())
        second = self.connect(self.request())
        self.assertEqual(self.outbound.get(timeout=1)["payload"], self.request())
        with self.assertRaises(queue.Empty):
            self.outbound.get(timeout=0.05)
        completion = host.ordinary_open_response("request-one", "unknown")
        self.server.complete(completion)
        self.assertEqual(host.read_ordinary_open_message(first), completion)
        self.assertEqual(host.read_ordinary_open_message(second), completion)
        first.close()
        second.close()

    def test_peer_uid_is_checked_before_request_delivery(self):
        with mock.patch.object(host.os, "getuid", return_value=os.getuid() + 1):
            other = host.OrdinaryOpenServer(
                self.outbound, path=self.runtime / "other.sock", uid=os.getuid() + 1,
            )
            with self.assertRaises(ValueError):
                other.start()

    def test_main_profile_detection_walks_parent_chain(self):
        proc = self.runtime / "proc"
        for pid, parent, arguments in (
            (30, 20, [b"host.py"]),
            (20, 10, [b"chrome", b"--user-data-dir=/profiles/main"]),
            (10, 1, [b"init"]),
        ):
            directory = proc / str(pid)
            directory.mkdir(parents=True)
            (directory / "cmdline").write_bytes(b"\0".join(arguments) + b"\0")
            (directory / "stat").write_text(f"{pid} (process) S {parent} 0 0 0\n")
        self.assertEqual(host.native_host_profile(30, proc), pathlib.Path("/profiles/main"))
        self.assertTrue(host.is_main_profile_host("/profiles/main", 30, proc))
        self.assertFalse(host.is_main_profile_host("/profiles/other", 30, proc))


class LauncherTest(unittest.TestCase):
    def setUp(self):
        self.base = ROOT / ".tmp" / f"launcher-{os.getpid()}-{id(self):x}"
        self.base.mkdir(parents=True, mode=0o700)
        self.home = self.base / "home"
        self.bin = self.base / "bin"
        self.proc = self.base / "proc"
        self.home.mkdir()
        self.bin.mkdir()
        self.proc.mkdir()
        self.log = self.base / "log"
        self.fake_chrome = self.bin / "chrome"
        self.fake_chrome.write_text(
            "#!/usr/bin/env bash\nprintf '%s\\0' \"$@\" > \"$ABUSH_TEST_CHROME_LOG\"\n"
        )
        self.fake_chrome.chmod(0o755)
        self.fake_client = self.base / "root/scripts/ordinary-open-client.py"
        self.fake_client.parent.mkdir(parents=True)
        self.fake_client.write_text(
            "import os,sys,pathlib\n"
            "pathlib.Path(os.environ['ABUSH_TEST_CLIENT_LOG']).write_text(sys.argv[1])\n"
        )

    def tearDown(self):
        import shutil
        shutil.rmtree(self.base)

    def environment(self):
        return {
            **os.environ,
            "HOME": str(self.home),
            "ABUSH_CHROME_PROFILE": str(self.base / "profile"),
            "ABUSH_CHROME_BIN": str(self.fake_chrome),
            "ABUSH_PROC_ROOT": str(self.proc),
            "ABUSH_SERVERCONFIG_ROOT": str(self.base / "root"),
            "ABUSH_TEST_CHROME_LOG": str(self.log),
            "ABUSH_TEST_CLIENT_LOG": str(self.base / "client"),
        }

    def test_chrome_start_argv_never_contains_url(self):
        url = "https://example.test/secret"
        result = subprocess.run([ROOT / "scripts/chromium", url], env=self.environment(),
                                capture_output=True, text=True, timeout=2)
        self.assertEqual(result.returncode, 0, result.stderr)
        deadline = time.monotonic() + 1
        while time.monotonic() < deadline and not self.log.exists():
            time.sleep(0.01)
        arguments = self.log.read_bytes().split(b"\0")
        self.assertNotIn(url.encode(), arguments)
        self.assertIn(b"--restore-last-session", arguments)
        self.assertEqual((self.base / "client").read_text(), url)

    def test_running_chrome_skips_start_and_still_submits_socket_client(self):
        process = self.proc / "42"
        process.mkdir()
        profile = self.base / "profile"
        (process / "cmdline").write_bytes(
            b"chrome\0--user-data-dir=" + os.fsencode(profile) + b"\0"
        )
        url = "https://example.test/running"
        result = subprocess.run([ROOT / "scripts/chromium", url], env=self.environment(),
                                capture_output=True, text=True, timeout=2)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.log.exists())
        self.assertEqual((self.base / "client").read_text(), url)

    def test_missing_socket_client_failure_is_nonzero_without_fallback(self):
        missing_root = self.base / "missing-root"
        missing_root.mkdir()
        environment = self.environment()
        environment["ABUSH_SERVERCONFIG_ROOT"] = str(missing_root)
        result = subprocess.run(
            [ROOT / "scripts/chromium", "https://example.test/no-socket"],
            env=environment, capture_output=True, text=True, timeout=2)
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("chromium-remote", result.stderr)

    def test_luakit_argument_count_and_file_uri_encoding(self):
        env = self.environment()
        capture = self.base / "capture"
        launcher = self.bin / "capture"
        launcher.write_text(
            "#!/usr/bin/env bash\nprintf '%s' \"$1\" > \"$ABUSH_TEST_CAPTURE\"\n"
        )
        launcher.chmod(0o755)
        env.update({"ABUSH_CHROMIUM_LAUNCHER": str(launcher),
                    "ABUSH_TEST_CAPTURE": str(capture)})
        for arguments in ([], ["one", "two"]):
            result = subprocess.run([ROOT / "scripts/luakit", *arguments], env=env,
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 2)
        target = self.base / "file with space#hash.txt"
        target.write_text("x")
        result = subprocess.run([ROOT / "scripts/luakit", str(target)], env=env,
                                capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(capture.read_text(), target.resolve().as_uri())


if __name__ == "__main__":
    unittest.main()
