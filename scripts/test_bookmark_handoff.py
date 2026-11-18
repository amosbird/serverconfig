#!/usr/bin/env python3

import importlib.util
import json
import os
import pathlib
import select
import signal
import struct
import subprocess
import sys
import threading
import unittest
from unittest import mock

import test_luakit_desktop_focus as desktop_tests

ROOT = pathlib.Path(__file__).resolve().parents[1]
HOST_PATH = ROOT / "rofi-chrome/host/main.py"
SPEC = importlib.util.spec_from_file_location("bookmark_local_host", HOST_PATH)
host = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(host)


def request(url="https://example.test/bookmark", request_id="a" * 32):
    return {"version": 1, "info": "openInBrowser", "request_id": request_id,
            "param": {"url": url}}


class BookmarkHandoffTest(unittest.TestCase):
    def setUp(self):
        self.desktop = desktop_tests.LuakitDesktopFocusTest()
        self.desktop.setUp()
        self.addCleanup(self.desktop.doCleanups)

    def start(self, running=True, message=None):
        environment = self.desktop.environment(running=running)
        environment["ABUSH_CHROMIUM_LAUNCHER"] = str(ROOT / "scripts/chromium")
        process = subprocess.Popen([sys.executable, "-B", HOST_PATH], env=environment,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

        def cleanup():
            if process.poll() is None:
                process.terminate()
            try:
                process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=1)
                self.fail("owned bookmark native host did not stop")
            for stream in (process.stdin, process.stdout, process.stderr):
                stream.close()

        self.addCleanup(cleanup)
        message = request() if message is None else message
        payload = json.dumps(message).encode()
        process.stdin.write(struct.pack("<I", len(payload)) + payload)
        process.stdin.flush()
        return process

    def finish(self, process):
        process.wait(timeout=3)
        self.assertEqual(process.returncode, 0, process.stderr.read().decode())
        data = process.stdout.read()
        self.assertGreaterEqual(len(data), 4)
        size = struct.unpack("<I", data[:4])[0]
        self.assertEqual(len(data), size + 4, "exactly one native terminal is published")
        response = json.loads(data[4:])
        self.assertEqual(response["request_id"], "a" * 32)
        return response

    def assert_no_reply(self, process):
        self.assertEqual(select.select([process.stdout], [], [], 0)[0], [],
                         "native success must follow both ordinary and Qtile completion")

    def test_warm_bookmark_reply_waits_for_applied_then_the_desktop_ack(self):
        gate = threading.Event()
        self.addCleanup(gate.set)
        self.desktop.qtile.before_reply = lambda: self.assertTrue(gate.wait(timeout=2))
        process = self.start()
        ordinary = self.desktop.outbound.get(timeout=2)["payload"]
        self.assert_no_reply(process)
        self.assertTrue(self.desktop.qtile.requests.empty())
        self.desktop.complete(ordinary)
        self.assertEqual(self.desktop.qtile.requests.get(timeout=2), desktop_tests.FOCUS_COMMAND)
        self.assert_no_reply(process)
        gate.set()
        response = self.finish(process)
        self.assertEqual(response["completion"], "completed")
        self.assertTrue(self.desktop.outbound.empty())
        self.assertTrue(self.desktop.qtile.requests.empty())
        self.assertFalse(self.desktop.launcher.log.exists())

    def test_cold_bookmark_handoff_keeps_url_out_of_chrome_argv(self):
        process = self.start(running=False)
        ordinary = self.desktop.outbound.get(timeout=2)["payload"]
        self.assert_no_reply(process)
        self.desktop.complete(ordinary)
        self.assertEqual(self.finish(process)["completion"], "completed")
        self.assertEqual(self.desktop.qtile.requests.get(timeout=0.2), desktop_tests.FOCUS_COMMAND)
        self.assertNotIn(ordinary["url"].encode(),
                         self.desktop.launcher.log.read_bytes().split(b"\0"))
        self.assertTrue(self.desktop.outbound.empty())

    def test_non_applied_outcomes_never_acknowledge_success_or_switch_desktops(self):
        for completion in ("not_applied", "unknown"):
            with self.subTest(completion=completion):
                process = self.start()
                ordinary = self.desktop.outbound.get(timeout=2)["payload"]
                self.desktop.complete(ordinary, completion)
                response = self.finish(process)
                self.assertEqual(response["completion"], "unknown")
                self.assertIn(completion, response["message"])
                self.assertTrue(self.desktop.qtile.requests.empty())
                self.assertTrue(self.desktop.outbound.empty())

    def test_desktop_failure_preserves_the_applied_page_and_partial_diagnostic(self):
        self.desktop.qtile.response = b'[1,"group is unavailable"]'
        process = self.start()
        ordinary = self.desktop.outbound.get(timeout=2)["payload"]
        self.desktop.complete(ordinary)
        response = self.finish(process)
        self.assertEqual(response["completion"], "unknown")
        self.assertIn("URL opened; desktop switch failed", response["message"])
        self.assertIn("Do not retry", response["message"])
        self.assertEqual(self.desktop.qtile.requests.get(timeout=0.2), desktop_tests.FOCUS_COMMAND)
        self.assertTrue(self.desktop.qtile.requests.empty())
        self.assertTrue(self.desktop.outbound.empty())

    def test_cancelling_the_native_host_cancels_only_its_owned_launcher(self):
        process = self.start()
        ordinary = self.desktop.outbound.get(timeout=2)["payload"]
        process.send_signal(signal.SIGTERM)
        response = self.finish(process)
        self.assertEqual(response["completion"], "unknown")
        self.assertIn("cancelled", response["message"])
        cancel = self.desktop.outbound.get(timeout=2)["payload"]
        self.assertEqual(cancel["operation"], "ordinary_open_cancel")
        self.assertEqual(cancel["request_id"], ordinary["request_id"])
        self.desktop.complete(ordinary)
        self.assertTrue(self.desktop.qtile.requests.empty())

    def test_invalid_urls_fail_before_browser_or_desktop_effects(self):
        process = self.start(message=request("file:///private"))
        self.assertEqual(self.finish(process)["completion"], "not_started")
        self.assertTrue(self.desktop.outbound.empty())
        self.assertTrue(self.desktop.qtile.requests.empty())


class BookmarkHostBoundaryTest(unittest.TestCase):
    def test_local_launcher_wait_is_exact_and_failure_is_not_false_success(self):
        process = mock.Mock()
        process.poll.return_value = 0
        with mock.patch.object(host.subprocess, "Popen", return_value=process) as spawn, \
                mock.patch.object(host, "wait_browser_open", return_value=(0, "")) as wait:
            result = host.handle_message(request())
        self.assertEqual(result["completion"], "completed")
        spawn.assert_called_once_with([host.BOOKMARK_BROWSER, "https://example.test/bookmark"],
            stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        wait.assert_called_once()

    def test_completed_launcher_does_not_wait_for_inherited_stderr_to_close(self):
        reader, writer = os.pipe()
        with os.fdopen(reader, "rb", buffering=0) as diagnostic, \
                os.fdopen(writer, "wb", buffering=0) as inherited:
            inherited.write(b"final available diagnostic")
            process = mock.Mock()
            process.stderr = diagnostic
            process.poll.return_value = 0
            with mock.patch.object(host, "BOOKMARK_OPEN_TIMEOUT", 0.2):
                self.assertEqual(host.wait_browser_open(process, lambda: False),
                                 (0, "final available diagnostic"))
            self.assertFalse(inherited.closed)

    def test_timeout_and_ignored_termination_have_bounded_owned_child_cleanup(self):
        process = mock.Mock()
        process.poll.return_value = None
        process.wait.side_effect = [subprocess.TimeoutExpired("owned fixture", 1), -signal.SIGKILL]
        with mock.patch.object(host.subprocess, "Popen", return_value=process) as spawn, \
                mock.patch.object(host, "wait_browser_open", side_effect=TimeoutError("deadline")):
            result = host.handle_message(request())
        self.assertEqual(result["completion"], "unknown")
        self.assertEqual(process.wait.call_args_list, [mock.call(timeout=1), mock.call(timeout=1)])
        process.terminate.assert_called_once()
        process.kill.assert_called_once()
        spawn.assert_called_once()


if __name__ == "__main__":
    unittest.main()
