#!/usr/bin/env python3

import importlib.util
import io
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
            "deadline_ms": 2_000_000_000_000,
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
        self.assertEqual(event["payload"]["request_id"], "request-one")
        self.assertEqual(event["payload"], self.request())
        completion = host.ordinary_open_response("request-one", "applied")
        self.server.complete(completion)
        self.assertEqual(host.read_ordinary_open_message(client), completion)
        client.close()
        self.assertEqual(self.path.stat().st_mode & 0o777, 0o600)
        self.assertEqual(self.path.parent.stat().st_mode & 0o777, 0o700)

    def test_strict_deadline_is_required_for_native_ordinary_mutations(self):
        broker = host.BrowserBroker(outbound=queue.Queue())
        binding = mock.Mock()
        binding.prepare_ordinary_window.return_value = {"xid": 42, "nonce": "a" * 32}
        broker.x11_binding = mock.Mock(return_value=binding)
        envelope = {
            "version": 1,
            "id": "deadline-native",
            "service": "ordinary_open",
            "operation": "prepare",
            "payload": {"nonce": "a" * 32},
        }
        with self.assertRaisesRegex(ValueError, "payload"):
            broker.handle_request(envelope)
        with self.assertRaisesRegex(TimeoutError, "deadline"):
            broker.handle_request({**envelope, "payload": {
                "nonce": "a" * 32, "deadline_ms": int(time.time() * 1000) - 1,
            }})
        deadline = int(time.time() * 1000) + 30_000
        self.assertEqual(broker.handle_request({**envelope, "payload": {
            "nonce": "a" * 32, "deadline_ms": deadline,
        }}), {"xid": 42, "nonce": "a" * 32})
        binding.prepare_ordinary_window.assert_called_once_with({"nonce": "a" * 32})

    def test_validate_request_rejects_non_web_and_non_file_schemes(self):
        request = self.request("scheme-check")
        for url in (
            "chrome-extension://id/page", "javascript:alert(1)", "data:text/plain,x",
            "file://remote/share", "https:///missing-host",
        ):
            with self.subTest(url=url), self.assertRaisesRegex(ValueError, "URL"):
                host.validate_ordinary_open_request({**request, "url": url})
        validated = host.validate_ordinary_open_request({
            **request, "url": "file:///home/amos/a.txt",
        })
        self.assertEqual(validated["url"], "file:///home/amos/a.txt")

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

    def test_conflicting_duplicate_is_rejected_without_second_event(self):
        first = self.connect(self.request())
        self.assertEqual(self.outbound.get(timeout=1)["payload"], self.request())
        conflicting = self.request()
        conflicting["url"] = "https://different.test/"
        second = self.connect(conflicting)
        with self.assertRaises((EOFError, ConnectionResetError, socket.timeout)):
            host.read_ordinary_open_message(second)
        completion = host.ordinary_open_response("request-one", "applied")
        self.server.complete(completion)
        self.assertEqual(host.read_ordinary_open_message(first), completion)
        first.close()
        second.close()

    def test_deadline_timeout_publishes_typed_cancel_and_never_extends(self):
        request = {**self.request("deadline"),
                   "deadline_ms": int(time.time() * 1000) + 120}
        client = self.connect(request)
        opened = self.outbound.get(timeout=1)
        self.assertEqual(opened["payload"]["deadline_ms"], request["deadline_ms"])
        cancel = self.outbound.get(timeout=1)
        self.assertEqual(cancel, {
            "version": 1,
            "event": "ordinary_open_cancel",
            "service": "ordinary_open",
            "payload": {
                "version": 1,
                "request_id": "deadline",
                "operation": "ordinary_open_cancel",
            },
        })
        with self.assertRaises((EOFError, ConnectionResetError, socket.timeout)):
            host.read_ordinary_open_message(client)
        client.close()

    def test_client_disconnect_publishes_typed_cancel(self):
        client = self.connect(self.request("disconnect"))
        self.assertEqual(self.outbound.get(timeout=1)["event"], "ordinary_open")
        client.close()
        cancel = self.outbound.get(timeout=1)
        self.assertEqual(cancel["event"], "ordinary_open_cancel")
        self.assertEqual(cancel["payload"]["request_id"], "disconnect")

    def test_internal_submit_waits_for_terminal_completion(self):
        result = queue.Queue()
        submitted = threading.Thread(
            target=lambda: result.put(self.server.submit(
                "https://example.test/from-extension", request_id="internal-request")),
            daemon=True,
        )
        submitted.start()
        event = self.outbound.get(timeout=1)
        self.assertEqual(event["payload"]["request_id"], "internal-request")
        self.assertTrue(submitted.is_alive(), "accepted is not a terminal applied result")
        self.server.complete(host.ordinary_open_response("internal-request", "unknown"))
        submitted.join(timeout=1)
        self.assertFalse(submitted.is_alive())
        self.assertEqual(result.get_nowait(), "unknown")

    def test_real_peer_credentials_are_read_from_connected_socket(self):
        client = self.connect(self.request("peer-credentials"))
        event = self.outbound.get(timeout=1)
        self.assertEqual(event["payload"]["request_id"], "peer-credentials")
        self.server.complete(host.ordinary_open_response("peer-credentials", "not_applied"))
        self.assertEqual(host.read_ordinary_open_message(client)["completion"], "not_applied")
        client.close()

    def test_active_socket_is_never_unlinked(self):
        other = host.OrdinaryOpenServer(
            self.outbound, path=self.path, timeout=1,
        )
        with self.assertRaisesRegex(ValueError, "already active"):
            other.start()
        self.assertTrue(self.path.exists())
        client = self.connect(self.request("still-active"))
        self.assertEqual(self.outbound.get(timeout=1)["payload"]["request_id"], "still-active")
        self.server.complete(host.ordinary_open_response("still-active", "applied"))
        self.assertEqual(host.read_ordinary_open_message(client)["completion"], "applied")
        client.close()

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


class FakeWebSocket:
    def __init__(self, responses):
        self.responses = list(responses)
        self.sent = []

    def __enter__(self):
        return self

    def __exit__(self, *_args):
        return False

    def send(self, message):
        self.sent.append(json.loads(message))

    def recv(self, timeout=None):
        if not self.responses:
            raise TimeoutError("no response")
        response = self.responses.pop(0)
        if isinstance(response, BaseException):
            raise response
        return json.dumps(response)


class OrdinaryFileNavigationTest(unittest.TestCase):
    nonce = "a" * 32
    url = "file:///home/amos/file%20with%20space.txt#section"

    def target(self, *, target_id="target-one", nonce=None, url=None, title=None):
        nonce = nonce or self.nonce
        return {
            "id": target_id,
            "type": "page",
            "url": url or host.ordinary_staging_url(nonce),
            "title": title or f"__ABUSH_ORDINARY__:{nonce}",
            "webSocketDebuggerUrl": f"ws://127.0.0.1:9222/devtools/page/{target_id}",
        }

    @staticmethod
    def opener_for(*target_lists):
        responses = iter(target_lists)

        def open_targets(request_url, timeout):
            if request_url != "http://127.0.0.1:9222/json" or timeout != 1:
                raise AssertionError((request_url, timeout))
            return io.BytesIO(json.dumps(next(responses)).encode())

        return open_targets

    def payload(self, url=None):
        return {"nonce": self.nonce, "xid": 42, "url": url or self.url}

    def test_unique_nonce_title_and_url_target_navigates_exact_file_uri(self):
        websocket = FakeWebSocket([
            {"method": "Page.frameNavigated"},
            {"id": 1, "result": {"frameId": "frame-one"}},
        ])
        staging = self.target()
        navigated = self.target(url=self.url, title="file with space.txt")
        verifier = mock.Mock(return_value=True)
        connect = mock.Mock(return_value=websocket)

        result = host.navigate_ordinary_file(
            self.payload(), verifier, opener=self.opener_for([staging], [navigated]),
            websocket_connect=connect, verify_attempts=1,
        )

        self.assertEqual(result, {**self.payload(), "completion": "applied"})
        verifier.assert_called_once_with({"nonce": self.nonce, "xid": 42})
        connect.assert_called_once_with(
            staging["webSocketDebuggerUrl"], open_timeout=1, close_timeout=1)
        self.assertEqual(websocket.sent, [{
            "id": 1,
            "method": "Page.navigate",
            "params": {"url": self.url},
        }])

    def test_ambiguous_staging_target_is_not_applied_without_websocket(self):
        connect = mock.Mock()
        result = host.navigate_ordinary_file(
            self.payload(), lambda _identity: True,
            opener=self.opener_for([self.target(), self.target(target_id="target-two")]),
            websocket_connect=connect,
        )
        self.assertEqual(result["completion"], "not_applied")
        connect.assert_not_called()

    def test_nonce_title_and_extension_url_must_all_correlate(self):
        wrong_nonce = "b" * 32
        candidates = [
            self.target(nonce=wrong_nonce),
            self.target(title="wrong title"),
            self.target(url=host.ordinary_staging_url(wrong_nonce)),
        ]
        connect = mock.Mock()
        result = host.navigate_ordinary_file(
            self.payload(), lambda _identity: True,
            opener=self.opener_for(candidates), websocket_connect=connect,
        )
        self.assertEqual(result["completion"], "not_applied")
        connect.assert_not_called()

    def test_target_url_must_be_strict_local_file_uri(self):
        for url in (
            "https://example.test/",
            "file://server/share",
            "file:///home/amos/file.txt?query=1",
            "FILE:///home/amos/file.txt",
        ):
            with self.subTest(url=url):
                opener = mock.Mock()
                result = host.navigate_ordinary_file(
                    self.payload(url), lambda _identity: True, opener=opener)
                self.assertEqual(result["completion"], "not_applied")
                opener.assert_not_called()

    def test_strict_file_url_accepts_file_fragments_but_not_remote_authorities(self):
        self.assertTrue(host._strict_file_url(self.url))
        self.assertFalse(host._strict_file_url("file://localhost/home/amos/file.txt"))

    def test_debug_port_can_be_injected(self):
        target = self.target()
        target["webSocketDebuggerUrl"] = "ws://127.0.0.1:9333/devtools/page/target-one"
        websocket = FakeWebSocket([{"id": 1, "result": {"frameId": "frame"}}])

        def opener(request_url, timeout):
            self.assertEqual(request_url, "http://127.0.0.1:9333/json")
            targets = [target] if not websocket.sent else [self.target(url=self.url)]
            targets[0]["webSocketDebuggerUrl"] = target["webSocketDebuggerUrl"]
            return io.BytesIO(json.dumps(targets).encode())

        result = host.navigate_ordinary_file(
            self.payload(), lambda _identity: True, debug_port="9333", opener=opener,
            websocket_connect=lambda *_args, **_kwargs: websocket, verify_attempts=1,
        )
        self.assertEqual(result["completion"], "applied")

    def test_failure_during_page_navigate_send_is_unknown(self):
        websocket = FakeWebSocket([])
        websocket.send = mock.Mock(side_effect=OSError("send result unknown"))
        result = host.navigate_ordinary_file(
            self.payload(), lambda _identity: True,
            opener=self.opener_for([self.target()]),
            websocket_connect=lambda *_args, **_kwargs: websocket,
        )
        self.assertEqual(result["completion"], "unknown")

    def test_failure_after_page_navigate_send_is_unknown(self):
        websocket = FakeWebSocket([TimeoutError("response lost")])
        result = host.navigate_ordinary_file(
            self.payload(), lambda _identity: True,
            opener=self.opener_for([self.target()]),
            websocket_connect=lambda *_args, **_kwargs: websocket,
        )
        self.assertEqual(result["completion"], "unknown")
        self.assertEqual(websocket.sent[0]["params"]["url"], self.url)


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
        self.chrome_ready = self.base / "chrome-ready"
        self.fake_chrome = self.bin / "chrome"
        self.fake_chrome.write_text(
            "#!/usr/bin/env bash\nprintf '%s\\0' \"$@\" > \"$ABUSH_TEST_CHROME_LOG\"\n"
            "touch \"$ABUSH_TEST_CHROME_READY\"\n"
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
            "ABUSH_TEST_CHROME_READY": str(self.chrome_ready),
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
        self.assertIn(b"--remote-debugging-port=9222", arguments)
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

    def test_unsupported_scheme_is_rejected_before_start_or_socket_submission(self):
        result = subprocess.run(
            [ROOT / "scripts/chromium", "chrome-extension://id/page.html"],
            env=self.environment(), capture_output=True, text=True, timeout=2,
        )
        self.assertEqual(result.returncode, 2)
        self.assertIn("unsupported URL scheme", result.stderr)
        self.assertFalse(self.log.exists())
        self.assertFalse((self.base / "client").exists())

    def test_flagged_or_app_launches_are_rejected_without_singleton_forwarding(self):
        remote_log = self.base / "remote"
        remote = self.bin / "chromium-remote"
        remote.write_text(
            "#!/usr/bin/env bash\nprintf '%s\\0' \"$@\" > \"$ABUSH_TEST_REMOTE_LOG\"\n"
        )
        remote.chmod(0o755)
        environment = self.environment()
        environment["PATH"] = f"{self.bin}:{environment['PATH']}"
        environment["ABUSH_TEST_REMOTE_LOG"] = str(remote_log)
        for arguments in (
            ["--current-group", "--app=chrome-extension://id/bookmarks.html"],
            ["--class=jupyterlab", "https://example.test/app"],
        ):
            with self.subTest(arguments=arguments):
                result = subprocess.run(
                    [ROOT / "scripts/chromium", *arguments], env=environment,
                    capture_output=True, text=True, timeout=2,
                )
                self.assertEqual(result.returncode, 2)
                self.assertIn("dedicated profile launcher", result.stderr)
                self.assertFalse((self.base / "client").exists())
                self.assertFalse(remote_log.exists())
                self.assertFalse(self.log.exists())

    def test_real_client_waits_for_delayed_server_and_reports_terminal_states(self):
        url = "https://example.test/delayed-server"
        for completion, expected in (("applied", 0), ("unknown", 1)):
            with self.subTest(completion=completion):
                socket_path = self.base / f"{completion}.sock"
                environment = self.environment()
                environment["ABUSH_SERVERCONFIG_ROOT"] = str(ROOT)
                environment["ABUSH_ORDINARY_OPEN_SOCKET"] = str(socket_path)
                environment["ABUSH_ORDINARY_OPEN_TIMEOUT"] = "2"
                outbound = queue.Queue()
                server = host.OrdinaryOpenServer(
                    outbound, path=socket_path, timeout=2,
                )

                def delayed_server():
                    time.sleep(0.1)
                    server.start()
                    event = outbound.get(timeout=1)
                    server.complete(host.ordinary_open_response(
                        event["payload"]["request_id"], completion))

                worker = threading.Thread(target=delayed_server, daemon=True)
                worker.start()
                result = subprocess.run(
                    [ROOT / "scripts/chromium", url], env=environment,
                    capture_output=True, text=True, timeout=3,
                )
                worker.join(timeout=1)
                server.close()
                self.assertEqual(result.returncode, expected, result.stderr)
                arguments = self.log.read_bytes().split(b"\0")
                self.assertNotIn(url.encode(), arguments)
                self.log.unlink()
                self.chrome_ready.unlink(missing_ok=True)

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
