#!/usr/bin/env python3

import importlib.util
import json
import pathlib
import socket
import threading
import unittest
from tempfile import TemporaryDirectory

ROOT = pathlib.Path(__file__).resolve().parents[1]
HOST = ROOT / "chromium/external-links/host.py"
spec = importlib.util.spec_from_file_location("browser_peer_host", HOST)
host = importlib.util.module_from_spec(spec)
spec.loader.exec_module(host)


class FakePeer:
    def __init__(self, path, principal, fence):
        self.path = pathlib.Path(path)
        self.principal = principal
        self.fence = fence
        self.received = []
        self.connected = threading.Event()
        self.done = threading.Event()
        self.client = None
        self.server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.server.bind(str(self.path))
        self.server.listen(1)
        self.thread = threading.Thread(target=self.run, daemon=True)
        self.thread.start()

    def run(self):
        try:
            self.client, _ = self.server.accept()
            self.received.append(host.read_peer_message(self.client))
            self.client.sendall(host.encode_peer_message({
                "type": "hello_accepted",
                "request_id": "hello-request",
                "principal_id": self.principal,
                "executor_fence": self.fence,
            }))
            self.connected.set()
            while True:
                self.received.append(host.read_peer_message(self.client))
        except (OSError, EOFError):
            pass
        finally:
            self.done.set()

    def send(self, message):
        self.client.sendall(host.encode_peer_message(message))

    def wait_for(self, message_type, timeout=2):
        attempts = max(1, int(timeout / 0.01))
        for _ in range(attempts):
            for message in self.received:
                if message.get("type") == message_type:
                    return message
            if self.done.wait(0.01):
                break
        raise AssertionError(f"timed out waiting for {message_type}: {self.received}")

    def close(self):
        if self.client:
            self.client.close()
        self.server.close()
        self.done.wait(2)
        self.thread.join(timeout=2)


class BrowserPeerBrokerTest(unittest.TestCase):
    def setUp(self):
        (ROOT / ".tmp").mkdir(exist_ok=True)
        self.temporary = TemporaryDirectory(dir=ROOT / ".tmp")
        self.root = pathlib.Path(self.temporary.name) / "browser-remotes"
        self.root.mkdir(mode=0o700)
        self.servers = []

    def tearDown(self):
        for server in self.servers:
            server.close()
        self.temporary.cleanup()

    def connection(self, remote_key, connection_id):
        directory = self.root / remote_key / connection_id
        directory.parent.mkdir(mode=0o700, exist_ok=True)
        directory.mkdir(mode=0o700)
        socket_path = directory / "browser-peer.sock"
        path = directory / "connection.json"
        path.write_text(json.dumps({
            "schema_version": 1,
            "remote_key": remote_key,
            "connection_id": connection_id,
            "display_name": remote_key,
            "browser_socket": str(socket_path),
            "remote_browser_socket": "/run/user/1000/abush/browser-peer.sock",
        }))
        path.chmod(0o600)
        return path, socket_path

    def peer(self, remote_key="r_12345678", connection_id="c_12345678",
             principal="principal-one", epoch=11):
        _, socket_path = self.connection(remote_key, connection_id)
        server = FakePeer(socket_path, principal, {
            "executor_id": f"executor-{connection_id}", "epoch": epoch,
        })
        self.servers.append(server)
        return server

    def next_event(self, broker, event_name):
        while True:
            message = broker.outbound.get(timeout=2)
            if message.get("event") == event_name:
                return message

    def request(self, operation, payload):
        return {
            "version": 1, "id": operation, "service": "abush_browser",
            "operation": operation, "payload": payload,
        }

    def test_v3_handshake_and_ensure_completion_are_strictly_correlated(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            hello = server.received[0]
            self.assertEqual(hello["version"], 3)
            self.assertEqual(set(hello), {
                "type", "version", "executor_id", "nonce", "capabilities",
            })
            server.send({
                "type": "ensure_managed_page",
                "request_id": "request-one",
                "ensure_id": "ensure-one",
                "workspace_id": "workspace-authoritative",
                "expected_page": None,
                "executor_fence": server.fence,
            })
            forwarded = self.next_event(broker, "abush_browser.ensure_managed_page")
            self.assertEqual(forwarded["payload"], {
                "remote_key": "r_12345678",
                "connection_id": "c_12345678",
                "principal_id": "principal-one",
                "workspace_id": "workspace-authoritative",
                "request_id": "request-one",
                "ensure_id": "ensure-one",
                "executor_fence": server.fence,
                "expected_page": None,
            })
            payload = {**forwarded["payload"], "completion": {
                "outcome": "created", "window_id": "10", "page_id": "20",
            }}
            broker.handle_request(self.request("ensure_managed_page_completion", payload))
            self.assertEqual(server.wait_for("ensure_managed_page_completed"), {
                "type": "ensure_managed_page_completed",
                "request_id": "request-one",
                "ensure_id": "ensure-one",
                "executor_fence": server.fence,
                "completion": {"outcome": "created", "window_id": "10", "page_id": "20"},
            })
            with self.assertRaises(ValueError):
                broker.handle_request(self.request("ensure_managed_page_completion", payload))
        finally:
            broker.close()

    def test_cancel_ensure_forwards_exact_authenticated_event_without_closing_peer(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            server.send({
                "type": "ensure_managed_page", "request_id": "ensure-request",
                "ensure_id": "ensure-one", "workspace_id": "workspace-authoritative",
                "expected_page": None, "executor_fence": server.fence,
            })
            self.next_event(broker, "abush_browser.ensure_managed_page")
            server.send({
                "type": "cancel_ensure_managed_page", "request_id": "ensure-request",
                "ensure_id": "ensure-one", "executor_fence": server.fence,
            })
            forwarded = self.next_event(
                broker, "abush_browser.cancel_ensure_managed_page")["payload"]
            self.assertEqual(forwarded, {
                "remote_key": "r_12345678", "connection_id": "c_12345678",
                "principal_id": "principal-one", "request_id": "ensure-request",
                "ensure_id": "ensure-one", "executor_fence": server.fence,
            })
            self.assertFalse(server.done.is_set())
        finally:
            broker.close()

    def test_stale_cancel_ensure_is_ignored_without_cancelling_newer_ensure(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            server.send({
                "type": "ensure_managed_page", "request_id": "new-request",
                "ensure_id": "ensure-one", "workspace_id": "workspace-authoritative",
                "expected_page": None, "executor_fence": server.fence,
            })
            payload = self.next_event(broker, "abush_browser.ensure_managed_page")["payload"]
            server.send({
                "type": "cancel_ensure_managed_page", "request_id": "old-request",
                "ensure_id": "ensure-one", "executor_fence": server.fence,
            })
            broker.handle_request(self.request("ensure_managed_page_completion", {
                **payload, "completion": {
                    "outcome": "created", "window_id": "window", "page_id": "page",
                },
            }))
            self.assertEqual(server.wait_for("ensure_managed_page_completed")["request_id"],
                             "new-request")
            self.assertFalse(server.done.is_set())
        finally:
            broker.close()

    def test_stale_principal_fence_and_connection_fail_closed(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            server.send({
                "type": "ensure_managed_page", "request_id": "request-one",
                "ensure_id": "ensure-one", "workspace_id": "workspace-authoritative",
                "expected_page": None, "executor_fence": server.fence,
            })
            payload = self.next_event(broker, "abush_browser.ensure_managed_page")["payload"]
            completion = {"outcome": "rejected", "code": "invalid_command"}
            for change in (
                {"principal_id": "other"},
                {"connection_id": "c_abcdefgh"},
                {"executor_fence": {**server.fence, "epoch": 12}},
                {"workspace_id": "other"},
                {"request_id": "other"},
                {"extra": True},
            ):
                with self.subTest(change=change), self.assertRaises(ValueError):
                    broker.handle_request(self.request("ensure_managed_page_completion", {
                        **payload, **change, "completion": completion,
                    }))
            broker.handle_request(self.request("ensure_managed_page_completion", {
                **payload, "completion": completion,
            }))
        finally:
            broker.close()

    def test_execute_event_includes_authenticated_principal(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            page = {"window": {
                "workspace_id": "workspace", "window_id": "window", "generation": 1,
            }, "page_id": "20", "generation": 1}
            server.send({
                "type": "execute", "request_id": "request", "effect_id": "effect",
                "executor_fence": server.fence,
                "command": {"command": "navigate", "page": page,
                            "url": "https://example.test/"},
            })
            forwarded = self.next_event(broker, "abush_browser.execute")["payload"]
            self.assertEqual(forwarded["principal_id"], "principal-one")
            completion = {key: value for key, value in forwarded.items() if key != "command"}
            with self.assertRaises(ValueError):
                broker.handle_request(self.request("executor_completion", {
                    **completion, "principal_id": "other", "completion": "applied",
                }))
            broker.handle_request(self.request("executor_completion", {
                **completion, "completion": "applied",
            }))
            self.assertEqual(server.wait_for("completion")["completion"], "applied")
        finally:
            broker.close()

    def test_cancel_uses_distinct_request_and_forwards_execution_request(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            page = {"window": {
                "workspace_id": "workspace", "window_id": "window", "generation": 1,
            }, "page_id": "20", "generation": 1}
            server.send({
                "type": "execute", "request_id": "current-request", "effect_id": "effect",
                "executor_fence": server.fence,
                "command": {"command": "navigate", "page": page,
                            "url": "https://example.test/"},
            })
            self.next_event(broker, "abush_browser.execute")
            server.send({
                "type": "cancel", "request_id": "current-request", "effect_id": "effect",
                "executor_fence": server.fence,
            })
            server.done.wait(2)
            self.assertTrue(server.done.is_set())
        finally:
            broker.close()

    def test_distinct_cancel_request_is_forwarded_with_original_execute_request(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            page = {"window": {
                "workspace_id": "workspace", "window_id": "window", "generation": 1,
            }, "page_id": "20", "generation": 1}
            server.send({
                "type": "execute", "request_id": "execute-request", "effect_id": "effect",
                "executor_fence": server.fence,
                "command": {"command": "navigate", "page": page,
                            "url": "https://example.test/"},
            })
            self.next_event(broker, "abush_browser.execute")
            server.send({
                "type": "cancel", "request_id": "cancel-request", "effect_id": "effect",
                "executor_fence": server.fence,
            })
            forwarded = self.next_event(broker, "abush_browser.cancel")["payload"]
            self.assertEqual(forwarded, {
                "remote_key": "r_12345678", "connection_id": "c_12345678",
                "principal_id": "principal-one", "request_id": "cancel-request",
                "execution_request_id": "execute-request", "effect_id": "effect",
                "executor_fence": server.fence,
            })
        finally:
            broker.close()

    def test_rejected_completion_only_accepts_browser_error_codes(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            for index, code in enumerate(sorted(host.BROWSER_ERROR_CODES)):
                request_id = f"request-{index}"
                ensure_id = f"ensure-{index}"
                server.send({
                    "type": "ensure_managed_page", "request_id": request_id,
                    "ensure_id": ensure_id, "workspace_id": f"workspace-{index}",
                    "expected_page": None, "executor_fence": server.fence,
                })
                payload = self.next_event(
                    broker, "abush_browser.ensure_managed_page")["payload"]
                broker.handle_request(self.request("ensure_managed_page_completion", {
                    **payload, "completion": {"outcome": "rejected", "code": code},
                }))
            server.send({
                "type": "ensure_managed_page", "request_id": "invalid-request",
                "ensure_id": "invalid-ensure", "workspace_id": "workspace-invalid",
                "expected_page": None, "executor_fence": server.fence,
            })
            payload = self.next_event(broker, "abush_browser.ensure_managed_page")["payload"]
            with self.assertRaises(ValueError):
                broker.handle_request(self.request("ensure_managed_page_completion", {
                    **payload, "completion": {
                        "outcome": "rejected", "code": "workspace_unavailable",
                    },
                }))
        finally:
            broker.close()

    def test_v2_hello_shape_and_topology_messages_are_rejected(self):
        peer = host.RemotePeer(None, {})
        peer.executor_fence = {"executor_id": "executor", "epoch": 1}
        with self.assertRaises(ValueError):
            peer._accept_hello({
                "type": "hello_accepted", "request_id": "hello",
                "principal_id": "principal", "executor_fence": peer.executor_fence,
                "default_window": {"workspace_id": "w", "window_id": "x", "generation": 1},
            })
        with self.assertRaises(ValueError):
            peer.handle({"type": "topology_response", "request_id": "request", "result": {
                "outcome": "rejected", "code": "old_wire",
            }})


if __name__ == "__main__":
    unittest.main()
