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
        self.default_window = {
            "workspace_id": f"workspace-{principal}",
            "window_id": "window-default",
            "generation": 1,
        }
        self.received = []
        self.ready = threading.Event()
        self.connected = threading.Event()
        self.done = threading.Event()
        self.client = None
        self.server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.server.bind(str(self.path))
        self.server.listen(1)
        self.thread = threading.Thread(target=self.run, daemon=True)
        self.thread.start()
        self.ready.set()

    def run(self):
        try:
            self.client, _ = self.server.accept()
            hello = host.read_peer_message(self.client)
            self.received.append(hello)
            self.client.sendall(host.encode_peer_message({
                "type": "hello_accepted",
                "request_id": "hello-request",
                "principal_id": self.principal,
                "executor_fence": self.fence,
                "default_window": self.default_window,
            }))
            self.connected.set()
            while True:
                self.received.append(host.read_peer_message(self.client))
                received = self.received[-1]
                if received.get("type") == "managed_page_created":
                    self.client.sendall(host.encode_peer_message({
                        "type": "topology_response",
                        "request_id": received["request_id"],
                        "result": {"outcome": "page_created", "page": {
                            "window": received["window"],
                            "page_id": received["page_id"],
                            "generation": 1,
                        }},
                    }))
                elif received.get("type") == "managed_page_moved_outside":
                    self.client.sendall(host.encode_peer_message({
                        "type": "topology_response",
                        "request_id": received["request_id"],
                        "result": {"outcome": "page_moved_outside"},
                    }))
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
        self.fail_timeout(message_type)

    def fail_timeout(self, message_type):
        raise AssertionError(f"timed out waiting for {message_type}: {self.received}")

    def close(self):
        if self.client:
            self.client.close()
        self.server.close()
        self.done.wait(2)
        self.thread.join(timeout=2)


class BrowserPeerBrokerTest(unittest.TestCase):
    def setUp(self):
        self.temporary = TemporaryDirectory(dir=ROOT / ".tmp")
        self.root = pathlib.Path(self.temporary.name) / "browser-remotes"
        self.root.mkdir(mode=0o700)
        self.servers = []

    def tearDown(self):
        for server in self.servers:
            server.close()
        self.temporary.cleanup()

    def connection(self, remote_key, connection_id, display_name=None):
        directory = self.root / remote_key / connection_id
        directory.parent.mkdir(mode=0o700, exist_ok=True)
        directory.mkdir(mode=0o700)
        socket_path = directory / "browser-peer.sock"
        metadata = {
            "schema_version": 1,
            "remote_key": remote_key,
            "connection_id": connection_id,
            "display_name": display_name or remote_key,
            "browser_socket": str(socket_path),
            "remote_browser_socket": "/run/user/1000/abush/browser-peer.sock",
        }
        path = directory / "connection.json"
        path.write_text(json.dumps(metadata))
        path.chmod(0o600)
        return directory, path, socket_path

    def peer(self, remote_key, connection_id, principal, epoch):
        _, _, socket_path = self.connection(remote_key, connection_id)
        fence = {"executor_id": f"executor-{connection_id}", "epoch": epoch}
        server = FakePeer(socket_path, principal, fence)
        self.servers.append(server)
        return server

    def test_metadata_validation_rejects_modes_symlinks_and_escape(self):
        directory, path, _ = self.connection("r_12345678", "c_12345678")
        self.assertEqual(host.validate_remote_metadata(path, self.root)["connection_id"],
                         "c_12345678")
        path.chmod(0o644)
        with self.assertRaises(ValueError):
            host.validate_remote_metadata(path, self.root)
        path.chmod(0o600)
        data = json.loads(path.read_text())
        data["browser_socket"] = str(directory.parent / "browser-peer.sock")
        path.write_text(json.dumps(data))
        path.chmod(0o600)
        with self.assertRaises(ValueError):
            host.validate_remote_metadata(path, self.root)
        path.unlink()
        outside = directory / "outside.json"
        outside.write_text("{}")
        outside.chmod(0o600)
        path.symlink_to(outside)
        with self.assertRaises(ValueError):
            host.validate_remote_metadata(path, self.root)

    def test_distinct_remotes_handshake_list_ping_and_execute_unknown(self):
        first = self.peer("r_12345678", "c_12345678", "principal-one", 11)
        second = self.peer("r_abcdefgh", "c_abcdefgh", "principal-two", 22)
        local_peer = self.peer("r_local000", "c_local000", "principal-local", 33)
        broker = host.BrowserBroker(self.root)
        try:
            remotes = broker.refresh()
            self.assertEqual(len(remotes), 3)
            self.assertTrue(all(remote["connected"] for remote in remotes))
            self.assertEqual({remote["principal_id"] for remote in remotes},
                             {"principal-one", "principal-two", "principal-local"})
            for server in (first, second, local_peer):
                hello = server.received[0]
                self.assertEqual(hello, {
                    "type": "hello",
                    "version": 2,
                    "executor_id": broker.executor_id({
                        "remote_key": server.path.parent.parent.name,
                    }),
                    "nonce": hello["nonce"],
                    "capabilities": {"cancellation": True},
                })
                self.assertRegex(hello["nonce"], r"^[0-9a-f]{32}$")
            first.send({"type": "ping", "request_id": "ping-1", "ping_id": "ping-7"})
            self.assertEqual(first.wait_for("pong"), {
                "type": "pong", "request_id": "ping-1", "ping_id": "ping-7",
            })
            second.send({
                "type": "execute",
                "request_id": "execute-1",
                "effect_id": "effect-1",
                "executor_fence": second.fence,
                "command": {"command": "navigate", "page": {
                    "window": second.default_window, "page_id": "page", "generation": 1,
                }, "url": "https://example.com/"},
            })
            execute = None
            while execute is None or execute.get("event") != "abush_browser.execute":
                execute = broker.outbound.get(timeout=2)
            self.assertEqual(execute, {
                "version": 1,
                "event": "abush_browser.execute",
                "service": "abush_browser",
                "payload": {
                    "remote_key": "r_abcdefgh", "connection_id": "c_abcdefgh",
                    "request_id": "execute-1", "effect_id": "effect-1",
                    "executor_fence": second.fence,
                    "command": {"command": "navigate", "page": {
                        "window": second.default_window, "page_id": "page", "generation": 1,
                    }, "url": "https://example.com/"},
                },
            })
            broker.handle_request({
                "version": 1, "id": "complete", "service": "abush_browser",
                "operation": "executor_completion", "payload": {
                    "remote_key": "r_abcdefgh", "connection_id": "c_abcdefgh",
                    "request_id": "execute-1", "effect_id": "effect-1",
                    "executor_fence": second.fence, "completion": "applied",
                },
            })
            self.assertEqual(second.wait_for("completion"), {
                "type": "completion", "request_id": "execute-1", "effect_id": "effect-1",
                "executor_fence": second.fence, "completion": "applied",
            })
        finally:
            broker.close()

    def test_same_remote_has_one_active_peer_and_fails_over(self):
        first = self.peer("r_12345678", "c_12345678", "principal-one", 11)
        second = self.peer("r_12345678", "c_abcdefgh", "principal-two", 22)
        first_metadata = self.root / "r_12345678" / "c_12345678" / "connection.json"
        second_metadata = self.root / "r_12345678" / "c_abcdefgh" / "connection.json"
        first_metadata.touch()
        broker = host.BrowserBroker(self.root)
        try:
            remotes = broker.refresh()
            self.assertEqual(len(remotes), 1)
            self.assertEqual(remotes[0]["connection_id"], "c_12345678")
            self.assertTrue(first.connected.wait(2))
            self.assertFalse(second.connected.is_set())

            first_metadata.unlink()
            remotes = broker.refresh()
            self.assertEqual(len(remotes), 1)
            self.assertEqual(remotes[0]["connection_id"], "c_abcdefgh")
            self.assertTrue(second.connected.wait(2))
            self.assertEqual(remotes[0]["principal_id"], "principal-two")
            self.assertNotEqual(first.fence, second.fence)
            self.assertTrue(second_metadata.exists())
            events = []
            while not broker.outbound.empty():
                item = broker.outbound.get_nowait()
                if item.get("event") == "abush_browser.remote_state":
                    events.append(item["payload"])
            self.assertTrue(events)
            self.assertTrue(all(item["connected"] for item in events))
            self.assertEqual(events[-1]["connection_id"], "c_abcdefgh")
        finally:
            broker.close()

    def test_live_disconnect_fails_over_without_disconnected_event(self):
        first = self.peer("r_12345678", "c_12345678", "principal-one", 11)
        second = self.peer("r_12345678", "c_abcdefgh", "principal-two", 22)
        first_metadata = self.root / "r_12345678" / "c_12345678" / "connection.json"
        first_metadata.touch()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            self.assertTrue(first.connected.wait(2))
            while not broker.outbound.empty():
                broker.outbound.get_nowait()
            first.client.shutdown(socket.SHUT_RDWR)
            first.client.close()
            self.assertTrue(second.connected.wait(2))
            state = broker.outbound.get(timeout=2)
            self.assertEqual(state["event"], "abush_browser.remote_state")
            self.assertTrue(state["payload"]["connected"])
            self.assertEqual(state["payload"]["connection_id"], "c_abcdefgh")
            self.assertTrue(broker.outbound.empty())
        finally:
            broker.close()

    def test_topology_requests_and_responses_match_v2_wire_shape(self):
        server = self.peer("r_12345678", "c_12345678", "principal-one", 11)
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            page_id = "page-one"
            page = broker.handle_request({
                "version": 1, "id": "register", "service": "abush_browser",
                "operation": "register_managed_page", "payload": {
                    "remote_key": "r_12345678", "connection_id": "c_12345678",
                    "window": server.default_window, "page_id": page_id,
                },
            })["page"]
            created = server.wait_for("managed_page_created")
            self.assertEqual(created, {
                "type": "managed_page_created", "request_id": created["request_id"],
                "executor_fence": server.fence, "window": server.default_window,
                "page_id": page_id,
            })
            self.assertEqual(page, {
                "window": server.default_window, "page_id": page_id, "generation": 1,
            })
            broker.handle_request({
                "version": 1, "id": "move", "service": "abush_browser",
                "operation": "moved_outside", "payload": {
                    "remote_key": "r_12345678", "connection_id": "c_12345678", "page": page,
                },
            })
            moved = server.wait_for("managed_page_moved_outside")
            self.assertEqual(moved, {
                "type": "managed_page_moved_outside", "request_id": moved["request_id"],
                "executor_fence": server.fence, "page": page,
            })
        finally:
            broker.close()

    def test_v1_like_peer_shapes_are_rejected(self):
        peer = host.RemotePeer(None, {})
        peer.executor_fence = {"executor_id": "executor", "epoch": 1}
        for message in (
            {"type": "ping", "request_id": "ping", "timestamp": 7},
            {"type": "execute", "request_id": "request", "effect": {},
             "executor_fence": 1},
            {"type": "topology_response", "request_id": "request",
             "outcome": "page_moved_outside"},
        ):
            with self.subTest(message=message), self.assertRaises(ValueError):
                peer.handle(message)

    def test_malformed_peer_does_not_affect_other_and_refresh_cleans_up(self):
        first = self.peer("r_12345678", "c_12345678", "principal-one", 11)
        second = self.peer("r_abcdefgh", "c_abcdefgh", "principal-two", 22)
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            first.client.sendall((host.MAX_MESSAGE_SIZE + 1).to_bytes(4, "big"))
            self.assertTrue(first.done.wait(2))
            second.send({"type": "ping", "request_id": "still-live", "ping_id": "live"})
            self.assertEqual(second.wait_for("pong")["request_id"], "still-live")
            states = {item["connection_id"]: item for item in broker.list_remotes()}
            self.assertFalse(states["c_12345678"]["connected"])
            self.assertTrue(states["c_abcdefgh"]["connected"])
            metadata = self.root / "r_12345678" / "c_12345678" / "connection.json"
            metadata.unlink()
            remotes = broker.refresh()
            self.assertEqual([item["connection_id"] for item in remotes], ["c_abcdefgh"])
        finally:
            broker.close()


if __name__ == "__main__":
    unittest.main()
