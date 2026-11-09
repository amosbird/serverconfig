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
                "capabilities": {"cancellation": True},
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

    def peer(self, remote_key, connection_id, principal, fence):
        _, _, socket_path = self.connection(remote_key, connection_id)
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

    def test_two_peers_handshake_list_ping_and_execute_unknown(self):
        first = self.peer("r_12345678", "c_12345678", "principal-one", 11)
        second = self.peer("r_12345678", "c_abcdefgh", "principal-two", 22)
        broker = host.BrowserBroker(self.root)
        try:
            remotes = broker.refresh()
            self.assertEqual(len(remotes), 2)
            self.assertTrue(all(remote["connected"] for remote in remotes))
            self.assertEqual({remote["principal_id"] for remote in remotes},
                             {"principal-one", "principal-two"})
            for server in (first, second):
                hello = server.received[0]
                self.assertEqual(hello["type"], "hello")
                self.assertEqual(hello["version"], 1)
                self.assertTrue(hello["capabilities"]["cancellation"])
                self.assertIn(server.path.parent.name, hello["executor_id"])
            first.send({"type": "ping", "request_id": "ping-1", "timestamp": 7})
            self.assertEqual(first.wait_for("pong"), {
                "type": "pong", "request_id": "ping-1", "timestamp": 7,
            })
            effect = {"command": "navigate", "url": "https://example.com"}
            second.send({
                "type": "execute",
                "request_id": "execute-1",
                "effect": effect,
                "executor_fence": 22,
            })
            self.assertEqual(second.wait_for("completion"), {
                "type": "completion",
                "request_id": "execute-1",
                "effect": effect,
                "executor_fence": 22,
                "outcome": "unknown",
            })
        finally:
            broker.close()

    def test_malformed_peer_does_not_affect_other_and_refresh_cleans_up(self):
        first = self.peer("r_12345678", "c_12345678", "principal-one", 11)
        second = self.peer("r_abcdefgh", "c_abcdefgh", "principal-two", 22)
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            first.client.sendall((host.MAX_MESSAGE_SIZE + 1).to_bytes(4, "big"))
            self.assertTrue(first.done.wait(2))
            second.send({"type": "ping", "request_id": "still-live"})
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
