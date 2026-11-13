#!/usr/bin/env python3

import importlib.util
import json
import pathlib
import queue
import socket
import threading
import unittest
from tempfile import TemporaryDirectory
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
HOST = ROOT / "chromium/external-links/host.py"
spec = importlib.util.spec_from_file_location("browser_peer_host", HOST)
host = importlib.util.module_from_spec(spec)
spec.loader.exec_module(host)


class FakePeer:
    def __init__(self, path, principal, fence, owner_display=None):
        self.path = pathlib.Path(path)
        self.principal = principal
        self.fence = fence
        self.owner_display = owner_display or {
            "daemon_instance_id": "daemon-instance",
            "daemon_boot_id": "daemon-boot",
            "workbench_name": "workbench",
        }
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
                "type": "hello_prepared",
                "request_id": "hello-request",
                "principal_id": self.principal,
                "executor_fence": self.fence,
                "owner_display": self.owner_display,
            }))
            self.received.append(host.read_peer_message(self.client))
            self.client.sendall(host.encode_peer_message({
                "type": "hello_accepted",
                "request_id": "hello-request",
                "principal_id": self.principal,
                "executor_fence": self.fence,
                "owner_display": self.owner_display,
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

    def wait_for(self, message_type, timeout=2, request_id=None):
        attempts = max(1, int(timeout / 0.01))
        for _ in range(attempts):
            for message in self.received:
                if message.get("type") == message_type and \
                        (request_id is None or message.get("request_id") == request_id):
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
    def test_native_bootstrap_request_replays_exact_current_browser_identity(self):
        output = mock.Mock()
        native = host.NativeBrokerHost(
            input_stream=mock.Mock(), output_stream=output, remote_root=self.root,
            ordinary_open_factory=None, browser={
                "pid": 42,
                "starttime": "1234",
                "profile": pathlib.Path("/profiles/main"),
                "browser_boot_id": "chrome-current-boot",
            })
        native._respond({
            "version": 1,
            "id": "bootstrap-request",
            "service": "native_host",
            "operation": "bootstrap",
            "payload": {},
        })
        response = native.outbound.get(timeout=1)
        self.assertEqual(response, {
            "version": 1,
            "reply_to": "bootstrap-request",
            "ok": True,
            "payload": {
                "browser_boot_id": "chrome-current-boot",
                "browser_pid": 42,
                "browser_starttime": "1234",
                "ordinary_launch_leases": [],
            },
        })
        native._respond({
            "version": 1,
            "id": "invalid-bootstrap-request",
            "service": "native_host",
            "operation": "bootstrap",
            "payload": {"unexpected": True},
        })
        rejected = native.outbound.get(timeout=1)
        self.assertFalse(rejected["ok"])
        self.assertIn("must be empty", rejected["error"])
        native.broker.close()

    def test_native_host_writes_remote_state_before_activating_peer_reader(self):
        native = host.NativeBrokerHost(
            input_stream=mock.Mock(), output_stream=mock.Mock(), remote_root=self.root,
            ordinary_open_factory=None)
        peer = mock.Mock()
        peer.logical_state.return_value = {
            "remote_key": "r_12345678", "connection_id": "c_12345678",
            "display_name": "remote", "connected": True,
            "principal_id": "principal-one",
            "executor_fence": {"executor_id": "executor", "epoch": 1},
        }
        writes = []
        native.writer = threading.Thread(target=native._write_loop, daemon=True)
        try:
            with mock.patch.object(host, "send_message",
                                   side_effect=lambda message, _stream: writes.append(message)):
                native.writer.start()
                native.broker._activate_after_remote_state(peer)
            self.assertEqual(writes[0]["event"], "abush_browser.remote_state")
            peer.activate.assert_called_once_with()
        finally:
            native.outbound.put(None)
            native.writer.join(timeout=1)
            native.broker.close()
        self.assertFalse(native.writer.is_alive())

    def test_native_host_refresh_loop_discovers_metadata_after_empty_startup(self):
        native = host.NativeBrokerHost(
            input_stream=mock.Mock(), output_stream=mock.Mock(), remote_root=self.root,
            ordinary_open_factory=None)
        native.broker.write_barrier = None
        native.refresh_interval = 60
        native.refresh_thread.start()
        try:
            self.assertTrue(native.refresh_started.wait(1))
            server = self.peer()
            native.refresh_wakeup.set()
            self.assertTrue(server.connected.wait(2))
            self.assertEqual(native.broker.list_remotes(), [{
                "remote_key": "r_12345678",
                "connection_id": "c_12345678",
                "display_name": "r_12345678",
                "connected": True,
                "principal_id": "principal-one",
                "executor_fence": server.fence,
                "owner_display": server.owner_display,
            }])
        finally:
            native.refresh_stop.set()
            native.refresh_wakeup.set()
            native.refresh_thread.join(timeout=1)
            native.broker.close()
        self.assertFalse(native.refresh_thread.is_alive())

    def setUp(self):
        temporary_root = ROOT.parent.parent / ".tmp"
        temporary_root.mkdir(exist_ok=True)
        self.temporary = TemporaryDirectory(dir=temporary_root)
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
             principal="principal-one", epoch=11, owner_display=None):
        _, socket_path = self.connection(remote_key, connection_id)
        server = FakePeer(socket_path, principal, {
            "executor_id": f"executor-{connection_id}", "epoch": epoch,
        }, owner_display)
        self.servers.append(server)
        return server

    def next_event(self, broker, event_name):
        while True:
            message = broker.outbound.get(timeout=2)
            if message.get("event") == event_name:
                return message

    def completion_payload(self, forwarded, completion):
        payload = {key: value for key, value in forwarded.items()
                   if key not in {"expected_page", "owner_display", "connection_display_name"}}
        payload["completion"] = completion
        return payload

    def request(self, operation, payload):
        return {
            "version": 1, "id": operation, "service": "abush_browser",
            "operation": operation, "payload": payload,
        }

    def test_v6_handshake_is_rejected_by_exact_v7_peer(self):
        left, right = socket.socketpair()

        class ConnectedSocket:
            def settimeout(self, value):
                left.settimeout(value)

            def connect(self, _path):
                return None

            def sendall(self, value):
                left.sendall(value)

            def recv(self, size):
                return left.recv(size)

            def shutdown(self, how):
                left.shutdown(how)

            def close(self):
                left.close()

        peer = host.RemotePeer(mock.Mock(), {
            "remote_key": "r_12345678", "connection_id": "c_12345678",
            "browser_socket": "unused",
        })
        peer.broker.executor_id.return_value = "executor"

        def server():
            hello = host.read_peer_message(right)
            self.assertEqual(hello["version"], 7)
            right.sendall(host.encode_peer_message({
                "type": "hello_rejected", "request_id": "hello",
                "reason": "unsupported_version",
            }))

        worker = threading.Thread(target=server)
        worker.start()
        try:
            with mock.patch.object(host.os, "lstat", return_value=mock.Mock(
                    st_mode=host.stat.S_IFSOCK, st_uid=host.os.getuid())), \
                    mock.patch.object(host.socket, "socket",
                                      return_value=ConnectedSocket()):
                self.assertFalse(peer.connect())
        finally:
            right.close()
            worker.join(timeout=2)
            self.assertFalse(worker.is_alive())

    def test_v7_handshake_and_ensure_completion_are_strictly_correlated(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            hello = server.received[0]
            self.assertEqual(hello["version"], 7)
            self.assertEqual(server.received[1], {
                "type": "hello_ready", "request_id": "hello-request",
                "executor_fence": server.fence,
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
                "owner_display": server.owner_display,
                "connection_display_name": "r_12345678",
                "expected_page": None,
            })
            payload = {key: value for key, value in forwarded["payload"].items()
                       if key not in {"expected_page", "owner_display",
                                      "connection_display_name"}}
            payload["completion"] = {
                "outcome": "created", "window_id": "10", "page_id": "20",
            }
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

    def test_cancel_ensure_waits_for_cleanup_ack_and_retire_forwards_exact_identity(self):
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
            cancel = self.next_event(
                broker, "abush_browser.cancel_ensure_managed_page")["payload"]
            broker.handle_request(self.request(
                "ensure_managed_page_cancellation_completion", cancel))
            self.assertEqual(server.wait_for("ensure_managed_page_cancellation_completed"), {
                "type": "ensure_managed_page_cancellation_completed",
                "request_id": "ensure-request", "ensure_id": "ensure-one",
                "executor_fence": server.fence,
            })

            prior_cancellations = len([message for message in server.received
                                       if message.get("type") ==
                                       "ensure_managed_page_cancellation_completed"])
            server.send({
                "type": "ensure_managed_page", "request_id": "retire-request",
                "ensure_id": "ensure-retire", "workspace_id": "workspace-authoritative",
                "expected_page": None, "executor_fence": server.fence,
            })
            self.next_event(broker, "abush_browser.ensure_managed_page")
            server.send({
                "type": "cancel_ensure_managed_page", "request_id": "retire-request",
                "ensure_id": "ensure-retire", "executor_fence": server.fence,
            })
            self.next_event(broker, "abush_browser.cancel_ensure_managed_page")
            server.send({
                "type": "retire_managed_page", "request_id": "retire-cleanup-request",
                "retire_id": "stable-retire", "ensure_id": "ensure-retire",
                "executor_fence": server.fence, "origin_executor_fence": server.fence,
                "window_id": "window", "page_id": "page",
            })
            retire = self.next_event(broker, "abush_browser.retire_managed_page")["payload"]
            self.assertEqual(retire, {
                "remote_key": "r_12345678", "connection_id": "c_12345678",
                "principal_id": "principal-one", "request_id": "retire-cleanup-request",
                "retire_id": "stable-retire", "ensure_id": "ensure-retire",
                "executor_fence": server.fence, "origin_executor_fence": server.fence,
                "window_id": "window", "page_id": "page",
            })
            self.assertEqual(len([message for message in server.received
                                  if message.get("type") ==
                                  "ensure_managed_page_cancellation_completed"]),
                             prior_cancellations,
                             "retire must not reuse ensure cancellation completion")
        finally:
            broker.close()

    def test_retire_replay_updates_exact_host_command_fields(self):
        peer = host.RemotePeer(None, {
            "remote_key": "r_12345678", "connection_id": "c_12345678",
        })
        peer.principal_id = "principal-one"
        peer.executor_fence = {"executor_id": "executor", "epoch": 2}
        peer.broker = type("Broker", (), {"outbound": queue.Queue()})()
        base = {
            "type": "retire_managed_page", "request_id": "retire-old-command",
            "retire_id": "stable-retire", "ensure_id": "stable-ensure",
            "executor_fence": peer.executor_fence,
            "origin_executor_fence": {"executor_id": "origin", "epoch": 1},
            "window_id": "window", "page_id": "page",
        }
        peer._handle_retire_managed_page(base)
        replay = {**base, "request_id": "retire-new-command"}
        peer._handle_retire_managed_page(replay)
        forwarded = peer.broker.outbound.get_nowait()["payload"]
        forwarded = peer.broker.outbound.get_nowait()["payload"]
        self.assertEqual(forwarded, {
            "remote_key": "r_12345678", "connection_id": "c_12345678",
            "principal_id": "principal-one", "request_id": "retire-new-command",
            "retire_id": "stable-retire", "ensure_id": "stable-ensure",
            "executor_fence": peer.executor_fence,
            "origin_executor_fence": {"executor_id": "origin", "epoch": 1},
            "window_id": "window", "page_id": "page",
        })
        sent = []
        peer.send = sent.append
        with self.assertRaises(TimeoutError):
            peer.complete_retire(
                "retire-new-command", "stable-retire", peer.executor_fence,
                base["origin_executor_fence"], {"outcome": "already_retired"}, timeout=0.01)
        self.assertEqual(sent, [{
            "type": "retire_managed_page_completed",
            "request_id": "retire-new-command", "retire_id": "stable-retire",
            "executor_fence": peer.executor_fence,
            "origin_executor_fence": base["origin_executor_fence"],
            "completion": {"outcome": "already_retired"},
        }])

    def test_retire_completed_ack_and_consumed_use_canonical_v7_messages(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            server.send({
                "type": "retire_managed_page", "request_id": "retire-command-delivery",
                "retire_id": "stable-retire", "ensure_id": "ensure-retire",
                "executor_fence": server.fence, "origin_executor_fence": server.fence,
                "window_id": "window", "page_id": "page",
            })
            retire = self.next_event(broker, "abush_browser.retire_managed_page")["payload"]
            completion = {
                "remote_key": retire["remote_key"],
                "connection_id": retire["connection_id"],
                "principal_id": retire["principal_id"],
                "request_id": retire["request_id"],
                "retire_id": retire["retire_id"],
                "executor_fence": retire["executor_fence"],
                "origin_executor_fence": retire["origin_executor_fence"],
                "completion": {"outcome": "already_retired"},
            }
            result = {}

            def complete():
                result.update(broker.handle_request(
                    self.request("retire_managed_page_completion", completion)))

            worker = threading.Thread(target=complete)
            worker.start()
            self.assertEqual(server.wait_for("retire_managed_page_completed"), {
                "type": "retire_managed_page_completed",
                "request_id": "retire-command-delivery", "retire_id": "stable-retire",
                "executor_fence": server.fence, "origin_executor_fence": server.fence,
                "completion": {"outcome": "already_retired"},
            })
            server.send({
                "type": "retire_managed_page_ack",
                "request_id": "retire-command-delivery", "retire_id": "stable-retire",
                "executor_fence": server.fence, "response_delivery_id": "retire-response",
                "origin_executor_fence": server.fence,
                "completion": {"outcome": "already_retired"},
            })
            worker.join(timeout=2)
            self.assertFalse(worker.is_alive())
            self.assertEqual(result["response_delivery_id"], "retire-response")
            consumed = {
                "remote_key": retire["remote_key"],
                "connection_id": retire["connection_id"],
                "principal_id": retire["principal_id"],
                "request_id": "retire-consumed-request",
                "delivery_request_id": "retire-command-delivery",
                "retire_id": "stable-retire", "executor_fence": server.fence,
                "response_delivery_id": "retire-response",
            }
            confirmation = {}
            failure = []

            def consume():
                try:
                    confirmation.update(broker.handle_request(
                        self.request("retire_managed_page_ack_consumed", consumed)))
                except Exception as error:
                    failure.append(error)

            consumer = threading.Thread(target=consume)
            consumer.start()
            self.assertEqual(server.wait_for("retire_managed_page_ack_consumed"), {
                "type": "retire_managed_page_ack_consumed",
                "request_id": "retire-consumed-request",
                "delivery_request_id": "retire-command-delivery",
                "retire_id": "stable-retire", "executor_fence": server.fence,
                "response_delivery_id": "retire-response",
            })
            self.assertTrue(consumer.is_alive(), "native completion must wait for server ACK")
            server.send({
                "type": "retire_managed_page_ack_consumed_ack",
                "request_id": "retire-consumed-request", "retire_id": "stable-retire",
                "executor_fence": server.fence,
                "response_delivery_id": "retire-response",
            })
            consumer.join(timeout=2)
            self.assertFalse(consumer.is_alive())
            self.assertEqual(failure, [])
            self.assertEqual(confirmation, {
                "request_id": "retire-consumed-request", "retire_id": "stable-retire",
                "executor_fence": server.fence,
                "response_delivery_id": "retire-response",
            })
            for field in ("delivery_request_id", None):
                invalid = dict(consumed)
                if field is None:
                    invalid["unexpected"] = True
                else:
                    invalid.pop(field)
                with self.subTest(retire_consumed_field=field), self.assertRaises(ValueError):
                    broker.handle_request(
                        self.request("retire_managed_page_ack_consumed", invalid))
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
            broker.handle_request(self.request("ensure_managed_page_completion",
                                               self.completion_payload(payload, {
                "outcome": "created", "window_id": "window", "page_id": "page",
            })))
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
                    completion_payload = self.completion_payload(payload, completion)
                    completion_payload.update(change)
                    broker.handle_request(self.request(
                        "ensure_managed_page_completion", completion_payload))
            broker.handle_request(self.request("ensure_managed_page_completion",
                                               self.completion_payload(payload, completion)))
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

    def test_cancel_completion_ack_is_sent_only_after_extension_convergence(self):
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
            cancel = self.next_event(broker, "abush_browser.cancel")["payload"]
            self.assertEqual(cancel["execution_request_id"], "execute-request")
            broker.handle_request(self.request("cancellation_completion", {
                key: value for key, value in cancel.items() if key != "execution_request_id"
            } | {"completion": "unknown"}))
            self.assertEqual(server.wait_for("cancellation_completed"), {
                "type": "cancellation_completed", "request_id": "cancel-request",
                "effect_id": "effect", "executor_fence": server.fence,
                "completion": "unknown",
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
                broker.handle_request(self.request("ensure_managed_page_completion",
                                                   self.completion_payload(payload, {
                    "outcome": "rejected", "code": code,
                })))
            server.send({
                "type": "ensure_managed_page", "request_id": "invalid-request",
                "ensure_id": "invalid-ensure", "workspace_id": "workspace-invalid",
                "expected_page": None, "executor_fence": server.fence,
            })
            payload = self.next_event(broker, "abush_browser.ensure_managed_page")["payload"]
            with self.assertRaises(ValueError):
                broker.handle_request(self.request("ensure_managed_page_completion",
                                                   self.completion_payload(payload, {
                    "outcome": "rejected", "code": "workspace_unavailable",
                })))
        finally:
            broker.close()

    def test_workspace_executor_completion_shape_is_accepted_directly(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            server.send({
                "type": "ensure_managed_page", "request_id": "request-one",
                "ensure_id": "ensure-one", "workspace_id": "workspace-authoritative",
                "expected_page": None, "executor_fence": server.fence,
            })
            forwarded = self.next_event(broker, "abush_browser.ensure_managed_page")["payload"]
            extension_payload = {
                key: value for key, value in forwarded.items()
                if key not in {"expected_page", "owner_display", "connection_display_name"}
            }
            extension_payload["completion"] = {
                "outcome": "created", "window_id": "window", "page_id": "page",
            }
            broker.handle_request(self.request(
                "ensure_managed_page_completion", extension_payload))
            self.assertEqual(server.wait_for("ensure_managed_page_completed")["completion"],
                             extension_payload["completion"])
        finally:
            broker.close()

    def test_uncorrelated_completion_does_not_consume_pending_effect(self):
        peer = host.RemotePeer(None, {})
        peer.executor_fence = {"executor_id": "executor", "epoch": 1}
        peer.pending["effect"] = {"request_id": "right", "fence": peer.executor_fence}
        with self.assertRaises(ValueError):
            peer.complete("wrong", "effect", peer.executor_fence, "applied")
        self.assertIn("effect", peer.pending)

    def test_duplicate_effect_rejects_different_payload_without_second_delivery(self):
        broker = mock.Mock()
        broker.outbound = queue.Queue()
        peer = host.RemotePeer(broker, {
            "remote_key": "r_12345678", "connection_id": "c_12345678",
        })
        peer.principal_id = "principal"
        peer.executor_fence = {"executor_id": "executor", "epoch": 1}
        page = {
            "window": {"workspace_id": "workspace", "window_id": "window", "generation": 1},
            "page_id": "page", "generation": 1,
        }
        message = {
            "type": "execute", "request_id": "request", "effect_id": "effect",
            "executor_fence": peer.executor_fence,
            "command": {"command": "navigate", "page": page, "url": "https://one.test/"},
        }
        peer.handle(message)
        first = broker.outbound.get_nowait()
        self.assertEqual(first["payload"]["command"]["url"], "https://one.test/")
        peer.handle(json.loads(json.dumps(message)))
        with self.assertRaises(queue.Empty):
            broker.outbound.get_nowait()
        conflicting = json.loads(json.dumps(message))
        conflicting["command"]["url"] = "https://two.test/"
        with self.assertRaisesRegex(ValueError, "conflicting duplicate"):
            peer.handle(conflicting)
        with self.assertRaises(queue.Empty):
            broker.outbound.get_nowait()

    def test_owner_display_requires_exact_fields_and_matches_acceptance(self):
        peer = host.RemotePeer(None, {})
        prepared = {
            "type": "hello_prepared", "request_id": "request",
            "principal_id": "principal", "executor_fence": {
                "executor_id": "executor", "epoch": 1,
            },
            "owner_display": {
                "daemon_instance_id": "instance", "daemon_boot_id": "boot",
                "workbench_name": "workbench",
            },
        }
        peer._accept_prepared(prepared)
        peer._accept_hello({**prepared, "type": "hello_accepted"})
        with self.assertRaises(ValueError):
            peer._accept_hello({**prepared, "type": "hello_accepted", "owner_display": {
                **prepared["owner_display"], "workbench_name": "other",
            }})
        with self.assertRaises(ValueError):
            peer._accept_prepared({**prepared, "owner_display": {
                **prepared["owner_display"], "extra": "rejected",
            }})

    def test_positive_generations_and_typed_hello_rejection(self):
        peer = host.RemotePeer(None, {})
        page = {"window": {"workspace_id": "w", "window_id": "x", "generation": 1},
                "page_id": "p", "generation": 1}
        self.assertTrue(peer._valid_page_ref(page))
        for path in ("window", "page"):
            invalid = json.loads(json.dumps(page))
            if path == "window":
                invalid["window"]["generation"] = 0
            else:
                invalid["generation"] = 0
            self.assertFalse(peer._valid_page_ref(invalid))
        with self.assertRaisesRegex(ValueError, "unsupported_version"):
            peer._accept_hello({"type": "hello_rejected", "request_id": "hello",
                                "reason": "unsupported_version"})
        with self.assertRaisesRegex(ValueError, "invalid hello rejection"):
            peer._accept_hello({"type": "hello_rejected", "request_id": "hello",
                                "reason": "free form"})

    def test_same_principal_new_boot_failover_updates_pinned_owner(self):
        first = self.peer(connection_id="c_11111111")
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            replacement_owner = {
                "daemon_instance_id": "daemon-instance",
                "daemon_boot_id": "daemon-boot-two",
                "workbench_name": "renamed-workbench",
            }
            second = self.peer(connection_id="c_22222222", owner_display=replacement_owner)
            broker.refresh()
            self.next_event(broker, "abush_browser.remote_state")
            self.assertEqual(broker.principals["r_12345678"],
                             ("principal-one", first.owner_display))

            broker.peers["r_12345678"].close(emit=True)

            replacement = broker.peers["r_12345678"]
            self.assertEqual(replacement.owner_display, replacement_owner)
            self.assertEqual(broker.principals["r_12345678"],
                             ("principal-one", replacement_owner))
            state = self.next_event(broker, "abush_browser.remote_state")["payload"]
            self.assertEqual(state["owner_display"], replacement_owner)
            self.assertTrue(second.connected.wait(2))
        finally:
            broker.close()

    def test_failover_cannot_switch_pinned_daemon_instance(self):
        first = self.peer(connection_id="c_11111111")
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            pinned = ("principal-one", first.owner_display)
            second = self.peer(connection_id="c_22222222", owner_display={
                "daemon_instance_id": "other-instance",
                "daemon_boot_id": "daemon-boot-two",
                "workbench_name": "workbench",
            })
            broker.refresh()
            broker.peers["r_12345678"].close(emit=True)
            self.assertNotIn("r_12345678", broker.peers)
            self.assertTrue(second.done.wait(2))
            self.assertEqual(broker.principals["r_12345678"], pinned)
        finally:
            broker.close()

    def test_failover_cannot_switch_pinned_principal(self):
        first = self.peer(connection_id="c_11111111", principal="principal-one")
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            pinned = ("principal-one", first.owner_display)
            self.assertEqual(broker.principals["r_12345678"], pinned)
            second = self.peer(connection_id="c_22222222", principal="principal-two")
            broker.refresh()
            broker.peers["r_12345678"].close(emit=True)
            self.assertNotIn("r_12345678", broker.peers)
            self.assertTrue(second.done.wait(2))
            self.assertEqual(broker.principals["r_12345678"], pinned)
        finally:
            broker.close()

    def test_transport_timeout_forgets_request_and_retry_uses_new_delivery_id(self):
        broker = mock.Mock()
        peer = host.RemotePeer(broker, {
            "remote_key": "r_12345678", "connection_id": "c_12345678",
        })
        peer.executor_fence = {"executor_id": "executor", "epoch": 1}
        peer.topology_timeout = 0.01
        sent = []
        peer.send = sent.append
        page = {"window": {"workspace_id": "workspace", "window_id": "window",
                           "generation": 1}, "page_id": "page", "generation": 1}
        with self.assertRaises(TimeoutError):
            peer.report_managed_page_moved_outside(
                "delivery-one", "stable-report", peer.executor_fence,
                peer.executor_fence, page, timeout=0.01)
        self.assertNotIn("delivery-one", peer.topology_requests)
        with self.assertRaises(TimeoutError):
            peer.report_managed_page_moved_outside(
                "delivery-two", "stable-report", peer.executor_fence,
                peer.executor_fence, page, timeout=0.01)
        self.assertEqual([message["request_id"] for message in sent],
                         ["delivery-one", "delivery-two"])
        self.assertEqual({message["report_id"] for message in sent}, {"stable-report"})

    def test_consumed_ack_rejects_wrong_or_old_correlation(self):
        broker = mock.Mock()
        broker.outbound = queue.Queue()
        peer = host.RemotePeer(broker, {
            "remote_key": "r_12345678", "connection_id": "c_12345678",
        })
        peer.executor_fence = {"executor_id": "executor", "epoch": 2}
        state = {"identity": {
            "report_id": "stable-report", "fence": peer.executor_fence,
            "response_delivery_id": "delivery-current",
        }, "done": threading.Event(), "response": None}
        peer.topology_consumed_requests["consumed-current"] = state
        for change in (
            {"request_id": "consumed-old"},
            {"report_id": "other-report"},
            {"executor_fence": {"executor_id": "executor", "epoch": 1}},
            {"response_delivery_id": "delivery-old"},
        ):
            message = {
                "type": "topology_response_consumed_ack",
                "request_id": "consumed-current", "report_id": "stable-report",
                "executor_fence": peer.executor_fence,
                "response_delivery_id": "delivery-current", **change,
            }
            with self.subTest(change=change), self.assertRaises(ValueError):
                peer.handle(message)
            self.assertFalse(state["done"].is_set())
        peer.handle({
            "type": "topology_response_consumed_ack",
            "request_id": "consumed-current", "report_id": "stable-report",
            "executor_fence": peer.executor_fence,
            "response_delivery_id": "delivery-current",
        })
        self.assertTrue(state["done"].is_set())

    def test_managed_page_moved_outside_round_trip_is_correlated(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        page = {"window": {"workspace_id": "workspace", "window_id": "window",
                           "generation": 1}, "page_id": "page", "generation": 1}
        try:
            broker.refresh()
            payload = {"remote_key": "r_12345678", "connection_id": "c_12345678",
                       "principal_id": "principal-one", "request_id": "topology-one",
                       "report_id": "stable-topology-one",
                       "executor_fence": server.fence,
                       "origin_executor_fence": server.fence, "page": page}
            result = {}
            failure = []

            def report():
                try:
                    result.update(broker.handle_request(
                        self.request("managed_page_moved_outside", payload)))
                except Exception as error:
                    failure.append(error)

            first = threading.Thread(target=report)
            first.start()
            self.assertEqual(server.wait_for("managed_page_moved_outside"), {
                "type": "managed_page_moved_outside", "request_id": "topology-one",
                "report_id": "stable-topology-one", "executor_fence": server.fence,
                "origin_executor_fence": server.fence, "page": page,
            })
            server.send({
                "type": "topology_report_response", "request_id": "topology-one",
                "report_id": "stable-topology-one", "executor_fence": server.fence,
                "response_delivery_id": "topology-response",
                "result": {"outcome": "already_converged"},
            })
            first.join(timeout=2)
            self.assertFalse(first.is_alive())
            self.assertEqual(failure, [])
            self.assertEqual(result, {
                "request_id": "topology-one", "report_id": "stable-topology-one",
                "executor_fence": server.fence,
                "response_delivery_id": "topology-response",
                "result": {"outcome": "already_converged"},
            })
            consumed = {key: value for key, value in payload.items()
                        if key not in {"page", "origin_executor_fence"}}
            consumed["request_id"] = "topology-consumed-request"
            consumed["delivery_request_id"] = "topology-one"
            consumed["response_delivery_id"] = "topology-response"
            confirmation = {}
            failure.clear()

            def consume():
                try:
                    confirmation.update(broker.handle_request(
                        self.request("topology_response_consumed", consumed)))
                except Exception as error:
                    failure.append(error)

            consumer = threading.Thread(target=consume)
            consumer.start()
            self.assertEqual(server.wait_for("topology_response_consumed"), {
                "type": "topology_response_consumed", "request_id": "topology-consumed-request",
                "delivery_request_id": "topology-one", "report_id": "stable-topology-one",
                "executor_fence": server.fence, "response_delivery_id": "topology-response",
            })
            self.assertTrue(consumer.is_alive(), "native completion must wait for server ACK")
            server.send({
                "type": "topology_response_consumed_ack",
                "request_id": "topology-consumed-request", "report_id": "stable-topology-one",
                "executor_fence": server.fence, "response_delivery_id": "topology-response",
            })
            consumer.join(timeout=2)
            self.assertFalse(consumer.is_alive())
            self.assertEqual(failure, [])
            self.assertEqual(confirmation, {
                "request_id": "topology-consumed-request", "report_id": "stable-topology-one",
                "executor_fence": server.fence, "response_delivery_id": "topology-response",
            })
            for field in ("delivery_request_id", None):
                invalid = dict(consumed)
                if field is None:
                    invalid["unexpected"] = True
                else:
                    invalid.pop(field)
                with self.subTest(topology_consumed_field=field), self.assertRaises(ValueError):
                    broker.handle_request(self.request("topology_response_consumed", invalid))
        finally:
            broker.close()

    def test_topology_report_reauthorizes_rotated_fence_with_stable_identity(self):
        old = self.peer(connection_id="c_old00000", epoch=1)
        broker = host.BrowserBroker(self.root)
        page = {"window": {"workspace_id": "workspace", "window_id": "window",
                           "generation": 1}, "page_id": "page", "generation": 1}
        try:
            broker.refresh()
            broker.peers["r_12345678"].topology_timeout = 0.02
            original = {
                "remote_key": "r_12345678", "connection_id": "c_old00000",
                "principal_id": "principal-one", "request_id": "delivery-old",
                "report_id": "stable-report", "executor_fence": old.fence,
                "origin_executor_fence": old.fence, "page": page,
            }
            with self.assertRaises(TimeoutError):
                broker.handle_request(self.request("managed_page_moved_outside", original))
            self.assertEqual(old.wait_for("managed_page_moved_outside"), {
                "type": "managed_page_moved_outside", "request_id": "delivery-old",
                "report_id": "stable-report", "executor_fence": old.fence,
                "origin_executor_fence": old.fence, "page": page,
            })

            replacement = self.peer(connection_id="c_new00000", epoch=2,
                                    owner_display=old.owner_display)
            (self.root / "r_12345678" / "c_old00000" / "connection.json").unlink()
            broker.refresh()
            self.assertTrue(replacement.connected.wait(1))
            current = {
                **original,
                "connection_id": "c_new00000",
                "request_id": "delivery-new",
                "executor_fence": replacement.fence,
            }
            result = {}

            def report():
                result.update(broker.handle_request(
                    self.request("managed_page_moved_outside", current)))

            worker = threading.Thread(target=report)
            worker.start()
            self.assertEqual(replacement.wait_for("managed_page_moved_outside"), {
                "type": "managed_page_moved_outside", "request_id": "delivery-new",
                "report_id": "stable-report", "executor_fence": replacement.fence,
                "origin_executor_fence": old.fence, "page": page,
            })
            replacement.send({
                "type": "topology_report_response", "request_id": "delivery-new",
                "report_id": "stable-report", "executor_fence": replacement.fence,
                "response_delivery_id": "response-stable",
                "result": {"outcome": "applied"},
            })
            worker.join(timeout=2)
            self.assertFalse(worker.is_alive())
            self.assertEqual(result, {
                "request_id": "delivery-new",
                "report_id": "stable-report",
                "executor_fence": replacement.fence,
                "response_delivery_id": "response-stable",
                "result": {"outcome": "applied"},
            })
        finally:
            broker.close()

    def test_uncorrelated_durable_response_is_forwarded_for_extension_replay(self):
        broker = mock.Mock()
        broker.outbound = queue.Queue()
        peer = host.RemotePeer(broker, {
            "remote_key": "r_12345678", "connection_id": "c_12345678",
        })
        peer.principal_id = "principal"
        peer.executor_fence = {"executor_id": "executor", "epoch": 2}
        peer.handle({
            "type": "topology_report_response", "request_id": "replayed-delivery",
            "report_id": "stable-report", "executor_fence": peer.executor_fence,
            "response_delivery_id": "stable-response",
            "result": {"outcome": "already_converged"},
        })
        forwarded = broker.outbound.get_nowait()
        self.assertEqual(forwarded["event"], "abush_browser.topology_report_response")
        self.assertEqual(forwarded["payload"]["report_id"], "stable-report")
        self.assertEqual(forwarded["payload"]["request_id"], "replayed-delivery")
        self.assertEqual(forwarded["payload"]["executor_fence"], peer.executor_fence)

    def test_v6_topology_messages_and_old_fence_response_are_rejected(self):
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
                "outcome": "page_moved_outside",
            }})
        peer.topology_requests["delivery"] = {
            "identity": {"report_id": "report", "fence": peer.executor_fence},
            "done": threading.Event(), "response": None,
        }
        with self.assertRaises(ValueError):
            peer.handle({
                "type": "topology_report_response", "request_id": "delivery",
                "report_id": "report",
                "executor_fence": {"executor_id": "executor", "epoch": 2},
                "response_delivery_id": "response", "result": {"outcome": "applied"},
            })


if __name__ == "__main__":
    unittest.main()
