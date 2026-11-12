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

    def test_v6_handshake_and_ensure_completion_are_strictly_correlated(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        try:
            broker.refresh()
            hello = server.received[0]
            self.assertEqual(hello["version"], 6)
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
                "ensure_id": "ensure-retire", "executor_fence": server.fence,
                "window_id": "window", "page_id": "page",
            })
            retire = self.next_event(broker, "abush_browser.retire_managed_page")["payload"]
            self.assertEqual(retire, {
                "remote_key": "r_12345678", "connection_id": "c_12345678",
                "principal_id": "principal-one", "request_id": "retire-cleanup-request",
                "ensure_id": "ensure-retire", "executor_fence": server.fence,
                "window_id": "window", "page_id": "page",
            })
            broker.handle_request(self.request(
                "ensure_managed_page_cancellation_completion", {
                    key: value for key, value in retire.items()
                    if key not in {"window_id", "page_id"}
                }))
            self.assertEqual(
                server.wait_for("ensure_managed_page_cancellation_completed",
                                request_id="retire-cleanup-request")["request_id"],
                "retire-cleanup-request")
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

    def test_managed_page_moved_outside_round_trip_is_correlated(self):
        server = self.peer()
        broker = host.BrowserBroker(self.root)
        page = {"window": {"workspace_id": "workspace", "window_id": "window",
                           "generation": 1}, "page_id": "page", "generation": 1}
        try:
            broker.refresh()
            payload = {"remote_key": "r_12345678", "connection_id": "c_12345678",
                       "principal_id": "principal-one", "request_id": "topology-one",
                       "executor_fence": server.fence, "page": page}
            broker.handle_request(self.request("managed_page_moved_outside", payload))
            self.assertEqual(server.wait_for("managed_page_moved_outside"), {
                "type": "managed_page_moved_outside", "request_id": "topology-one",
                "executor_fence": server.fence, "page": page,
            })
            server.send({"type": "topology_response", "request_id": "topology-one",
                         "result": {"outcome": "page_moved_outside"}})
            for _ in range(100):
                if "topology-one" not in broker.peers["r_12345678"].topology_pending:
                    break
                threading.Event().wait(0.01)
            self.assertNotIn("topology-one", broker.peers["r_12345678"].topology_pending)
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
