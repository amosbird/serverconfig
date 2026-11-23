#!/usr/bin/env python3

import importlib.util
import json
import pathlib
import queue
import socket
import threading
import time
import unittest
from tempfile import TemporaryDirectory
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("browser_peer_host", ROOT / "chromium/external-links/host.py")
host = importlib.util.module_from_spec(spec)
spec.loader.exec_module(host)

BOOT = {"pid": 42, "starttime": "123", "profile": pathlib.Path("/profiles/main"),
        "browser_boot_id": "chrome-current"}
OWNER = {"daemon_instance_id": "instance", "daemon_boot_id": "boot", "workbench_name": "workbench"}
PAGE = {"workspace_id": "workspace", "page_id": "page", "generation": 1}
AUTH = {"browser_boot_id": BOOT["browser_boot_id"], "enable_id": "enable", "revision": 1}


def clone(value):
    return json.loads(json.dumps(value))


CAPABILITIES = {"cancellation": True, "owned_tab_group": True,
                "official_engine": True, "typed_output": True}


def prepare_ready(broker):
    prepared = broker.prepare_executor({"browser_boot_id": broker.browser["browser_boot_id"],
                                       "protocol_version": 10})
    payload = {key: value for key, value in prepared.items() if key != "primary_profile"}
    payload["capabilities"] = clone(CAPABILITIES)
    broker.confirm_executor_ready(payload, lambda result: broker.outbound.put(result))
    broker.outbound.get(timeout=1)
    return payload


class FakePeer:
    def __init__(self, path, principal="principal", epoch=1, owner=None, handshake=None):
        self.principal = principal
        self.owner = owner or OWNER
        self.fence = {"executor_id": f"executor-{path.parent.name}", "epoch": epoch}
        self.handshake = handshake
        self.accept = True
        self.received = []
        self.condition = threading.Condition()
        self.stopping = threading.Event()
        self.connected = threading.Event()
        self.client = None
        self.write_lock = threading.Lock()
        self.server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.server.bind(str(path))
        self.server.listen(1)
        self.server.settimeout(0.2)
        self.thread = threading.Thread(target=self.run, daemon=True)
        self.thread.start()

    def record(self, message):
        with self.condition:
            self.received.append(message)
            self.condition.notify_all()

    def run(self):
        try:
            while not self.stopping.is_set():
                try:
                    self.client, _ = self.server.accept()
                    break
                except socket.timeout:
                    continue
            if self.client is None:
                return
            self.client.settimeout(2)
            hello = host.read_peer_message(self.client)
            self.record(hello)
            if hello["version"] != 10 or hello["capabilities"] != {
                    "cancellation": True, "owned_tab_group": True}:
                self.send({"type": "hello_rejected", "request_id": "hello",
                           "reason": "unsupported_version"})
                return
            prepared = {"type": "hello_prepared", "request_id": "hello", "principal_id": self.principal,
                        "workspace_id": "workspace", "executor_fence": self.fence,
                        "owner_display": self.owner}
            accepted = {**prepared, "type": "hello_accepted"}
            if self.handshake:
                prepared, accepted = self.handshake(prepared, accepted)
            self.send(prepared)
            self.record(host.read_peer_message(self.client))
            self.send(accepted)
            self.connected.set()
            self.client.settimeout(None)
            while not self.stopping.is_set():
                message = host.read_peer_message(self.client)
                self.record(message)
                receipt = host.COMPLETION_RECEIPTS.get(message["type"])
                if message["type"] == "workspace_state":
                    receipt = "workspace_state_accepted"
                if self.accept and receipt:
                    self.send({**message, "type": receipt})
        except (OSError, EOFError):
            pass

    def send(self, message):
        with self.write_lock:
            self.client.sendall(host.encode_peer_message(message))

    def wait_for(self, kind, request_id=None):
        deadline = time.monotonic() + 2
        with self.condition:
            while True:
                found = next((message for message in self.received if message["type"] == kind and
                              (request_id is None or message.get("request_id") == request_id)), None)
                if found:
                    return found
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise AssertionError(f"missing {kind}: {self.received}")
                self.condition.wait(remaining)

    def close(self):
        self.stopping.set()
        if self.client:
            try:
                self.client.shutdown(socket.SHUT_RDWR)
            except OSError:
                pass
            self.client.close()
        self.server.close()
        self.thread.join(timeout=2)
        if self.thread.is_alive():
            raise AssertionError("fixture reader did not stop")


class BrowserPeerBrokerTest(unittest.TestCase):
    def setUp(self):
        common = ROOT.parent / ".tmp"
        common.mkdir(parents=True, exist_ok=True)
        self.temporary = TemporaryDirectory(prefix="", dir=common)
        self.root = pathlib.Path(self.temporary.name) / "r"
        self.root.mkdir(mode=0o700)
        self.servers = []
        self.brokers = []

    def tearDown(self):
        for broker in self.brokers:
            broker.close()
        for server in self.servers:
            server.close()
        self.temporary.cleanup()

    def server(self, connection="c_12345678", remote="r_12345678", **kwargs):
        directory = self.root / remote / connection
        directory.parent.mkdir(mode=0o700, exist_ok=True)
        directory.mkdir(mode=0o700)
        path = directory / "connection.json"
        path.write_text(json.dumps({"schema_version": 1, "remote_key": remote,
            "connection_id": connection, "display_name": remote,
            "browser_socket": str(directory / "browser-peer.sock"),
            "remote_browser_socket": "/run/user/1000/abush/browser-peer.sock"}))
        path.chmod(0o600)
        server = FakePeer(directory / "browser-peer.sock", **kwargs)
        self.servers.append(server)
        return server

    def broker(self):
        broker = host.BrowserBroker(self.root, browser=BOOT, primary_profile=BOOT["profile"])
        prepare_ready(broker)
        self.brokers.append(broker)
        return broker

    def connected(self, **kwargs):
        server = self.server(**kwargs)
        broker = self.broker()
        broker.refresh()
        return broker, broker.peers["r_12345678"], server

    def request(self, operation, payload):
        return {"version": 1, "id": operation, "service": "abush_browser",
                "operation": operation, "payload": payload}

    def identity(self, peer):
        return {"remote_key": peer.metadata["remote_key"],
                "connection_id": peer.metadata["connection_id"], "principal_id": peer.principal_id}

    def state(self, phase="enabled", revision=1):
        return {"browser_boot_id": BOOT["browser_boot_id"], "revision": revision, "phase": phase,
                "binding": None if phase == "disabled" else {"enable_id": "enable", "page": clone(PAGE)}}

    def state_payload(self, peer, **changes):
        return {**self.identity(peer), "request_id": "state", "executor_fence": peer.executor_fence,
                "state": self.state(), **changes}

    def execute(self, peer, **changes):
        return {"type": "execute", "request_id": "execute", "effect_id": "effect",
                "executor_fence": peer.executor_fence, "authorization": clone(AUTH),
                "command": {"command": "navigate", "page": clone(PAGE), "url": "https://example.test/"},
                **changes}

    def terminal(self, peer, **changes):
        completion = changes.get("completion", "applied")
        output = {"result": {"text": "original result", "data": {"value": 1}, "images": []}, "error": None} if completion == "applied" else {"result": None, "error": "original error"}
        return {**self.identity(peer), "request_id": "execute", "effect_id": "effect",
                "executor_fence": peer.executor_fence, "completion": "applied", **output, **changes}

    def event(self, broker, name):
        deadline = time.monotonic() + 2
        while True:
            message = broker.outbound.get(timeout=max(0.001, deadline - time.monotonic()))
            if message.get("event") == name:
                return message["payload"]
            if time.monotonic() >= deadline:
                self.fail(f"missing event {name}")

    def barrier(self, server, request="barrier"):
        server.send({"type": "ping", "request_id": request, "ping_id": request})
        server.wait_for("pong", request)

    def native(self):
        native = host.NativeBrokerHost(input_stream=mock.Mock(), output_stream=mock.Mock(),
            remote_root=self.root, ordinary_open_factory=None, browser=BOOT,
            primary_profile=BOOT["profile"])
        prepare_ready(native.broker)
        native.broker.write_barrier = None
        self.brokers.append(native.broker)
        return native

    def raw_native(self, browser=BOOT, primary_profile=None):
        native = host.NativeBrokerHost(input_stream=mock.Mock(), output_stream=mock.Mock(),
            remote_root=self.root, ordinary_open_factory=None, browser=browser,
            primary_profile=primary_profile if primary_profile is not None else BOOT["profile"])
        native.broker.write_barrier = None
        self.brokers.append(native.broker)
        return native

    def native_request(self, native, operation, payload):
        native._respond({"version": 1, "id": operation, "service": "native_host",
                         "operation": operation, "payload": payload})
        return native.outbound.get(timeout=1)

    def prepared_payload(self, native):
        result = self.native_request(native, "browser_executor_prepare", {
            "browser_boot_id": BOOT["browser_boot_id"], "protocol_version": 10})
        self.assertTrue(result["ok"], result)
        prepared = result["payload"]
        self.assertEqual(set(prepared), {"browser_boot_id", "native_connection_id",
                                        "protocol_version", "primary_profile"})
        return {key: value for key, value in prepared.items() if key != "primary_profile"} | {
            "capabilities": clone(CAPABILITIES)}

    def test_prepare_alone_and_bootstrap_never_authorize(self):
        native = self.raw_native()
        with mock.patch.object(host, "discover_remotes") as discovery:
            bootstrap = self.native_request(native, "bootstrap", {})
            self.assertEqual(set(bootstrap["payload"]), {"browser_boot_id", "browser_pid",
                             "browser_starttime", "ordinary_launch_leases"})
            self.prepared_payload(native)
            self.assertEqual(native.broker.refresh(), [])
            self.assertFalse(native.refresh_wakeup.is_set())
            discovery.assert_not_called()
        with mock.patch.object(host.socket, "socket") as socket_factory:
            with self.assertRaisesRegex(ValueError, "readiness"):
                host.RemotePeer(native.broker, {}).connect()
            socket_factory.assert_not_called()

    def test_malformed_prepare_and_ready_never_admit(self):
        native = self.raw_native()
        with mock.patch.object(host, "discover_remotes") as discovery:
            for payload in ({}, {"browser_boot_id": BOOT["browser_boot_id"]},
                    {"browser_boot_id": "old", "protocol_version": 10},
                    {"browser_boot_id": BOOT["browser_boot_id"], "protocol_version": 9},
                    {"browser_boot_id": BOOT["browser_boot_id"], "protocol_version": True},
                    {"browser_boot_id": BOOT["browser_boot_id"], "protocol_version": 10, "extra": 1}):
                self.assertFalse(self.native_request(native, "browser_executor_prepare", payload)["ok"])
            self.assertFalse(native.broker.executor_prepared)
            payload = self.prepared_payload(native)
            invalid = [{}, {key: value for key, value in payload.items() if key != "capabilities"},
                {**payload, "protocol_version": 9}, {**payload, "protocol_version": True},
                {**payload, "browser_boot_id": "old"}, {**payload, "native_connection_id": "old"},
                {**payload, "extra": True}, {**payload, "capabilities": {}},
                {**payload, "capabilities": {**CAPABILITIES, "extra": True}}]
            for key in CAPABILITIES:
                invalid.extend([{**payload, "capabilities": {
                                    field: value for field, value in CAPABILITIES.items()
                                    if field != key}},
                                {**payload, "capabilities": {**CAPABILITIES, key: False}},
                                {**payload, "capabilities": {**CAPABILITIES, key: 1}}])
            for value in invalid:
                with self.subTest(value=value):
                    self.assertFalse(self.native_request(native, "browser_executor_ready", value)["ok"])
                    self.assertEqual(native.broker.refresh(), [])
                    self.assertFalse(native.broker.executor_ready)
            discovery.assert_not_called()

    def test_exact_ready_response_precedes_hello_and_activation(self):
        native = self.raw_native()
        server = self.server()
        payload = self.prepared_payload(native)
        original_put = native.outbound.put
        queued = threading.Event()
        release = threading.Event()
        trace = []
        failures = []
        def publish(message, **kwargs):
            original_put(message, **kwargs)
            if message.get("reply_to") == "browser_executor_ready":
                trace.append("ready_queued")
                queued.set()
                if not release.wait(2):
                    raise AssertionError("ready publication release deadline")
        def refresh():
            if not queued.wait(2):
                failures.append("ready queue deadline")
                return
            native.broker.refresh()
            trace.append("refresh_completed")
        worker = threading.Thread(target=refresh, daemon=True)
        ready_worker = threading.Thread(target=native._respond, args=({"version": 1,
            "id": "browser_executor_ready", "service": "native_host",
            "operation": "browser_executor_ready", "payload": payload},), daemon=True)
        with mock.patch.object(native.outbound, "put", side_effect=publish):
            worker.start()
            ready_worker.start()
            try:
                self.assertTrue(queued.wait(2))
                self.assertEqual(server.received, [])
                self.assertFalse(native.broker.executor_ready)
                result = native.outbound.get(timeout=1)
                self.assertEqual(result["payload"], {key: value for key, value in payload.items()
                                 if key != "capabilities"} | {"ready": True})
            finally:
                release.set()
                ready_worker.join(timeout=3)
                worker.join(timeout=3)
        self.assertFalse(worker.is_alive())
        self.assertFalse(ready_worker.is_alive())
        self.assertEqual(failures, [])
        self.assertEqual(trace, ["ready_queued", "refresh_completed"])
        self.assertEqual(server.wait_for("hello")["version"], 10)
        self.assertTrue(native.refresh_wakeup.is_set())

    def test_ready_queue_failure_does_not_admit(self):
        native = self.raw_native()
        payload = self.prepared_payload(native)
        with mock.patch.object(native.outbound, "put", side_effect=queue.Full), \
                mock.patch.object(host, "discover_remotes") as discovery:
            native._respond({"version": 1, "id": "ready", "service": "native_host",
                             "operation": "browser_executor_ready", "payload": payload})
            self.assertFalse(native.broker.executor_ready)
            self.assertEqual(native.broker.refresh(), [])
            discovery.assert_not_called()
        self.assertIsInstance(native.fatal_error, queue.Full)

    def test_secondary_same_principal_both_orders_never_replaces_primary(self):
        for secondary_first in (True, False):
            with self.subTest(secondary_first=secondary_first):
                remote_key = "r_12345678" if secondary_first else "r_87654321"
                server = self.server(remote=remote_key)
                primary = self.raw_native()
                secondary = self.raw_native({**BOOT, "profile": pathlib.Path("/profiles/other")})
                secondary_payload = self.prepared_payload(secondary)
                primary_payload = self.prepared_payload(primary)
                order = [(secondary, secondary_payload), (primary, primary_payload)]
                if not secondary_first:
                    order.reverse()
                for native, payload in order:
                    result = self.native_request(native, "browser_executor_ready", payload)
                    self.assertEqual(result["ok"], native is primary)
                    native.broker.refresh()
                remote = server.wait_for("hello")
                peer = primary.broker.peers[remote_key]
                peer.handle(self.execute(peer))
                pending = clone(peer.pending)
                fence = clone(peer.executor_fence)
                for _ in range(2):
                    self.assertEqual(secondary.broker.refresh(), [])
                    secondary.broker.remote_disconnected(peer)
                    self.assertFalse(self.native_request(secondary, "browser_executor_ready",
                                                       secondary_payload)["ok"])
                reconstructed = self.raw_native({**BOOT,
                    "profile": pathlib.Path("/profiles/other")})
                reconstructed_payload = self.prepared_payload(reconstructed)
                self.assertFalse(self.native_request(reconstructed, "browser_executor_ready",
                                                     reconstructed_payload)["ok"])
                self.assertEqual(reconstructed.broker.refresh(), [])
                with mock.patch.object(host.socket, "socket") as socket_factory:
                    with self.assertRaisesRegex(ValueError, "primary"):
                        host.RemotePeer(reconstructed.broker, {}).connect()
                    socket_factory.assert_not_called()
                self.assertEqual(peer.pending, pending)
                self.assertEqual(peer.executor_fence, fence)
                self.assertEqual(len([m for m in server.received if m["type"] == "hello"]), 1)
                self.assertEqual(remote["version"], 10)
                primary.broker.close()
                server.close()
                self.servers.remove(server)
                (self.root / remote_key / "c_12345678" / "connection.json").unlink()

    def test_secondary_only_retains_external_routing_and_explicit_errors(self):
        native = self.raw_native({**BOOT, "profile": pathlib.Path("/profiles/other")})
        with mock.patch.object(host, "discover_remotes") as discovery, \
                mock.patch.object(host, "handle_message", return_value={"ok": True}) as ordinary:
            self.assertEqual(native.broker.refresh(), [])
            for operation in ("refresh_remotes", "list_remotes", "engine_open", "workspace_state"):
                with self.assertRaisesRegex(ValueError, "primary"):
                    native.broker.handle_request(self.request(operation, {}))
            for operation, payload in (("open", {"url": "https://example.test/"}),
                                       ("scroll", {"direction": "down"})):
                native.broker.handle_request({"version": 1, "id": operation,
                    "service": "external_links", "operation": operation, "payload": payload})
            self.assertEqual(ordinary.call_count, 2)
            discovery.assert_not_called()

    def test_unknown_browser_preserves_bootstrap_and_periodic_refresh_is_blocked(self):
        ordinary_factory = mock.Mock()
        with mock.patch.object(host, "native_host_browser", return_value=None):
            native = host.NativeBrokerHost(input_stream=mock.Mock(), output_stream=mock.Mock(),
                ordinary_open_factory=ordinary_factory, primary_profile=BOOT["profile"])
        self.brokers.append(native.broker)
        ordinary_factory.assert_not_called()
        bootstrap = self.native_request(native, "bootstrap", {})
        self.assertTrue(bootstrap["ok"])
        self.assertEqual(bootstrap["payload"]["browser_pid"], 0)
        with mock.patch.object(host, "discover_remotes") as discovery, \
                mock.patch.object(native.refresh_wakeup, "wait",
                                  side_effect=lambda interval: native.refresh_stop.set()):
            native._refresh_loop()
            discovery.assert_not_called()
        self.assertFalse(native.broker.executor_ready)

    def test_ready_without_prepare_never_authorizes(self):
        native = self.raw_native()
        payload = {"browser_boot_id": BOOT["browser_boot_id"], "protocol_version": 10,
                   "native_connection_id": native.broker.native_connection_id,
                   "capabilities": clone(CAPABILITIES)}
        self.assertFalse(self.native_request(native, "browser_executor_ready", payload)["ok"])
        self.assertFalse(native.broker.executor_ready)
        self.assertEqual(native.broker.refresh(), [])

    def test_ready_duplicate_and_reconstruction_tokens_and_closed_broker(self):
        native = self.raw_native()
        payload = self.prepared_payload(native)
        first = self.native_request(native, "browser_executor_ready", payload)
        self.assertEqual(self.native_request(native, "browser_executor_ready", payload), first)
        newer = self.raw_native()
        newer_payload = self.prepared_payload(newer)
        self.assertNotEqual(payload["native_connection_id"], newer_payload["native_connection_id"])
        self.assertFalse(self.native_request(newer, "browser_executor_ready", payload)["ok"])
        native.broker.close()
        self.assertFalse(self.native_request(native, "browser_executor_ready", payload)["ok"])
        self.assertFalse(self.native_request(native, "browser_executor_prepare", {
            "browser_boot_id": BOOT["browser_boot_id"], "protocol_version": 10})["ok"])
        with mock.patch.object(host, "discover_remotes") as discovery:
            self.assertEqual(native.broker.refresh(), [])
            with self.assertRaisesRegex(ValueError, "closed"):
                native.broker.peer({})
            discovery.assert_not_called()

    def test_primary_profile_environment_canonical_path_and_resolved_once(self):
        profile = pathlib.Path(self.temporary.name) / "primary"
        profile.mkdir()
        link = profile.parent / "alias"
        link.symlink_to(profile, target_is_directory=True)
        with mock.patch.dict(host.os.environ, {"ABUSH_CHROME_PROFILE": str(link)}):
            native = host.NativeBrokerHost(input_stream=mock.Mock(), output_stream=mock.Mock(),
                ordinary_open_factory=None, browser={**BOOT, "profile": profile})
            self.brokers.append(native.broker)
            self.assertTrue(native.broker.is_primary_profile)
            host.os.environ["ABUSH_CHROME_PROFILE"] = "/profiles/changed"
            self.assertEqual(native.broker.primary_profile, profile)
            self.assertEqual(native.primary_profile, profile)
        override = self.raw_native({**BOOT, "profile": link}, primary_profile=profile)
        self.assertTrue(override.broker.is_primary_profile)
        with self.assertRaisesRegex(ValueError, "absolute"):
            host.BrowserBroker(browser=BOOT, primary_profile="relative")
        unknown = host.BrowserBroker(primary_profile=profile)
        self.brokers.append(unknown)
        self.assertEqual(unknown.refresh(), [])
        with self.assertRaisesRegex(ValueError, "primary"):
            unknown.peer({})

    def test_unprepared_primary_refresh_never_discovers_or_connects(self):
        server = self.server()
        broker = host.BrowserBroker(self.root, browser=BOOT, primary_profile=BOOT["profile"])
        self.brokers.append(broker)
        with mock.patch.object(host, "discover_remotes", wraps=host.discover_remotes) as discover:
            self.assertEqual(broker.refresh(), [])
            discover.assert_not_called()
        self.assertEqual(server.received, [])

    def test_unprepared_secondary_refresh_never_discovers_or_connects(self):
        server = self.server()
        broker = host.BrowserBroker(self.root, browser={**BOOT,
            "profile": pathlib.Path("/profiles/secondary")}, primary_profile=BOOT["profile"])
        self.brokers.append(broker)
        with mock.patch.object(host, "discover_remotes", wraps=host.discover_remotes) as discover:
            self.assertEqual(broker.refresh(), [])
            discover.assert_not_called()
        self.assertEqual(server.received, [])

    def test_v10_exact_handshake_and_capabilities(self):
        broker, peer, server = self.connected()
        hello = server.wait_for("hello")
        self.assertEqual(set(hello), {"type", "version", "executor_id", "nonce", "capabilities"})
        self.assertEqual(hello["version"], 10)
        self.assertEqual(hello["capabilities"], {"cancellation": True, "owned_tab_group": True})
        self.assertEqual(server.wait_for("hello_ready"), {"type": "hello_ready", "request_id": "hello",
                                                        "executor_fence": peer.executor_fence})
        self.assertEqual(broker.list_remotes()[0]["workspace_id"], "workspace")

    def test_v8_rejected(self):
        server = self.server()
        broker = self.broker()
        with mock.patch.object(host, "PEER_PROTOCOL_VERSION", 8):
            self.assertFalse(broker.refresh()[0]["connected"])
        self.assertEqual(server.wait_for("hello")["version"], 8)

    def test_mandatory_owned_group_capability(self):
        broker, peer, server = self.connected()
        self.assertIs(server.wait_for("hello")["capabilities"]["owned_tab_group"], True)
        self.assertFalse(peer.workspace_requests, "transport connection does not enable workspace")

    def test_handshake_workspace_required(self):
        self.server(handshake=lambda p, a: ({key: value for key, value in p.items()
                                            if key != "workspace_id"}, a))
        self.assertFalse(self.broker().refresh()[0]["connected"])

    def test_handshake_extra_fields_rejected(self):
        self.server(handshake=lambda p, a: ({**p, "extra": True}, a))
        self.assertFalse(self.broker().refresh()[0]["connected"])

    def test_handshake_acceptance_identity_immutable(self):
        for field, value in (("request_id", "other"), ("workspace_id", "other"),
                             ("principal_id", "other"), ("executor_fence", {"executor_id": "e", "epoch": 9}),
                             ("owner_display", {**OWNER, "workbench_name": "other"})):
            with self.subTest(field=field):
                peer = host.RemotePeer(self.broker(), {})
                prepared = {"type": "hello_prepared", "request_id": "h", "workspace_id": "w",
                            "principal_id": "p", "executor_fence": {"executor_id": "e", "epoch": 1},
                            "owner_display": OWNER}
                peer._accept_prepared(prepared)
                with self.assertRaises(ValueError):
                    peer._accept_hello({**prepared, "type": "hello_accepted", field: value})

    def test_workspace_state_exact_receipt(self):
        broker, peer, server = self.connected()
        payload = self.state_payload(peer)
        result = broker.handle_request(self.request("workspace_state", payload))
        sent = server.wait_for("workspace_state")
        self.assertEqual(sent, {"type": "workspace_state", "request_id": "state",
                               "executor_fence": peer.executor_fence, "state": payload["state"]})
        self.assertEqual(result, {**sent, "type": "workspace_state_accepted"})

    def test_workspace_disabled_and_paused(self):
        broker, peer, server = self.connected()
        for phase in ("disabled", "paused"):
            value = self.state(phase)
            result = peer.workspace_state(phase, peer.executor_fence, value)
            self.assertEqual(result["state"], value)

    def test_workspace_state_current_authority(self):
        broker, peer, server = self.connected()
        for change in ({"principal_id": "other"}, {"connection_id": "c_99999999"},
                       {"executor_fence": {**peer.executor_fence, "epoch": 99}}):
            with self.subTest(change=change), self.assertRaises(ValueError):
                broker.handle_request(self.request("workspace_state", self.state_payload(peer, **change)))
        self.assertFalse(peer.workspace_requests)

    def test_workspace_state_browser_boot(self):
        broker, peer, server = self.connected()
        with self.assertRaises(ValueError):
            peer.workspace_state("state", peer.executor_fence, {**self.state(), "browser_boot_id": "old"})
        self.assertFalse(peer.workspace_requests)

    def test_workspace_state_workspace_match(self):
        broker, peer, server = self.connected()
        value = self.state()
        value["binding"]["page"]["workspace_id"] = "other"
        with self.assertRaises(ValueError):
            peer.workspace_state("state", peer.executor_fence, value)

    def test_workspace_state_strict_fields(self):
        broker, peer, server = self.connected()
        value = self.state()
        for change in ({"revision": True}, {"revision": 0}, {"revision": 2 ** 64},
                       {"phase": "remote"}, {"binding": {"enable_id": "", "page": PAGE}},
                       {"binding": None}, {"extra": True}):
            with self.subTest(change=change), self.assertRaises(ValueError):
                peer.workspace_state("state", peer.executor_fence, {**value, **change})

    def test_workspace_native_payload_exact(self):
        broker, peer, server = self.connected()
        payload = self.state_payload(peer)
        for field in (*payload, "extra"):
            invalid = {**payload, "extra": 1} if field == "extra" else {
                key: value for key, value in payload.items() if key != field}
            with self.subTest(field=field), self.assertRaises(ValueError):
                broker.handle_request(self.request("workspace_state", invalid))

    def test_workspace_duplicate_exact_and_conflict(self):
        broker, peer, server = self.connected()
        args = ("state", peer.executor_fence, self.state())
        result = peer.workspace_state(*args)
        self.assertEqual(peer.workspace_state(*args), result)
        with self.assertRaises(ValueError):
            peer.workspace_state("state", peer.executor_fence, self.state(revision=2))
        self.assertEqual(sum(m["type"] == "workspace_state" for m in server.received), 1)

    def test_workspace_ack_loss_and_late_ack(self):
        broker, peer, server = self.connected()
        server.accept = False
        peer.workspace_timeout = 0
        args = ("state", peer.executor_fence, self.state())
        with self.assertRaises(TimeoutError):
            peer.workspace_state(*args)
        sent = server.wait_for("workspace_state")
        receipt = {**sent, "type": "workspace_state_accepted"}
        server.send(receipt)
        self.barrier(server)
        self.assertEqual(peer.workspace_state(*args), receipt)
        server.send(receipt)
        self.barrier(server, "duplicate")
        self.assertTrue(peer.connected)

    def test_workspace_wrong_receipt_does_not_close_peer(self):
        broker, peer, server = self.connected()
        server.accept = False
        peer.workspace_timeout = 0
        with self.assertRaises(TimeoutError):
            peer.workspace_state("state", peer.executor_fence, self.state())
        sent = server.wait_for("workspace_state")
        receipt = {**sent, "type": "workspace_state_accepted"}
        for change in ({"request_id": "wrong"}, {"executor_fence": {**peer.executor_fence, "epoch": 99}},
                       {"state": self.state(revision=2)}, {"extra": True}):
            server.send({**receipt, **change})
        self.barrier(server)
        self.assertIsNone(peer.workspace_requests["state"]["response"])
        self.assertTrue(peer.connected)

    def test_workspace_ambiguous_write_retains_correlation(self):
        broker, peer, server = self.connected()
        args = ("state", peer.executor_fence, self.state())
        with mock.patch.object(peer, "send", side_effect=OSError("write failed")):
            with self.assertRaises(OSError):
                peer.workspace_state(*args)
        self.assertIn("state", peer.workspace_requests)
        self.assertEqual(peer.workspace_state(*args)["state"], self.state())

    def test_workspace_reader_not_locked_during_send(self):
        broker, peer, server = self.connected()
        writing, release = threading.Event(), threading.Event()
        result = []
        original = peer.send
        def send(message):
            original(message)
            writing.set()
            if not release.wait(2):
                raise TimeoutError("send barrier")
        def worker():
            try:
                result.append(peer.workspace_state("state", peer.executor_fence, self.state()))
            except Exception as error:
                result.append(error)
        with mock.patch.object(peer, "send", side_effect=send):
            thread = threading.Thread(target=worker, daemon=True)
            thread.start()
            try:
                self.assertTrue(writing.wait(1))
                self.assertTrue(peer.workspace_requests["state"]["done"].wait(1))
            finally:
                release.set()
                thread.join(timeout=2)
        self.assertFalse(thread.is_alive())
        self.assertEqual(result[0]["type"], "workspace_state_accepted")

    def test_workspace_unacked_backlog_not_evicted(self):
        broker, peer, server = self.connected()
        peer.workspace_timeout = 0
        with mock.patch.object(peer, "send"):
            for index in range(256):
                with self.assertRaises(TimeoutError):
                    peer.workspace_state(str(index), peer.executor_fence, self.state())
            with self.assertRaisesRegex(ValueError, "backlog"):
                peer.workspace_state("overflow", peer.executor_fence, self.state())
        self.assertIn("0", peer.workspace_requests)

    def test_execute_background_exact_native_event(self):
        broker, peer, server = self.connected()
        message = self.execute(peer)
        server.send(message)
        payload = self.event(broker, "abush_browser.execute")
        self.assertEqual(payload, {**self.identity(peer), **{key: value for key, value in message.items()
                                                             if key != "type"}})
        self.assertFalse(peer.workspace_requests, "host must not create an enable")

    def test_execute_authorization_required(self):
        broker, peer, server = self.connected()
        message = self.execute(peer)
        message.pop("authorization")
        with self.assertRaises(ValueError):
            peer.handle(message)

    def test_execute_authorization_positive_exact(self):
        broker, peer, server = self.connected()
        for change in ({"revision": 0}, {"revision": True}, {"revision": 2 ** 64},
                       {"enable_id": ""}, {"browser_boot_id": "old"}, {"extra": 1}):
            with self.subTest(change=change), self.assertRaises(ValueError):
                peer.handle(self.execute(peer, authorization={**AUTH, **change}))

    def test_execute_page_flat_workspace_positive(self):
        broker, peer, server = self.connected()
        for page in ({**PAGE, "workspace_id": "other"}, {**PAGE, "generation": True},
                     {**PAGE, "generation": 0}, {**PAGE, "window_id": "old"},
                     {"window": {"workspace_id": "workspace", "window_id": "w", "generation": 1},
                      "page_id": "page", "generation": 1}):
            message = self.execute(peer)
            message["command"]["page"] = page
            with self.subTest(page=page), self.assertRaises(ValueError):
                peer.handle(message)

    def test_execute_strict_http_https_urls(self):
        broker, peer, server = self.connected()
        for url in ("file:///etc/passwd", "javascript:alert(1)", "https:example.test", "https:///",
                    "https://user:password@example.test/", "https://example.test:99999/",
                    "https://example.test/\n", "https://example.test/a b", ""):
            message = self.execute(peer)
            message["command"]["url"] = url
            with self.subTest(url=url), self.assertRaises(ValueError):
                peer.handle(message)
        message = self.execute(peer)
        message["command"]["url"] = "http://example.test:8080/"
        peer.handle(message)
        self.assertIn("effect", peer.pending)

    def test_execute_duplicate_is_idempotent_conflict_fails(self):
        broker, peer, server = self.connected()
        message = self.execute(peer)
        peer.handle(message)
        self.event(broker, "abush_browser.execute")
        peer.handle(message)
        self.assertTrue(broker.outbound.empty())
        with self.assertRaises(ValueError):
            peer.handle(self.execute(peer, authorization={**AUTH, "revision": 2}))

    def test_execute_old_fence_never_authorized(self):
        broker, peer, server = self.connected(epoch=2)
        with self.assertRaises(ValueError):
            peer.handle(self.execute(peer, executor_fence={"executor_id": "origin", "epoch": 1}))
        self.assertFalse(peer.pending)
        with mock.patch.dict(broker.peers, {"r_12345678": mock.Mock(connected=True,
                             principal_id="principal", metadata={"connection_id": "c_99999999"})}):
            with self.assertRaises(ValueError):
                peer.handle(self.execute(peer))

    def test_old_window_commands_rejected(self):
        broker, peer, server = self.connected()
        for command in ({"command": "close_window", "window": {"workspace_id": "workspace",
                        "window_id": "w", "generation": 1}}, {"command": "close_page", "page": PAGE}):
            with self.subTest(command=command), self.assertRaises(ValueError):
                peer.handle(self.execute(peer, command=command))

    def test_legacy_peer_operations_rejected_without_disconnect(self):
        broker, peer, server = self.connected()
        for kind in ("ensure_managed_page", "cancel_ensure_managed_page", "retire_managed_page",
                     "topology_report_response", "retire_managed_page_ack", "Execute", "Ping"):
            server.send({"type": kind})
        self.barrier(server)
        self.assertTrue(peer.connected)
        self.assertFalse(peer.pending)

    def test_legacy_native_operations_rejected(self):
        broker, peer, server = self.connected()
        for operation in ("ensure_managed_page_completion", "ensure_managed_page_cancellation_completion",
                          "managed_page_moved_outside", "topology_response_consumed",
                          "retire_managed_page_completion", "retire_managed_page_ack_consumed",
                          "register_candidate_lease", "arm_candidate_lease", "admit_candidate",
                          "finish_candidate_lease", "release_candidate_lease", "bind_x11_window",
                          "verify_x11_window", "clear_x11_window"):
            with self.subTest(operation=operation), self.assertRaisesRegex(ValueError, "unsupported"):
                broker.handle_request(self.request(operation, {}))

    def test_cancel_distinct_request_and_exact_event(self):
        broker, peer, server = self.connected()
        peer.handle(self.execute(peer))
        cancel = {"type": "cancel", "request_id": "cancel", "effect_id": "effect",
                  "executor_fence": peer.executor_fence}
        peer.handle(cancel)
        self.assertEqual(self.event(broker, "abush_browser.cancel"), {
            **self.identity(peer), "request_id": "cancel", "execution_request_id": "execute",
            "effect_id": "effect", "executor_fence": peer.executor_fence})
        with self.assertRaises(ValueError):
            peer.handle({**cancel, "request_id": "execute"})
        peer.handle(cancel)
        self.assertTrue(broker.outbound.empty())
        with self.assertRaises(ValueError):
            peer.handle({**cancel, "request_id": "conflict"})

    def test_cancel_after_selected_completion_retains_exact_origin(self):
        broker, peer, server = self.connected()
        peer.handle(self.execute(peer))
        self.event(broker, "abush_browser.execute")
        receipt = peer.complete("execute", "effect", peer.executor_fence, "applied", self.terminal(peer)["result"], None)
        self.assertEqual(receipt["completion"], "applied")
        self.assertNotIn("effect", peer.pending)
        cancel = {"type": "cancel", "request_id": "cancel", "effect_id": "effect",
                  "executor_fence": peer.executor_fence}
        peer.handle(cancel)
        self.assertEqual(self.event(broker, "abush_browser.cancel"), {
            **self.identity(peer), "request_id": "cancel", "execution_request_id": "execute",
            "effect_id": "effect", "executor_fence": peer.executor_fence})
        peer.handle(cancel)
        self.assertTrue(broker.outbound.empty())
        self.assertEqual(peer.complete_cancellation("cancel", "effect", peer.executor_fence,
                                                  "unknown")["completion"], "unknown")
        self.assertEqual(peer.complete("execute", "effect", peer.executor_fence, "applied", self.terminal(peer)["result"], None),
                         receipt)

    def test_paused_state_requires_exact_binding(self):
        broker, peer, server = self.connected()
        with self.assertRaises(ValueError):
            peer.workspace_state("paused", peer.executor_fence,
                                 {**self.state("paused"), "binding": None})
        self.assertFalse(peer.workspace_requests)

    def test_cancellation_cannot_be_applied(self):
        broker, peer, server = self.connected()
        with self.assertRaises(ValueError):
            peer.complete_cancellation("cancel", "effect", peer.executor_fence, "applied")
        with self.assertRaises(ValueError):
            peer.handle({"type": "cancellation_completion_accepted", "request_id": "cancel",
                         "effect_id": "effect", "executor_fence": peer.executor_fence,
                         "completion": "applied"})

    def test_terminal_native_success_requires_durable_socket_receipt(self):
        server = self.server()
        server.accept = False
        native = self.native()
        native.broker.refresh()
        peer = native.broker.peers["r_12345678"]
        self.event(native.broker, "abush_browser.remote_state")
        native._dispatch(self.request("executor_completion", self.terminal(peer)))
        sent = server.wait_for("completion")
        self.assertTrue(native.outbound.empty())
        receipt = {**sent, "type": "completion_accepted"}
        server.send(receipt)
        result = native.outbound.get(timeout=2)
        self.assertEqual(result, {"version": 1, "reply_to": "executor_completion", "ok": True,
                                  "payload": receipt})
        with native.request_workers_lock:
            workers = list(native.request_workers)
        for worker in workers:
            worker.join(timeout=2)
            self.assertFalse(worker.is_alive())

    def test_terminal_persistence_failure_no_ack_no_success(self):
        broker, peer, server = self.connected()
        server.accept = False  # Daemon persistence failure withholds durable receipt.
        peer.completion_timeout = 0
        with self.assertRaises(TimeoutError):
            broker.handle_request(self.request("executor_completion", self.terminal(peer)))
        sent = server.wait_for("completion")
        self.assertEqual(sent["completion"], "applied")
        self.assertIsNone(peer.completions[("completion", "effect")]["response"])

    def test_terminal_conflicting_retry_never_changes_selection(self):
        broker, peer, server = self.connected()
        result = broker.handle_request(self.request("executor_completion", self.terminal(peer)))
        with self.assertRaises(ValueError):
            broker.handle_request(self.request("executor_completion", self.terminal(peer, completion="unknown")))
        self.assertEqual(peer.completions[("completion", "effect")]["response"], result)

    def test_terminal_old_origin_current_transport_no_host_history(self):
        broker, peer, server = self.connected(epoch=2)
        origin = {"executor_id": "old-host", "epoch": 1}
        payload = self.terminal(peer, connection_id="c_99999999", executor_fence=origin)
        self.assertFalse(peer.pending)
        result = broker.handle_request(self.request("executor_completion", payload))
        self.assertEqual(result["executor_fence"], origin)
        self.assertEqual(server.wait_for("completion")["executor_fence"], origin)
        self.assertEqual(peer.executor_fence, server.fence)
        self.assertFalse(peer.pending)
        self.assertFalse(peer.workspace_requests)

    def test_terminal_current_principal_required(self):
        broker, peer, server = self.connected()
        with mock.patch.object(peer, "send") as send:
            with self.assertRaises(ValueError):
                broker.handle_request(self.request("executor_completion", self.terminal(peer, principal_id="other")))
            send.assert_not_called()

    def test_terminal_native_fields_and_fence_strict(self):
        broker, peer, server = self.connected()
        payload = self.terminal(peer)
        for change in ({"request_id": ""}, {"effect_id": ""}, {"connection_id": "../escape"},
                       {"executor_fence": {"executor_id": "e", "epoch": True}},
                       {"executor_fence": {"executor_id": "e", "epoch": 0}},
                       {"completion": "invalid"}, {"extra": 1}):
            with self.subTest(change=change), self.assertRaises(ValueError):
                broker.handle_request(self.request("executor_completion", {**payload, **change}))

    def test_terminal_late_receipt_and_duplicate_idempotent(self):
        broker, peer, server = self.connected()
        server.accept = False
        peer.completion_timeout = 0
        payload = self.terminal(peer)
        with self.assertRaises(TimeoutError):
            broker.handle_request(self.request("executor_completion", payload))
        receipt = {**server.wait_for("completion"), "type": "completion_accepted"}
        server.send(receipt)
        self.barrier(server)
        self.assertEqual(broker.handle_request(self.request("executor_completion", payload)), receipt)
        server.send(receipt)
        self.barrier(server, "late")
        self.assertTrue(peer.connected)
        self.assertEqual(sum(m["type"] == "completion" for m in server.received), 1)

    def test_terminal_conflicting_ack_does_not_close_valid_peer(self):
        broker, peer, server = self.connected()
        result = broker.handle_request(self.request("executor_completion", self.terminal(peer)))
        server.send({**result, "completion": "unknown"})
        self.barrier(server)
        self.assertTrue(peer.connected)
        self.assertEqual(peer.completions[("completion", "effect")]["response"], result)

    def test_terminal_write_failure_retains_selection_and_retries(self):
        broker, peer, server = self.connected()
        payload = self.terminal(peer)
        with mock.patch.object(peer, "send", side_effect=OSError("ambiguous write")):
            with self.assertRaises(OSError):
                broker.handle_request(self.request("executor_completion", payload))
        self.assertIn(("completion", "effect"), peer.completions)
        self.assertEqual(broker.handle_request(self.request("executor_completion", payload))["completion"], "applied")

    def test_terminal_unacked_backlog_not_evicted(self):
        broker, peer, server = self.connected()
        peer.completion_timeout = 0
        with mock.patch.object(peer, "send"):
            for index in range(256):
                with self.assertRaises(TimeoutError):
                    peer.complete(str(index), str(index), peer.executor_fence, "unknown", None, "original error")
            with self.assertRaisesRegex(ValueError, "backlog"):
                peer.complete("overflow", "overflow", peer.executor_fence, "unknown", None, "original error")
        self.assertIn(("completion", "0"), peer.completions)

    def test_terminal_receipt_before_waiter_is_exact(self):
        broker, peer, server = self.connected()
        receipt = {"type": "completion_accepted", "request_id": "execute", "effect_id": "effect",
                   "executor_fence": {"executor_id": "old", "epoch": 1}, "completion": "applied",
                   "result": self.terminal(peer)["result"], "error": None}
        server.send(receipt)
        self.barrier(server)
        with mock.patch.object(peer, "send") as send:
            self.assertEqual(peer.complete("execute", "effect", receipt["executor_fence"], "applied", receipt["result"], None), receipt)
            send.assert_not_called()

    def test_reconnect_replays_origin_terminal_not_execute(self):
        broker, peer, first = self.connected()
        first.accept = False
        peer.completion_timeout = 0
        with self.assertRaises(TimeoutError):
            peer.complete("execute", "effect", peer.executor_fence, "applied", self.terminal(peer)["result"], None)
        first.wait_for("completion")
        origin = peer.executor_fence
        second = self.server(connection="c_22222222", epoch=2)
        (self.root / "r_12345678/c_12345678/connection.json").unlink()
        broker.refresh()
        current = broker.peers["r_12345678"]
        payload = self.terminal(current, connection_id="c_12345678", executor_fence=origin)
        result = broker.handle_request(self.request("executor_completion", payload))
        self.assertEqual(result["executor_fence"], origin)
        self.assertEqual(second.wait_for("completion")["executor_fence"], origin)
        self.assertEqual(current.executor_fence, second.fence)
        self.assertFalse(current.pending)

    def test_principal_daemon_instance_pin_allows_boot_refresh(self):
        broker, peer, first = self.connected()
        second = self.server(connection="c_22222222", epoch=2, owner={**OWNER, "daemon_boot_id": "new"})
        (self.root / "r_12345678/c_12345678/connection.json").unlink()
        broker.refresh()
        self.assertEqual(broker.peers["r_12345678"].executor_fence, second.fence)

    def test_principal_replacement_rejected(self):
        broker, peer, first = self.connected()
        self.server(connection="c_22222222", principal="foreign")
        (self.root / "r_12345678/c_12345678/connection.json").unlink()
        self.assertFalse(broker.refresh()[0]["connected"])

    def test_secure_metadata_modes_and_socket_path(self):
        self.server()
        path = self.root / "r_12345678/c_12345678/connection.json"
        self.assertEqual(host.validate_remote_metadata(path, self.root)["connection_id"], "c_12345678")
        path.chmod(0o644)
        with self.assertRaises(ValueError):
            host.validate_remote_metadata(path, self.root)
        path.chmod(0o600)
        metadata = json.loads(path.read_text())
        metadata["browser_socket"] = "/outside/browser-peer.sock"
        path.write_text(json.dumps(metadata))
        with self.assertRaises(ValueError):
            host.validate_remote_metadata(path, self.root)

    def test_native_bootstrap_current_incarnation_exact(self):
        native = self.native()
        request = {"version": 1, "id": "bootstrap", "service": "native_host",
                   "operation": "bootstrap", "payload": {}}
        native._respond(request)
        self.assertEqual(native.outbound.get(timeout=1), {"version": 1, "reply_to": "bootstrap",
            "ok": True, "payload": {"browser_boot_id": BOOT["browser_boot_id"], "browser_pid": 42,
                                     "browser_starttime": "123", "ordinary_launch_leases": []}})
        native._respond({**request, "payload": {"extra": True}})
        self.assertFalse(native.outbound.get(timeout=1)["ok"])

    def test_native_remote_snapshot_written_before_reader_activation(self):
        native = self.native()
        native.broker.write_barrier = threading.Event
        peer = mock.Mock()
        peer.logical_state.return_value = {"workspace_id": "workspace"}
        writes = []
        with mock.patch.object(host, "send_message", side_effect=lambda value, _stream: writes.append(value)):
            native.writer.start()
            try:
                native.broker._activate_after_remote_state(peer)
                self.assertEqual(writes[0]["event"], "abush_browser.remote_state")
                peer.activate.assert_called_once_with()
            finally:
                native.outbound.put(None)
                native.writer.join(timeout=2)
        self.assertFalse(native.writer.is_alive())

    def test_native_workspace_state_is_blocking_typed_operation(self):
        native = self.native()
        for operation in ("workspace_state", "executor_completion", "cancellation_completion"):
            self.assertTrue(native._is_blocking_request(self.request(operation, {})))
        for operation in ("ensure_managed_page_completion", "topology_response_consumed", "arbitrary"):
            self.assertFalse(native._is_blocking_request(self.request(operation, {})))


if __name__ == "__main__":
    unittest.main()
