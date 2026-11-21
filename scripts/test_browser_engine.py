#!/usr/bin/env python3
import hashlib
import io
import json
import os
import queue
import struct
import subprocess
import sys
import tempfile
import time
import pathlib
import threading
import unittest
from unittest import mock
from test_browser_peer_broker import BrowserPeerBrokerTest, host, clone

ENGINE_ROOT = pathlib.Path(os.environ.get("ABUSH_BROWSER_ENGINE_ROOT",
    pathlib.Path.home() / ".local/share/abush/browser-engine")).resolve()


def engine_fixture(root):
    module = host._browser_engine
    package = {"name": "abush-browser-engine", "version": "0.1.0",
               "engines": {"node": ">=20"},
               "dependencies": {"playwright-core": module.PLAYWRIGHT_VERSION}}
    lock = {"lockfileVersion": 3, "packages": {"": package,
        "node_modules/playwright-core": {"version": module.PLAYWRIGHT_VERSION,
            "integrity": module.PLAYWRIGHT_INTEGRITY,
            "resolved": "https://registry.npmjs.org/playwright-core/-/playwright-core-1.65.1.tgz"}}}
    for name, value in (("package.json", package), ("package-lock.json", lock)):
        (root / name).write_text(json.dumps(value))
    for name in ("main.cjs", "engine.cjs", "protocol.cjs"):
        (root / name).write_text("fixture")
    dependency = root / "node_modules/playwright-core"
    dependency.mkdir(parents=True)
    (dependency / "package.json").write_text(json.dumps({"version": "1.65.1"}))


class EngineTrustTest(unittest.TestCase):
    def setUp(self):
        self.module = host._browser_engine
        temporary_root = pathlib.Path(__file__).resolve().parents[1] / ".tmp"
        temporary_root.mkdir(exist_ok=True)
        temporary = tempfile.TemporaryDirectory(dir=temporary_root)
        self.addCleanup(temporary.cleanup)
        self.root = pathlib.Path(temporary.name)
        self.source = self.root / "source"
        self.source.mkdir()
        engine_fixture(self.source)
        environment = mock.patch.dict(os.environ, {"ABUSH_BROWSER_ENGINE_ROOT": str(self.source),
            "ABUSH_BROWSER_ENGINE_NODE": str(self.root / "node"),
            "XDG_DATA_HOME": str(self.root), "XDG_CACHE_HOME": str(self.root)})
        environment.start()
        self.addCleanup(environment.stop)
        self.node = self.root / "node"
        self.node.write_text("fixture executable")
        self.node.chmod(0o700)

    def test_node_version_is_checked_once_with_sanitized_environment(self):
        with mock.patch.dict(os.environ, {"NODE_OPTIONS": "--require /bad/code.js",
                "NODE_PATH": "/bad/modules", "npm_config_userconfig": "/bad/npmrc",
                "NPM_CONFIG_CACHE": "/bad/cache"}), mock.patch.object(
                self.module, "node_version", return_value=(0, b"v20.1.0\n")) as run:
            env = self.module.engine_environment()
            self.assertFalse({"NODE_OPTIONS", "NODE_PATH", "npm_config_userconfig",
                              "NPM_CONFIG_CACHE"} & env.keys())
            self.assertEqual(self.module.trusted_node(env), str(self.node))
            self.module.trusted_node(env)
            run.assert_called_once_with(self.node, env)

    def test_invalid_or_old_node_fails_before_artifacts_and_spawn(self):
        factory = mock.Mock()
        with mock.patch.object(self.module, "artifacts_directory") as artifacts:
            for path in ("node", str(self.root / "missing"), str(self.source)):
                with self.subTest(path=path), mock.patch.dict(os.environ,
                        {"ABUSH_BROWSER_ENGINE_NODE": path}), self.assertRaises(ValueError):
                    host.BrowserEngine(lambda _: None, process_factory=factory)
            self.node.chmod(0o722)
            with self.assertRaisesRegex(ValueError, "unsafe"):
                host.BrowserEngine(lambda _: None, process_factory=factory)
            self.node.chmod(0o700)
            for output in (b"v18.20.0\n", b"invalid"):
                with mock.patch.object(self.module, "node_version", return_value=(0, output)), \
                        self.assertRaisesRegex(ValueError, "Node >=20"):
                    host.BrowserEngine(lambda _: None, process_factory=factory)
            with mock.patch.object(self.module, "node_version",
                    side_effect=subprocess.TimeoutExpired("node", 3)), \
                    self.assertRaisesRegex(ValueError, "timed out"):
                host.BrowserEngine(lambda _: None, process_factory=factory)
            artifacts.assert_not_called()
        factory.assert_not_called()

    def test_node_version_output_and_time_are_bounded_without_echoing_secrets(self):
        for script, expected in (("printf '%5000s' secret", "output exceeds limit"),
                                 ("exec sleep 10", "timed out")):
            self.node.write_text("#!/bin/sh\n" + script + "\n")
            self.node.chmod(0o700)
            with mock.patch.object(self.module, "NODE_CHECK_TIMEOUT", 0.05), \
                    self.assertRaisesRegex(ValueError, expected) as raised:
                self.module.trusted_node(self.module.engine_environment())
            self.assertNotIn("secret", str(raised.exception))

    def test_node_symlink_escape_and_missing_default_rejected(self):
        self.node.unlink()
        self.node.symlink_to(self.source / "main.cjs")
        with self.assertRaisesRegex(ValueError, "symlink escape"):
            self.module.trusted_node({})
        os.environ.pop("ABUSH_BROWSER_ENGINE_NODE")
        with mock.patch.object(self.module.shutil, "which", return_value=None) as which, \
                self.assertRaisesRegex(ValueError, "not found"):
            self.module.trusted_node({})
        which.assert_called_once_with("node", path=self.module.NODE_PATH)

    def test_explicit_root_is_trusted_override_without_manifest(self):
        self.assertEqual(self.module.trusted_engine_root(), self.source)
        for root in ("relative", str(self.root / "missing")):
            with mock.patch.dict(os.environ, {"ABUSH_BROWSER_ENGINE_ROOT": root}), \
                    self.assertRaises(ValueError):
                host.BrowserEngine(lambda _: None, process_factory=mock.Mock())
        self.source.chmod(0o777)
        with self.assertRaisesRegex(ValueError, "unsafe"):
            self.module.trusted_engine_root()
        self.source.chmod(0o700)
        (self.source / "node_modules/playwright-core/escape").symlink_to(self.node)
        with self.assertRaisesRegex(ValueError, "symlink escape"):
            self.module.trusted_engine_root()
        (self.source / "node_modules/playwright-core/escape").unlink()
        (self.source / "package-lock.json").write_text('{}')
        with self.assertRaisesRegex(ValueError, "lock mismatch"):
            self.module.trusted_engine_root()

    def managed(self):
        prefix = self.root / "abush"
        versions = prefix / "browser-engine-versions"
        versions.mkdir(parents=True)
        assets = {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                  for p in self.source.iterdir() if p.is_file()}
        fingerprint = hashlib.sha256(json.dumps(assets, sort_keys=True,
            separators=(",", ":")).encode()).hexdigest()
        generation = versions / f"0.1.0-{fingerprint}"
        self.source.rename(generation)
        files = {p.relative_to(generation).as_posix():
                 {"sha256": hashlib.sha256(p.read_bytes()).hexdigest()}
                 for p in generation.rglob("*") if p.is_file()}
        manifest = {"schema_version": 1, "playwright_core": "1.65.1",
                    "engine_version": "0.1.0", "assets": assets,
                    "fingerprint": fingerprint, "files": files}
        (generation / "abush-engine-manifest.json").write_text(json.dumps(manifest))
        for path in generation.rglob("*"):
            if path.is_file():
                path.chmod(0o400)
        (prefix / "browser-engine").symlink_to(
            pathlib.Path("browser-engine-versions") / generation.name)
        os.environ.pop("ABUSH_BROWSER_ENGINE_ROOT")
        return generation, prefix / "browser-engine", manifest

    def test_default_managed_root_requires_exact_manifest_and_read_only_files(self):
        generation, link, manifest = self.managed()
        self.assertEqual(self.module.trusted_engine_root(), generation)
        entry = generation / "main.cjs"
        entry.chmod(0o600)
        with self.assertRaisesRegex(ValueError, "read-only"):
            self.module.trusted_engine_root()
        entry.write_text("tampered")
        entry.chmod(0o400)
        with self.assertRaisesRegex(ValueError, "manifest mismatch"):
            self.module.trusted_engine_root()
        link.unlink()
        link.symlink_to(generation)
        with self.assertRaisesRegex(ValueError, "symlink target"):
            self.module.trusted_engine_root()

    def test_root_failures_do_not_create_artifacts_or_spawn(self):
        with mock.patch.object(self.module, "artifacts_directory") as artifacts, \
                mock.patch.object(self.module, "trusted_node") as node:
            factory = mock.Mock()
            (self.source / "main.cjs").chmod(0o666)
            with self.assertRaisesRegex(ValueError, "unsafe writable"):
                host.BrowserEngine(lambda _: None, process_factory=factory)
            artifacts.assert_not_called()
            factory.assert_not_called()
            node.assert_not_called()

    def test_managed_missing_manifest_and_unlisted_file_rejected(self):
        generation, _, _ = self.managed()
        extra = generation / "unlisted.cjs"
        extra.write_text("unlisted")
        extra.chmod(0o400)
        with self.assertRaisesRegex(ValueError, "manifest mismatch"):
            self.module.trusted_engine_root()
        extra.unlink()
        (generation / "abush-engine-manifest.json").unlink()
        with self.assertRaisesRegex(ValueError, "missing or invalid"):
            self.module.trusted_engine_root()

    def test_managed_manifest_dependency_mismatch_rejected(self):
        generation, _, manifest = self.managed()
        manifest["playwright_core"] = "1.64.0"
        path = generation / "abush-engine-manifest.json"
        path.chmod(0o600)
        path.write_text(json.dumps(manifest))
        path.chmod(0o400)
        with self.assertRaisesRegex(ValueError, "manifest mismatch"):
            self.module.trusted_engine_root()


class EngineBrokerTest(BrowserPeerBrokerTest):
    def context(self, peer, **changes):
        message = self.execute(peer, **changes)
        peer.handle(message)
        return self.event(peer.broker, "abush_browser.execute")

    def test_remote_execution_admission_keeps_cancel_available(self):
        broker, peer, server = self.connected()
        for index in range(32):
            peer.handle(self.execute(peer, request_id=f"request-{index}", effect_id=f"effect-{index}"))
            self.event(broker, "abush_browser.execute")
        with self.assertRaisesRegex(OSError, "4 MiB per remote"):
            peer.handle(self.execute(peer, request_id="overflow", effect_id="overflow"))
        peer.handle({"type": "cancel", "request_id": "cancel-request",
                     "effect_id": "effect-0", "executor_fence": peer.executor_fence})
        self.event(broker, "abush_browser.cancel")
        self.assertEqual(len(peer.pending), 32)
        self.assertNotIn("overflow", peer.pending)

    def test_engine_requires_origin_admission_and_exact_lease(self):
        broker, peer, server = self.connected()
        context = self.context(peer)
        engine = mock.Mock()
        engine.request.return_value = {"ok": True, "result": {"text": "original", "data": [1, None], "images": []}}
        with mock.patch.object(host, "BrowserEngine", return_value=engine):
            lease = broker.engine_request("engine_open", context)["lease_id"]
            self.event(broker, "abush_browser.engine_lease")
            for change in ({"lease_id": "smuggled"}, {"authorization": {**context["authorization"], "revision": 99}},
                           {"principal_id": "foreign"}, {"request_id": "not-admitted"}):
                with self.subTest(change=change), self.assertRaises(ValueError):
                    broker.engine_request("engine_execute", {**context, "lease_id": lease, **change})
            result = broker.engine_request("engine_execute", {**context, "lease_id": lease})
            terminal = {**self.terminal(peer), "result": result["result"], "error": result["error"]}
            receipt = broker.handle_request(self.request("executor_completion", terminal))
            self.assertEqual(receipt["result"], result["result"])
            self.assertEqual(server.wait_for("completion")["result"], result["result"])
            with self.assertRaises(ValueError):
                broker.engine_request("engine_execute", {**context, "lease_id": lease})
            broker.engine_request("engine_close", {**context, "lease_id": lease})
            engine.close.assert_called_once()

    def test_engine_cdp_relay_during_blocked_worker_and_disconnect_fences_output(self):
        broker, peer, server = self.connected()
        context = self.context(peer)
        entered, release = threading.Event(), threading.Event()
        callbacks = []
        stopped = threading.Event()
        engine = mock.Mock()
        engine.close.side_effect = stopped.set
        def request(kind, lease_id, **fields):
            if kind == "execute":
                entered.set()
                if not release.wait(2):
                    raise TimeoutError("fixture worker barrier")
            return {"ok": True, "result": {"text": "original", "data": None, "images": []}}
        engine.request.side_effect = request
        def spawn(callback):
            callbacks.append(callback)
            return engine
        with mock.patch.object(host, "BrowserEngine", side_effect=spawn):
            lease = broker.engine_request("engine_open", context)["lease_id"]
            self.event(broker, "abush_browser.engine_lease")
            results = []
            def execute_worker():
                try:
                    results.append(broker.engine_request("engine_execute", {**context, "lease_id": lease}))
                except ValueError as error:
                    results.append(error)
            worker = threading.Thread(target=execute_worker, daemon=True)
            worker.start()
            try:
                self.assertTrue(entered.wait(1))
                callbacks[0]({"type": "cdp", "lease_id": lease, "message": {"id": 5, "method": "Network.enable"}})
                event = self.event(broker, "abush_browser.engine_cdp")
                self.assertEqual(event, {**context, "lease_id": lease, "message": {"id": 5, "method": "Network.enable"}})
                broker.engine_request("engine_cdp", {**context, "lease_id": lease, "message": {"id": 5, "result": {}}})
                engine.send.assert_called_once()
                peer.close()
                callbacks[0]({"type": "cdp", "lease_id": lease, "message": {"id": 6}})
                self.assertTrue(broker.outbound.empty())
                self.assertTrue(stopped.wait(1))
                engine.close.assert_called_once()
            finally:
                release.set()
                worker.join(timeout=2)
            self.assertFalse(worker.is_alive())
            self.assertIsInstance(results[0], ValueError)

    def test_persistent_cdp_uses_lease_not_current_execution_context(self):
        broker, peer, server = self.connected()
        first = self.context(peer)
        callbacks = []
        engine = mock.Mock()
        engine.request.return_value = {"ok": True, "result": {
            "text": "original", "data": None, "images": []}}
        def spawn(callback):
            callbacks.append(callback)
            return engine
        with mock.patch.object(host, "BrowserEngine", side_effect=spawn):
            lease = broker.engine_request("engine_open", first)["lease_id"]
            self.event(broker, "abush_browser.engine_lease")
            broker.engine_request("engine_execute", {**first, "lease_id": lease})
            held = {**first, "lease_id": lease, "message": {"id": 81, "result": {}}}
            # Completion removes A's admission before B is even delivered.
            with peer.transport_lock:
                peer.pending.pop(first["effect_id"])
            second = self.context(peer, request_id="request-B", effect_id="effect-B",
                                  command={**first["command"], "url": "https://example.org/B"})
            # An event response can carry B's label before engine_execute(B).
            broker.engine_request("engine_cdp", {**second, "lease_id": lease,
                                                "message": {"id": 82, "result": {}}})
            entered, release = threading.Event(), threading.Event()
            def delayed_native_event():
                entered.set()
                if not release.wait(2):
                    return
                callbacks[0]({"type": "cdp", "lease_id": lease,
                              "message": {"id": 83, "method": "Network.getResponseBody"}})
            worker = threading.Thread(target=delayed_native_event, daemon=True)
            worker.start()
            try:
                self.assertTrue(entered.wait(1))
                broker.engine_request("engine_execute", {**second, "lease_id": lease})
                broker.engine_request("engine_cdp", held)
                self.assertEqual(engine.send.call_count, 2)
            finally:
                release.set()
                worker.join(timeout=2)
            self.assertFalse(worker.is_alive())
            event = self.event(broker, "abush_browser.engine_cdp")
            self.assertEqual(host.BrowserBroker.engine_binding(event),
                             host.BrowserBroker.engine_binding(first))
            self.assertEqual(event["request_id"], first["request_id"])
            for change in ({"lease_id": "retired"}, {"remote_key": "r_foreign000"},
                           {"principal_id": "foreign"},
                           {"executor_fence": {**first["executor_fence"], "epoch": 99}},
                           {"authorization": {**first["authorization"], "revision": 99}},
                           {"command": {**first["command"], "page": {
                               **first["command"]["page"], "generation": 99}}},
                           {"command": {**first["command"], "page": {
                               **first["command"]["page"], "page_id": "foreign"}}}):
                with self.subTest(change=change), self.assertRaises(ValueError):
                    broker.engine_request("engine_cdp", {**held, **change})
            with self.assertRaisesRegex(ValueError, "admission"):
                broker.engine_request("engine_execute", {**first, "lease_id": lease})
            broker.engine_request("engine_close", {**first, "lease_id": lease})
            engine.close.assert_called_once()

    def test_v9_rejected(self):
        server = self.server()
        broker = self.broker()
        with mock.patch.object(host, "PEER_PROTOCOL_VERSION", 9):
            self.assertFalse(broker.refresh()[0]["connected"])
        self.assertEqual(server.wait_for("hello")["version"], 9)

    def test_completion_output_required_and_exact(self):
        broker, peer, server = self.connected()
        for change in ({"result": None}, {"error": "unexpected"}, {"result": {"text": "missing"}},
                       {"completion": "unknown", "result": None, "error": ""}):
            with self.subTest(change=change), self.assertRaises(ValueError):
                broker.handle_request(self.request("executor_completion", self.terminal(peer, **change)))
        payload = self.terminal(peer, completion="unknown")
        receipt = broker.handle_request(self.request("executor_completion", payload))
        self.assertEqual(receipt["error"], "original error")
        self.assertIsNone(receipt["result"])
        with self.assertRaises(ValueError):
            broker.handle_request(self.request("executor_completion", {**payload, "error": "changed"}))


class EngineSocketTest(unittest.TestCase):
    def test_official_engine_inherited_fd_bootstrap_and_bounded_close(self):
        cache = pathlib.Path(__file__).resolve().parents[1] / ".tmp/engine-cache"
        cache.mkdir(parents=True, exist_ok=True)
        cdp = threading.Event()
        messages = []
        def receive(message):
            messages.append(message)
            cdp.set()
        with mock.patch.dict(os.environ, {"ABUSH_BROWSER_ENGINE_ROOT": str(ENGINE_ROOT),
                                         "XDG_CACHE_HOME": str(cache),
                                         "NODE_OPTIONS": "--require /bad/bootstrap.js",
                                         "NODE_PATH": "/bad/modules",
                                         "npm_config_userconfig": "/bad/npmrc"}):
            engine = host.BrowserEngine(receive, timeout=3)
            errors = []
            def opening():
                try:
                    engine.request("open", "fixture-lease")
                except (OSError, TimeoutError) as error:
                    errors.append(error)
            worker = threading.Thread(target=opening, daemon=True)
            worker.start()
            try:
                self.assertTrue(cdp.wait(3), "official Playwright must initialize over inherited FD")
                self.assertEqual(messages[0]["type"], "cdp")
                self.assertEqual(messages[0]["lease_id"], "fixture-lease")
                self.assertIsInstance(messages[0]["message"]["method"], str)
                engine.close_lease("fixture-lease")
                worker.join(timeout=2)
                self.assertFalse(worker.is_alive(), "lease cancellation wakes blocked request")
            finally:
                engine.close()
                worker.join(timeout=2)
            engine.reader.join(timeout=2)
            engine.writer.join(timeout=2)
            self.assertFalse(engine.reader.is_alive())
            self.assertFalse(engine.writer.is_alive())
            self.assertIsNotNone(engine.process.poll())


class TransportBoundsTest(unittest.TestCase):
    def native(self, **kwargs):
        return host.NativeBrokerHost(input_stream=io.BytesIO(), output_stream=io.BytesIO(),
                                     ordinary_open_factory=None, **kwargs)

    def test_byte_count_reserve_and_close_wakes_consumer(self):
        outbound = host.BoundedQueue(maxsize=2, max_bytes=8,
                                     reserve_count=1, reserve_bytes=8)
        outbound.put(b"12345678")
        with self.assertRaises(queue.Full):
            outbound.put(b"x")
        outbound.put((None, threading.Event()), control=True)
        with self.assertRaises(queue.Full):
            outbound.put(b"123456789", control=True)
        self.assertEqual(outbound.bytes, 8)
        self.assertEqual(outbound.get(), b"12345678")
        self.assertIsInstance(outbound.get(), tuple)
        entered, stopped = threading.Event(), threading.Event()
        def consume():
            entered.set()
            self.assertIsNone(outbound.get())
            stopped.set()
        worker = threading.Thread(target=consume, daemon=True)
        worker.start()
        self.assertTrue(entered.wait(1))
        outbound.close()
        self.assertTrue(stopped.wait(1))
        worker.join(1)
        self.assertFalse(worker.is_alive())
        with self.assertRaises(queue.Full):
            outbound.put(b"late")

    def test_worker_admission_reserves_completion_and_close(self):
        native = self.native()
        entered = threading.Condition()
        release = threading.Event()
        starts = []
        def blocked(message):
            with entered:
                starts.append(message["operation"])
                entered.notify_all()
            if not release.wait(2):
                raise TimeoutError("fixture release")
            return {}
        native.broker.handle_request = blocked
        try:
            for index in range(32):
                native._dispatch({"version": 1, "id": str(index), "service": "abush_browser",
                                  "operation": "engine_execute", "payload": {}})
            with entered:
                self.assertTrue(entered.wait_for(lambda: len(starts) == 32, timeout=1))
            native._dispatch({"version": 1, "id": "rejected", "service": "abush_browser",
                              "operation": "engine_execute", "payload": {}})
            rejection = native.outbound.get(timeout=1)
            self.assertFalse(rejection["ok"])
            self.assertIn("admission exhausted", rejection["error"])
            native._dispatch({"version": 1, "id": "cancel", "service": "abush_browser",
                              "operation": "engine_close", "payload": {}})
            with entered:
                self.assertTrue(entered.wait_for(lambda: len(starts) == 33, timeout=1))
            self.assertEqual(native.normal_workers, 32)
        finally:
            release.set()
            for worker in list(native.request_workers):
                worker.join(1)
                self.assertFalse(worker.is_alive())
        self.assertEqual(native.normal_workers, 0)
        self.assertEqual(native.worker_bytes, 0)

    def test_worker_byte_admission_and_flood_failure_are_explicit(self):
        native = self.native()
        native.normal_worker_bytes = 4 * host.MAX_MESSAGE_SIZE
        native.worker_bytes = native.normal_worker_bytes
        native._dispatch({"version": 1, "id": "byte-overflow", "service": "abush_browser",
                          "operation": "engine_execute", "payload": {}})
        self.assertIn("admission exhausted", native.outbound.get()["error"])
        self.assertEqual(len(native.request_workers), 0)
        native.outbound = host.BoundedQueue(maxsize=1, max_bytes=1024,
                                           reserve_count=1, reserve_bytes=1024,
                                           on_full=native._fatal)
        native.outbound.put(host.event("abush_browser.engine_cdp", "abush_browser", {}))
        with self.assertRaises(queue.Full):
            native.outbound.put(host.event("abush_browser.engine_cdp", "abush_browser", {}))
        self.assertIsInstance(native.fatal_error, OSError)
        native.outbound.put(host.response("durable-completion", payload={}))
        self.assertEqual(native.outbound.qsize(), 2)

    def test_native_partial_body_and_blocked_stdout_deadlines(self):
        for direction in ("read", "write"):
            with self.subTest(direction=direction):
                read_fd, write_fd = os.pipe()
                stream = os.fdopen(read_fd if direction == "read" else write_fd,
                                   "rb" if direction == "read" else "wb", buffering=0)
                other = write_fd if direction == "read" else read_fd
                native = self.native()
                try:
                    with mock.patch.object(host, "NATIVE_IO_TIMEOUT", 0.05):
                        if direction == "read":
                            native.input = stream
                            os.write(other, struct.pack("<I", 100) + b"{")
                            with self.assertRaisesRegex(TimeoutError, "assembly"):
                                native._receive()
                        else:
                            native.output = stream
                            with self.assertRaisesRegex(TimeoutError, "stdout"):
                                native._send({"body": "x" * 100000})
                finally:
                    stream.close()
                    os.close(other)

    def test_native_blocked_fixture_write_fails_closed_and_revokes(self):
        entered, release = threading.Event(), threading.Event()
        native = self.native()
        def write(data):
            entered.set()
            if not release.wait(2):
                raise TimeoutError("fixture output release")
        native.output = mock.Mock(write=write)
        native.broker.refresh = mock.Mock(return_value=[])
        errors = []
        def run():
            try:
                native.run()
            except OSError as error:
                errors.append(error)
        with mock.patch.object(host, "SHUTDOWN_TIMEOUT", 0.05):
            worker = threading.Thread(target=run, daemon=True)
            worker.start()
            try:
                self.assertTrue(entered.wait(1))
                worker.join(1)
                self.assertFalse(worker.is_alive())
                self.assertIn("writer did not exit", str(errors[0]))
                self.assertTrue(native.outbound.closed)
            finally:
                release.set()
                native.writer.join(1)
                worker.join(1)
        self.assertFalse(native.writer.is_alive())

    def test_shared_shutdown_deadline_not_per_worker(self):
        native = self.native()
        # This test exhausts the entire budget in worker joins, not native output.
        native.writer = mock.Mock(is_alive=lambda: False)
        joins = []
        def join(timeout):
            joins.append(timeout)
            threading.Event().wait(timeout)
        native.request_workers = {mock.Mock(join=join) for _ in range(8)}
        started = time.monotonic()
        with mock.patch.object(host, "SHUTDOWN_TIMEOUT", 0.1):
            native.run()
        self.assertLess(time.monotonic() - started, 0.5)
        self.assertEqual(len(joins), 8)
        self.assertLess(sum(joins), 0.15)

    def engine(self, script, timeout=1):
        root = pathlib.Path(__file__).resolve().parents[1] / ".tmp"
        root.mkdir(exist_ok=True)
        directory = tempfile.TemporaryDirectory(dir=root)
        self.addCleanup(directory.cleanup)
        cache = pathlib.Path(directory.name)
        engine_fixture(cache)
        def process(arguments, **kwargs):
            self.assertTrue(pathlib.Path(arguments[0]).is_absolute())
            self.assertNotIn("NODE_OPTIONS", kwargs["env"])
            fd = kwargs["pass_fds"][0]
            return subprocess.Popen([sys.executable, "-c", script, str(fd)], **kwargs)
        environment = {"ABUSH_BROWSER_ENGINE_ROOT": str(cache), "XDG_CACHE_HOME": str(cache)}
        with mock.patch.dict(os.environ, environment), mock.patch.object(
                host._browser_engine, "trusted_node", return_value="/usr/bin/node"):
            engine = host.BrowserEngine(lambda _: None, timeout=timeout, process_factory=process)
        self.addCleanup(engine.close)
        return engine

    def test_child_stderr_root_cause_survives_eof_and_artifacts_removed(self):
        engine = self.engine("import os,sys; os.write(2, b'Distinct startup dependency failure\\n'); "
                             "os.close(int(sys.argv[1]))")
        self.assertTrue(engine.close_done.wait(2))
        with self.assertRaisesRegex(OSError, "Distinct startup dependency failure"):
            engine.request("open", "fixture")
        self.assertFalse(engine.artifacts.exists())
        self.assertLessEqual(len(engine.diagnostics()), 8192)
        self.assertFalse(engine.writer.is_alive())

    def test_fragmented_bad_utf8_and_frame_deadline_fail_closed_not_idle(self):
        for data in (b"\x00\x00\x00\x01\xff", b"\x00"):
            with self.subTest(data=data):
                script = ("import os,sys,time; fd=int(sys.argv[1]); "
                          f"os.write(fd, {data!r}); time.sleep(10)")
                with mock.patch.object(host._browser_engine, "FRAME_TIMEOUT", 0.05):
                    engine = self.engine(script)
                    self.assertTrue(engine.close_done.wait(2))
                self.assertTrue(engine.closed)
                self.assertIsNotNone(engine.failure)
        idle = self.engine("import time; time.sleep(10)")
        self.assertFalse(idle.close_done.wait(0.1), "idle is not a partial frame")
        idle.close()

    def test_strict_reply_output_and_late_unknown_reply(self):
        for output in ({"ok": True, "result": {"text": "missing"}},
                       {"ok": False, "error": ""},
                       {"ok": True, "result": None, "extra": 1}):
            message = {"type": "reply", "request_id": "unknown", "lease_id": "lease", **output}
            data = json.dumps(message).encode()
            script = ("import os,sys,time; "
                      f"os.write(int(sys.argv[1]), {struct.pack('>I', len(data)) + data!r}); "
                      "time.sleep(10)")
            engine = self.engine(script)
            self.assertTrue(engine.close_done.wait(2))
            self.assertIn("invalid engine reply output", engine.failure)
        script = """
import json, os, socket, struct, sys, time
sock = socket.socket(fileno=int(sys.argv[1]))
def exact(size):
    data = b''
    while len(data) < size:
        data += sock.recv(size - len(data))
    return data
def receive():
    return json.loads(exact(struct.unpack('>I', exact(4))[0]))
def send(message):
    data = json.dumps(message).encode()
    sock.sendall(struct.pack('>I', len(data)) + data)
request = receive()
close = receive()
send({'type': 'reply', 'request_id': request['request_id'],
      'lease_id': request['lease_id'], 'ok': True,
      'result': {'text': 'late', 'data': None, 'images': []}})
send({'type': 'cdp', 'lease_id': 'other-lease', 'message': {'method': 'marker'}})
time.sleep(10)
"""
        engine = self.engine(script, timeout=0.05)
        observed = threading.Event()
        engine.on_cdp = lambda _: observed.set()
        with self.assertRaises(TimeoutError):
            engine.request("execute", "lease", action={"type": "text"})
        self.assertTrue(observed.wait(1), "late reply precedes barrier marker")
        self.assertFalse(engine.closed)
        self.assertEqual(engine.pending, {})
        with self.assertRaisesRegex(OSError, "revoked"):
            engine.request("execute", "lease", action={"type": "text"})

    def test_request_timeout_revokes_only_lease_and_late_completion_not_retried(self):
        engine = self.engine("import time; time.sleep(10)", timeout=0.05)
        with self.assertRaisesRegex(TimeoutError, "lease revoked"):
            engine.request("execute", "lease-one", action={"type": "text"})
        self.assertFalse(engine.closed)
        with self.assertRaisesRegex(OSError, "lease revoked"):
            engine.request("execute", "lease-one", action={"type": "text"})
        self.assertEqual(engine.pending, {})
        self.assertNotIn("lease-two", engine.revoked_leases)


if __name__ == "__main__":
    unittest.main()
