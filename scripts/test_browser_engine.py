#!/usr/bin/env python3
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
    "/home/amos/git/abush/.worktrees/browser-engine/browser-engine"))


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
                                         "XDG_CACHE_HOME": str(cache)}):
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
        (cache / "main.cjs").write_text("fixture")
        def process(arguments, **kwargs):
            fd = kwargs["pass_fds"][0]
            return subprocess.Popen([sys.executable, "-c", script, str(fd)], **kwargs)
        environment = {"ABUSH_BROWSER_ENGINE_ROOT": str(cache), "XDG_CACHE_HOME": str(cache)}
        with mock.patch.dict(os.environ, environment):
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
