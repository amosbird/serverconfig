#!/usr/bin/env python3
import os
import pathlib
import threading
import unittest
from unittest import mock
from test_browser_peer_broker import BrowserPeerBrokerTest, host, clone

ENGINE_ROOT = pathlib.Path("/home/amos/git/abush/.worktrees/browser-engine/browser-engine")


class EngineBrokerTest(BrowserPeerBrokerTest):
    def context(self, peer, **changes):
        message = self.execute(peer, **changes)
        peer.handle(message)
        return self.event(peer.broker, "abush_browser.execute")

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


if __name__ == "__main__":
    unittest.main()
