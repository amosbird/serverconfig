#!/usr/bin/env python3

import importlib.util
import json
import os
import pathlib
import socket
import subprocess
import sys
import tempfile
import threading
import time
import unittest
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts/abush-browser-local.py"
spec = importlib.util.spec_from_file_location("abush_browser_local", SCRIPT)
local = importlib.util.module_from_spec(spec)
spec.loader.exec_module(local)
common_dir = pathlib.Path(subprocess.run(
    ["git", "rev-parse", "--git-common-dir"], cwd=ROOT, check=True,
    capture_output=True, text=True, timeout=5,
).stdout.strip())
COMMON_TEMP = (ROOT / common_dir).resolve().parent / ".worktrees" / ".tmp"


class LocalBrowserPublisherTest(unittest.TestCase):
    def setUp(self):
        COMMON_TEMP.mkdir(parents=True, exist_ok=True)
        temporary = tempfile.TemporaryDirectory(prefix="l-", dir=COMMON_TEMP)
        self.addCleanup(temporary.cleanup)
        self.runtime = pathlib.Path(temporary.name)
        self.abush = self.runtime / "abush"
        self.abush.mkdir(mode=0o700)
        self.target = self.abush / "browser-peer.sock"
        self.assertLess(len(os.fsencode(self.target)), 108, "fixture AF_UNIX path is too long")
        self.server = self.socket()
        self.server.bind(str(self.target))
        os.chmod(self.target, 0o600)
        self.server.listen(4)

    def socket(self):
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.addCleanup(sock.close)
        sock.settimeout(2)
        return sock

    def accept(self):
        sock, _ = self.server.accept()
        self.addCleanup(sock.close)
        sock.settimeout(2)
        return sock

    def stop(self, process):
        try:
            if process.poll() is None:
                process.terminate()
            try:
                process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=2)
        finally:
            if process.stderr:
                process.stderr.close()

    def start(self):
        process = subprocess.Popen(
            [sys.executable, str(SCRIPT)],
            env={**os.environ, "XDG_RUNTIME_DIR": str(self.runtime)},
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
        )
        self.addCleanup(self.stop, process)
        return process

    def wait_for(self, path):
        deadline = time.monotonic() + 2
        while time.monotonic() < deadline:
            if path.exists():
                return
            time.sleep(0.01)
        self.fail(f"timed out waiting for {path}")

    def run_workers(self, tasks, sockets, timeout=10):
        errors = []

        def run(function, args):
            try:
                function(*args)
            except BaseException as error:
                errors.append(error)

        threads = [threading.Thread(target=run, args=task, daemon=True) for task in tasks]
        deadline = time.monotonic() + timeout
        try:
            for thread in threads:
                thread.start()
            for thread in threads:
                thread.join(timeout=max(0, deadline - time.monotonic()))
        finally:
            if any(thread.is_alive() for thread in threads):
                for sock in sockets:
                    try:
                        sock.shutdown(socket.SHUT_RDWR)
                    except OSError:
                        pass
                cleanup_deadline = time.monotonic() + 2
                for thread in threads:
                    if thread.ident is not None:
                        thread.join(timeout=max(0, cleanup_deadline - time.monotonic()))
        self.assertLess(time.monotonic(), deadline, "relay exchange timed out")
        self.assertFalse(any(thread.is_alive() for thread in threads), "relay worker did not exit")
        if errors:
            raise errors[0]

    def test_fixture_runtime_uses_common_repository_even_with_long_root(self):
        fixture = LocalBrowserPublisherTest()
        self.addCleanup(fixture.doCleanups)
        long_root = ROOT / ("long-worktree-" * 20)
        with mock.patch(__name__ + ".ROOT", long_root):
            fixture.setUp()
        self.assertEqual(fixture.runtime.parent, COMMON_TEMP)
        self.assertNotIn(long_root, fixture.runtime.parents)
        self.assertLess(len(os.fsencode(fixture.target)), 108)

    def test_setup_failure_closes_socket_and_removes_runtime(self):
        fixture = LocalBrowserPublisherTest()
        self.addCleanup(fixture.doCleanups)
        with mock.patch.object(socket.socket, "bind", side_effect=OSError("fixture bind failed")):
            with self.assertRaisesRegex(OSError, "fixture bind failed"):
                fixture.setUp()
        fixture.doCleanups()
        self.assertEqual(fixture.server.fileno(), -1)
        self.assertFalse(fixture.runtime.exists())

    def test_relay_worker_failure_is_propagated(self):
        def fail():
            raise RuntimeError("fixture worker failed")

        with self.assertRaisesRegex(RuntimeError, "fixture worker failed"):
            self.run_workers([(fail, ())], [])

    def test_relay_worker_deadline_unblocks_reads(self):
        client, upstream = socket.socketpair()
        self.addCleanup(client.close)
        self.addCleanup(upstream.close)
        client.settimeout(2)
        upstream.settimeout(2)
        with self.assertRaisesRegex(AssertionError, "relay exchange timed out"):
            self.run_workers(
                [(client.recv, (1,)), (upstream.recv, (1,))],
                [client, upstream], timeout=0.05,
            )

    def test_repeated_startup_reuses_registration_and_relays(self):
        first = self.start()
        connection = self.abush / "browser-remotes" / local.REMOTE_KEY / local.CONNECTION_ID
        metadata = connection / "connection.json"
        published = connection / "browser-peer.sock"
        self.wait_for(metadata)
        self.assertEqual(stat_mode(connection), 0o700)
        self.assertEqual(stat_mode(metadata), 0o600)
        self.assertEqual(stat_mode(published), 0o600)
        data = json.loads(metadata.read_text())
        self.assertEqual(data["remote_key"], local.REMOTE_KEY)
        self.assertEqual(data["connection_id"], local.CONNECTION_ID)

        second = self.start()
        self.assertEqual(second.wait(timeout=2), 0)
        self.assertIsNone(first.poll())

        client = self.socket()
        local.connect_socket(client, published)
        upstream = self.accept()
        client.sendall(b"hello")
        self.assertEqual(upstream.recv(5), b"hello")
        upstream.sendall(b"world")
        self.assertEqual(client.recv(5), b"world")
        client.close()
        upstream.close()

    def test_large_bidirectional_relay_preserves_data_and_half_closes(self):
        process = self.start()
        connection = self.abush / "browser-remotes" / local.REMOTE_KEY / local.CONNECTION_ID
        published = connection / "browser-peer.sock"
        self.wait_for(connection / "connection.json")
        self.assertIsNone(process.poll())

        client = self.socket()
        client.settimeout(5)
        local.connect_socket(client, published)
        upstream = self.accept()
        upstream.settimeout(5)
        client.setsockopt(socket.SOL_SOCKET, socket.SO_SNDBUF, 4096)
        upstream.setsockopt(socket.SOL_SOCKET, socket.SO_SNDBUF, 4096)
        from_client = os.urandom(local.RELAY_BUFFER_LIMIT * 3 + 137)
        from_upstream = os.urandom(local.RELAY_BUFFER_LIMIT * 2 + 251)
        received = {}

        def send_and_close(sock, payload):
            sock.sendall(payload)
            sock.shutdown(socket.SHUT_WR)

        def receive(name, sock):
            chunks = []
            while True:
                data = sock.recv(65536)
                if not data:
                    break
                chunks.append(data)
            received[name] = b"".join(chunks)

        self.run_workers([
            (send_and_close, (client, from_client)),
            (send_and_close, (upstream, from_upstream)),
            (receive, ("client", client)),
            (receive, ("upstream", upstream)),
        ], [client, upstream])
        self.assertEqual(received["client"], from_upstream)
        self.assertEqual(received["upstream"], from_client)
        client.close()
        upstream.close()

    def test_concurrent_connects_preserve_process_working_directory(self):
        process = self.start()
        connection = self.abush / "browser-remotes" / local.REMOTE_KEY / local.CONNECTION_ID
        published = connection / "browser-peer.sock"
        self.wait_for(connection / "connection.json")
        original = pathlib.Path.cwd()
        clients = []
        upstreams = []
        for _ in range(4):
            client = self.socket()
            local.connect_socket(client, published)
            upstream = self.accept()
            clients.append(client)
            upstreams.append(upstream)
        self.assertEqual(pathlib.Path.cwd(), original)
        for index, (client, upstream) in enumerate(zip(clients, upstreams)):
            payload = f"relay-{index}".encode()
            client.sendall(payload)
            self.assertEqual(upstream.recv(len(payload)), payload)
            client.close()
            upstream.close()
        self.assertEqual(pathlib.Path.cwd(), original)

    def test_target_liveness_waits_for_first_socket_and_bounds_cleanup(self):
        missing = self.runtime / "missing.sock"
        seen, since, expired = local.update_target_liveness(missing, False, None, 100)
        self.assertEqual((seen, since, expired), (False, None, False))
        seen, since, expired = local.update_target_liveness(missing, seen, since, 1000)
        self.assertEqual((seen, since, expired), (False, None, False))

        seen, since, expired = local.update_target_liveness(missing, True, None, 1000)
        self.assertEqual((seen, since, expired), (True, 1000, False))
        seen, since, expired = local.update_target_liveness(
            missing, seen, since, 1000 + local.TARGET_MISSING_GRACE + 0.1)
        self.assertEqual((seen, since, expired), (True, 1000, True))

    def test_wait_ready_requires_live_owner_and_exact_registration(self):
        process = self.start()
        connection = self.abush / "browser-remotes" / local.REMOTE_KEY / local.CONNECTION_ID
        self.wait_for(connection / "connection.json")
        self.assertTrue(local.wait_ready(self.runtime, 0.1))
        data = json.loads((connection / "connection.json").read_text())
        data["owner_pid"] = 999999999
        (connection / "connection.json").write_text(json.dumps(data))
        (connection / "connection.json").chmod(0o600)
        self.assertFalse(local.wait_ready(self.runtime, 0.05))
        self.assertIsNone(process.poll())

    def create_connection_directory(self):
        root = self.abush / "browser-remotes"
        root.mkdir(mode=0o700)
        remote = root / local.REMOTE_KEY
        remote.mkdir(mode=0o700)
        connection = remote / local.CONNECTION_ID
        connection.mkdir(mode=0o700)
        return connection

    def test_metadata_temporary_symlink_is_rejected_without_truncating_target(self):
        connection = self.create_connection_directory()
        victim = self.runtime / "victim"
        victim.write_text("preserve me")
        (connection / "connection.tmp").symlink_to(victim)
        process = self.start()
        self.assertNotEqual(process.wait(timeout=2), 0)
        self.assertEqual(victim.read_text(), "preserve me")

    def test_stale_regular_metadata_temporary_is_replaced(self):
        connection = self.create_connection_directory()
        temporary = connection / "connection.tmp"
        temporary.write_text("stale")
        temporary.chmod(0o600)
        process = self.start()
        self.wait_for(connection / "connection.json")
        self.assertIsNone(process.poll())
        self.assertFalse(temporary.exists())

    def test_stale_socket_is_replaced_and_symlink_is_rejected(self):
        root = self.abush / "browser-remotes"
        root.mkdir(mode=0o700)
        remote = root / local.REMOTE_KEY
        remote.mkdir(mode=0o700)
        connection = remote / local.CONNECTION_ID
        connection.mkdir(mode=0o700)
        stale = self.socket()
        local.bind_socket(stale, connection / "browser-peer.sock")
        stale.listen(1)
        stale.close()
        process = self.start()
        deadline = time.monotonic() + 2
        while time.monotonic() < deadline and process.poll() is None:
            if (connection / "connection.json").exists():
                break
            time.sleep(0.01)
        if process.poll() is not None:
            self.fail(process.communicate(timeout=2)[1].decode())
        self.wait_for(connection / "connection.json")
        self.assertIsNone(process.poll())

        process.terminate()
        process.wait(timeout=2)
        published = connection / "browser-peer.sock"
        try:
            published.unlink()
        except FileNotFoundError:
            pass
        published.symlink_to(self.target)
        rejected = self.start()
        self.assertNotEqual(rejected.wait(timeout=2), 0)


def stat_mode(path):
    return path.stat().st_mode & 0o777


if __name__ == "__main__":
    unittest.main()
