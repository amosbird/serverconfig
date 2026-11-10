#!/usr/bin/env python3

import importlib.util
import json
import os
import pathlib
import socket
import subprocess
import sys
import threading
import time
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts/abush-browser-local.py"
spec = importlib.util.spec_from_file_location("abush_browser_local", SCRIPT)
local = importlib.util.module_from_spec(spec)
spec.loader.exec_module(local)


class LocalBrowserPublisherTest(unittest.TestCase):
    def setUp(self):
        runtime_base = ROOT / ".tmp" / f"local-{os.getpid()}-{id(self):x}"
        runtime_base.mkdir(mode=0o700)
        self.temporary = runtime_base
        self.runtime = runtime_base
        self.abush = self.runtime / "abush"
        self.abush.mkdir(mode=0o700)
        self.target = self.abush / "browser-peer.sock"
        self.server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.server.bind(str(self.target))
        os.chmod(self.target, 0o600)
        self.server.listen(1)
        self.processes = []

    def tearDown(self):
        for process in self.processes:
            process.terminate()
            try:
                process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=2)
            if process.stderr:
                process.stderr.close()
        self.server.close()
        import shutil
        shutil.rmtree(self.temporary)

    def start(self):
        process = subprocess.Popen(
            [sys.executable, str(SCRIPT)],
            env={**os.environ, "XDG_RUNTIME_DIR": str(self.runtime)},
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
        )
        self.processes.append(process)
        return process

    def wait_for(self, path):
        deadline = time.monotonic() + 2
        while time.monotonic() < deadline:
            if path.exists():
                return
            time.sleep(0.01)
        self.fail(f"timed out waiting for {path}")

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

        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        local.connect_socket(client, published)
        upstream, _ = self.server.accept()
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

        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        client.settimeout(5)
        local.connect_socket(client, published)
        upstream, _ = self.server.accept()
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

        threads = [
            threading.Thread(target=send_and_close, args=(client, from_client)),
            threading.Thread(target=send_and_close, args=(upstream, from_upstream)),
            threading.Thread(target=receive, args=("client", client)),
            threading.Thread(target=receive, args=("upstream", upstream)),
        ]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join(timeout=10)
            self.assertFalse(thread.is_alive(), "relay exchange timed out")
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
            client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            local.connect_socket(client, published)
            upstream, _ = self.server.accept()
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
        stale = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
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
            self.fail(process.stderr.read().decode())
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
