#!/usr/bin/env python3

import asyncio
import os
import signal
import stat
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
import preview_gateway as preview


async def read_headers(reader):
    return await reader.readuntil(b"\r\n\r\n")


class PreviewGatewayTests(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        os.chmod(self.root, 0o700)
        self.control = self.root / "control.sock"
        self.http = self.root / "http.sock"
        self.gateway = preview.Gateway(self.control, self.http)
        await self.gateway.start()
        self.backend = await asyncio.start_server(self.handle_backend, "127.0.0.1", 0)
        self.port = self.backend.sockets[0].getsockname()[1]
        self.lease_reader = None
        self.lease_writer = None
        self.backend_requests = []
        self.backend_connections = 0

    async def asyncTearDown(self):
        if self.lease_writer:
            self.lease_writer.close()
            await self.lease_writer.wait_closed()
        self.backend.close()
        await self.backend.wait_closed()
        await self.gateway.close()
        self.temp.cleanup()

    async def handle_backend(self, reader, writer):
        self.backend_connections += 1
        try:
            headers = await read_headers(reader)
            self.backend_requests.append(headers)
            line = headers.split(b"\r\n", 1)[0]
            target = line.split(b" ")[1]
            lower = headers.lower()
            if target == b"/head":
                writer.write(b"HTTP/1.1 200 OK\r\nContent-Length: 7\r\n\r\n")
                await writer.drain()
                return
            if target == b"/204":
                writer.write(b"HTTP/1.1 204 No Content\r\n\r\n")
                await writer.drain()
                return
            if target == b"/304":
                writer.write(b"HTTP/1.1 304 Not Modified\r\nContent-Length: 9\r\n\r\n")
                await writer.drain()
                return
            if target == b"/info":
                writer.write(b"HTTP/1.1 103 Early Hints\r\nLink: </x>; rel=preload\r\n\r\n"
                             b"HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nOK")
                await writer.drain()
                return
            if b"upgrade: websocket" in lower:
                writer.write(b"HTTP/1.1 101 Switching Protocols\r\n"
                             b"Connection: Upgrade\r\nUpgrade: websocket\r\n"
                             b"Sec-WebSocket-Accept: accepted\r\n\r\n")
                await writer.drain()
                while data := await reader.read(65536):
                    writer.write(data)
                    await writer.drain()
                return
            if b"transfer-encoding: chunked" in lower:
                body = bytearray()
                while True:
                    size = int((await reader.readline()).split(b";", 1)[0], 16)
                    if not size:
                        await reader.readline()
                        break
                    body.extend(await reader.readexactly(size))
                    await reader.readexactly(2)
            else:
                marker = b"content-length:"
                length = 0
                for header in lower.split(b"\r\n"):
                    if header.startswith(marker):
                        length = int(header.split(b":", 1)[1])
                body = bytearray(await reader.readexactly(length)) if length else bytearray()
            payload = line + b"|" + bytes(body)
            writer.write(b"HTTP/1.1 200 OK\r\nContent-Length: " + str(len(payload)).encode()
                         + b"\r\nConnection: close\r\n\r\n" + payload)
            await writer.drain()
        except (ValueError, asyncio.IncompleteReadError, ConnectionError):
            pass
        finally:
            writer.close()
            await writer.wait_closed()

    async def register(self, names=("app",)):
        response, self.lease_reader, self.lease_writer = await preview.control_request(
            self.control, {"version": 1, "op": "register", "names": list(names),
                           "port": self.port}, keep=True)
        self.assertTrue(response["ok"])

    async def request(self, data):
        reader, writer = await asyncio.open_unix_connection(self.http)
        writer.write(data)
        await writer.drain()
        result = await reader.read()
        writer.close()
        await writer.wait_closed()
        return result

    async def test_control_lease_conflict_and_release(self):
        await self.register(("app", "alternate"))
        response = await preview.control_request(
            self.control, {"version": 1, "op": "register", "names": ["app", "unused"],
                           "port": self.port})
        self.assertEqual(response["code"], "name_in_use")
        listing = await preview.control_request(self.control, {"version": 1, "op": "list"})
        self.assertEqual([x["name"] for x in listing["routes"]], ["alternate", "app"])
        self.lease_writer.close()
        await self.lease_writer.wait_closed()
        self.lease_writer = None
        for _ in range(20):
            if not self.gateway.routes:
                break
            await asyncio.sleep(0.01)
        result = await self.request(b"GET / HTTP/1.1\r\nHost: app.localhost:17890\r\n\r\n")
        self.assertTrue(result.startswith(b"HTTP/1.1 404"))

    async def test_get_post_and_chunked(self):
        await self.register()
        result = await self.request(b"GET /hello HTTP/1.1\r\nHost: app.localhost\r\n\r\n")
        self.assertIn(b"GET /hello HTTP/1.1|", result)
        result = await self.request(
            b"POST /fixed HTTP/1.1\r\nHost: app.localhost:17890\r\n"
            b"Content-Length: 4\r\nContent-Length: 4\r\n\r\ndata")
        self.assertIn(b"POST /fixed HTTP/1.1|data", result)
        forwarded = self.backend_requests[-1].lower()
        self.assertEqual(forwarded.count(b"content-length:"), 1)
        result = await self.request(
            b"POST /chunk HTTP/1.1\r\nHost: app.localhost\r\nTransfer-Encoding: chunked\r\n"
            b"Trailer: X-Checksum\r\n\r\n3;foo=bar\r\nabc\r\n2\r\nde\r\n"
            b"0\r\nX-Checksum: yes\r\n\r\n")
        self.assertIn(b"POST /chunk HTTP/1.1|abcde", result)

    async def test_strict_transfer_encoding_and_chunks(self):
        await self.register()
        for transfer in (b"gzip, chunked", b"chunked, chunked", b"gzip", b"chunked, gzip"):
            result = await self.request(
                b"POST / HTTP/1.1\r\nHost: app.localhost\r\nTransfer-Encoding: "
                + transfer + b"\r\n\r\n")
            self.assertTrue(result.startswith(b"HTTP/1.1 400"), transfer)
        for chunk in (b"+1\r\na\r\n0\r\n\r\n", b"0x1\r\na\r\n0\r\n\r\n",
                      b" 1\r\na\r\n0\r\n\r\n", b"1 \r\na\r\n0\r\n\r\n",
                      b"1\r\naXX0\r\n\r\n", b"0\r\nUndeclared: x\r\n\r\n"):
            result = await self.request(
                b"POST / HTTP/1.1\r\nHost: app.localhost\r\n"
                b"Transfer-Encoding: chunked\r\n\r\n" + chunk)
            self.assertTrue(result.startswith(b"HTTP/1.1 400"), chunk)

    async def test_head_no_body_statuses_and_informational(self):
        await self.register()
        result = await self.request(b"HEAD /head HTTP/1.1\r\nHost: app.localhost\r\n\r\n")
        self.assertIn(b"Content-Length: 7", result)
        self.assertTrue(result.endswith(b"\r\n\r\n"))
        result = await self.request(b"GET /204 HTTP/1.1\r\nHost: app.localhost\r\n\r\n")
        self.assertIn(b"204 No Content", result)
        self.assertNotIn(b"Content-Length:", result)
        result = await self.request(b"GET /304 HTTP/1.1\r\nHost: app.localhost\r\n\r\n")
        self.assertIn(b"304 Not Modified", result)
        self.assertIn(b"Content-Length: 9", result)
        result = await self.request(b"GET /info HTTP/1.1\r\nHost: app.localhost\r\n\r\n")
        self.assertIn(b"103 Early Hints", result)
        self.assertIn(b"HTTP/1.1 200 OK", result)
        self.assertTrue(result.endswith(b"OK"))

    async def test_expect_and_header_validation(self):
        await self.register()
        before = self.backend_connections
        result = await self.request(
            b"POST / HTTP/1.1\r\nHost: app.localhost\r\nExpect: 100-continue\r\n"
            b"Content-Length: 4\r\n\r\n")
        self.assertTrue(result.startswith(b"HTTP/1.1 417"))
        self.assertEqual(self.backend_connections, before)
        for request in (
                b"GET / HTTP/1.1\r\nHost: app.localhost\r\nBad: x\x01y\r\n\r\n",
                b"GET / HTTP/1.1\r\nHost: app.localhost\r\nConnection: Host\r\n\r\n",
                b"GE(T / HTTP/1.1\r\nHost: app.localhost\r\n\r\n"):
            result = await self.request(request)
            self.assertTrue(result.startswith(b"HTTP/1.1 400"))

    async def test_websocket_raw_relay_and_lease_close(self):
        await self.register()
        reader, writer = await asyncio.open_unix_connection(self.http)
        writer.write(b"GET /socket HTTP/1.1\r\nHost: app.localhost\r\n"
                     b"Connection: keep-alive, Foo, Upgrade\r\nFoo: secret\r\n"
                     b"Upgrade: websocket\r\nSec-WebSocket-Key: key\r\n"
                     b"Sec-WebSocket-Version: 13\r\n\r\n")
        await writer.drain()
        response = await read_headers(reader)
        self.assertIn(b"101 Switching Protocols", response)
        self.assertIn(b"Sec-WebSocket-Accept: accepted", response)
        forwarded = self.backend_requests[-1]
        self.assertNotIn(b"Foo: secret", forwarded)
        self.assertIn(b"Connection: Upgrade", forwarded)
        self.assertIn(b"Upgrade: websocket", forwarded)
        self.assertIn(b"Sec-WebSocket-Key: key", forwarded)
        payload = os.urandom(131072)
        writer.write(payload)
        await writer.drain()
        self.assertEqual(await reader.readexactly(len(payload)), payload)
        self.lease_writer.close()
        await self.lease_writer.wait_closed()
        self.lease_writer = None
        self.assertEqual(await asyncio.wait_for(reader.read(), 1), b"")
        writer.close()
        await writer.wait_closed()

    async def test_h2c_and_mismatched_upgrade_rejected(self):
        await self.register()
        result = await self.request(
            b"GET / HTTP/1.1\r\nHost: app.localhost\r\n"
            b"Connection: Upgrade\r\nUpgrade: h2c\r\n\r\n")
        self.assertTrue(result.startswith(b"HTTP/1.1 400"))
        response = preview.parse_message([
            b"HTTP/1.1 101 Switching Protocols", b"Connection: Upgrade", b"Upgrade: other"],
            request=False, request_method=b"GET")
        self.assertEqual(response.upgrade_protocol, b"other")

    async def test_bad_framing_and_backend_unavailable(self):
        await self.register()
        result = await self.request(
            b"POST / HTTP/1.1\r\nHost: app.localhost\r\nContent-Length: 1\r\n"
            b"Transfer-Encoding: chunked\r\n\r\n")
        self.assertTrue(result.startswith(b"HTTP/1.1 400"))
        self.backend.close()
        await self.backend.wait_closed()
        result = await self.request(b"GET / HTTP/1.1\r\nHost: app.localhost\r\n\r\n")
        self.assertTrue(result.startswith(b"HTTP/1.1 502"))


class DaemonLifecycleTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.runtime = Path(self.temp.name) / "runtime"
        self.state = Path(self.temp.name) / "state"
        self.runtime.mkdir(mode=0o700)
        self.state.mkdir(mode=0o700)
        self.env = os.environ.copy()
        self.env["XDG_RUNTIME_DIR"] = str(self.runtime)
        self.env["XDG_STATE_HOME"] = str(self.state)
        self.command = [sys.executable, str(Path(preview.__file__).resolve())]

    def tearDown(self):
        subprocess.run(self.command + ["gateway", "stop", "--force"], env=self.env,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=10)
        self.temp.cleanup()

    def run_preview(self, *args, check=True):
        return subprocess.run(self.command + list(args), env=self.env, text=True,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                              timeout=10, check=check)

    def test_concurrent_ensure_stop_and_sigkill_recovery(self):
        processes = [subprocess.Popen(
            self.command + ["gateway", "ensure", "--quiet"], env=self.env,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True) for _ in range(4)]
        for process in processes:
            stdout, stderr = process.communicate(timeout=10)
            self.assertEqual(process.returncode, 0, (stdout, stderr))
        self.run_preview("gateway", "status")
        pid_path = self.runtime / "preview-gateway" / "gateway.pid"
        pid = int(pid_path.read_text())
        os.kill(pid, signal.SIGKILL)
        for _ in range(100):
            try:
                os.kill(pid, 0)
            except ProcessLookupError:
                break
            time.sleep(0.01)
        self.run_preview("gateway", "ensure", "--quiet")
        replacement = int(pid_path.read_text())
        self.assertNotEqual(replacement, pid)
        self.run_preview("gateway", "stop")
        self.assertFalse((self.runtime / "preview-gateway" / "control.sock").exists())
        self.run_preview("gateway", "ensure", "--quiet")

    def test_expose_ctrl_c_releases_lease_and_stale_pid_is_never_killed(self):
        self.run_preview("gateway", "ensure", "--quiet")
        process = subprocess.Popen(
            self.command + ["expose", "65535", "ctrl-c"], env=self.env,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        for _ in range(100):
            listing = self.run_preview("list")
            if "ctrl-c.localhost" in listing.stdout:
                break
            time.sleep(0.02)
        else:
            self.fail("expose lease did not appear")
        process.send_signal(signal.SIGINT)
        stdout, stderr = process.communicate(timeout=5)
        self.assertEqual(process.returncode, 0, (stdout, stderr))
        for _ in range(100):
            if "ctrl-c.localhost" not in self.run_preview("list").stdout:
                break
            time.sleep(0.02)
        else:
            self.fail("Ctrl-C did not release expose lease")
        self.run_preview("gateway", "stop")
        pid_path = self.runtime / "preview-gateway" / "gateway.pid"
        pid_path.write_text(str(os.getpid()) + "\n")
        result = self.run_preview("gateway", "stop", "--force", check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("not running", result.stderr)


class RuntimeSecurityTests(unittest.TestCase):
    def test_runtime_permissions_and_symlink(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            old = os.environ.get("XDG_RUNTIME_DIR")
            try:
                os.environ["XDG_RUNTIME_DIR"] = str(root)
                os.chmod(root, 0o777)
                with self.assertRaises(RuntimeError):
                    preview.runtime_dir()
                os.chmod(root, 0o700)
                target = root / "target"
                target.mkdir()
                (root / "preview-gateway").symlink_to(target, target_is_directory=True)
                with self.assertRaises(RuntimeError):
                    preview.runtime_dir()
            finally:
                if old is None:
                    os.environ.pop("XDG_RUNTIME_DIR", None)
                else:
                    os.environ["XDG_RUNTIME_DIR"] = old

    def test_socket_modes(self):
        async def check():
            with tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                gateway = preview.Gateway(root / "control", root / "http")
                await gateway.start()
                try:
                    self.assertEqual(stat.S_IMODE((root / "control").stat().st_mode), 0o600)
                    self.assertEqual(stat.S_IMODE((root / "http").stat().st_mode), 0o600)
                finally:
                    await gateway.close()
        asyncio.run(check())


if __name__ == "__main__":
    unittest.main()
