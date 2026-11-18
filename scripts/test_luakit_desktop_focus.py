#!/usr/bin/env python3
import importlib.util
import json
import os
import pathlib
import queue
import select
import signal
import socket
import subprocess
import sys
import threading
import time
import unittest
from unittest import mock

import test_ordinary_open as ordinary_tests

host = ordinary_tests.host
track_server = ordinary_tests.track_server

ROOT = pathlib.Path(__file__).resolve().parents[1]
CLIENT = ROOT / "scripts/ordinary-open-client.py"
spec = importlib.util.spec_from_file_location("desktop_ordinary_client", CLIENT)
client = importlib.util.module_from_spec(spec)
spec.loader.exec_module(client)
REAL_DESKTOP_FOCUS = client.focus_browser_desktop
FOCUS_COMMAND = [[["group", "f"]], "toscreen", [], {}, True]


class QtileFixture:
    def __init__(self, test, path, response=b"[0,null]", before_reply=None, handler=None):
        self.test, self.path = test, path
        self.response, self.before_reply = response, before_reply
        self.handler = handler
        self.requests = queue.Queue()
        self.errors = queue.Queue()
        self.listener = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.listener.bind(str(path))
        path.chmod(0o600)
        self.listener.listen()
        self.stop_read, self.stop_write = socket.socketpair()
        self.thread = threading.Thread(target=self.run, daemon=True)
        self.thread.start()
        test.addCleanup(self.close)

    def run(self):
        try:
            while True:
                ready, _, _ = select.select([self.listener, self.stop_read], [], [], 3)
                if self.stop_read in ready:
                    return
                if self.listener not in ready:
                    continue
                connection, _ = self.listener.accept()
                with connection:
                    connection.settimeout(2)
                    request = bytearray()
                    while chunk := connection.recv(4096):
                        request.extend(chunk)
                        if len(request) > 65536:
                            raise AssertionError("oversized Qtile request")
                    self.requests.put(json.loads(request))
                    try:
                        if self.before_reply is not None:
                            self.before_reply()
                        response = self.handler(bytes(request)) if self.handler else self.response
                        connection.sendall(response)
                    except (BrokenPipeError, ConnectionResetError):
                        pass
        except BaseException:
            self.errors.put(sys.exc_info())

    def close(self):
        self.stop_write.sendall(b"1")
        self.thread.join(timeout=3)
        for connection in (self.listener, self.stop_read, self.stop_write):
            connection.close()
        self.test.assertFalse(self.thread.is_alive(), "owned Qtile fixture did not stop")
        if not self.errors.empty():
            _, error, traceback = self.errors.get_nowait()
            raise error.with_traceback(traceback)


class LuakitDesktopFocusTest(unittest.TestCase):
    def setUp(self):
        self.launcher = ordinary_tests.LauncherTest()
        self.launcher.setUp()
        self.addCleanup(self.launcher.doCleanups)
        self.launcher.fake_client.write_text(CLIENT.read_text())
        self.outbound = queue.Queue()
        self.server = track_server(self, host.OrdinaryOpenServer(
            self.outbound, path=self.launcher.base / "o", timeout=2))
        self.server.start()
        self.qtile = QtileFixture(self, self.launcher.base / "q")

    def environment(self, running=True):
        environment = self.launcher.environment()
        environment.update({
            "ABUSH_CHROMIUM_LAUNCHER": str(ROOT / "scripts/chromium"),
            "ABUSH_ORDINARY_OPEN_SOCKET": str(self.server.path),
            "ABUSH_QTILE_SOCKET": str(self.qtile.path),
            "ABUSH_ORDINARY_OPEN_TIMEOUT": "5",
        })
        if running:
            process = self.launcher.proc / "42"
            process.mkdir(exist_ok=True)
            (process / "cmdline").write_bytes(
                b"chrome\0--user-data-dir=" + os.fsencode(self.launcher.base / "profile") + b"\0")
        return environment

    def start(self, arguments, running=True, extra_environment=None):
        environment = {**self.environment(running), **(extra_environment or {})}
        process = subprocess.Popen(arguments, env=environment,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

        def cleanup():
            if process.poll() is None:
                process.terminate()
            try:
                process.communicate(timeout=2)
            except subprocess.TimeoutExpired:
                process.kill()
                process.communicate(timeout=1)
                self.fail("owned launcher did not stop")

        self.addCleanup(cleanup)
        return process

    def complete(self, request, completion="applied"):
        self.server.complete(host.ordinary_open_response(request["request_id"], completion))

    def test_warm_luakit_switches_f_only_after_one_successful_open(self):
        process = self.start([ROOT / "scripts/luakit", "https://example.test/luakit"])
        request = self.outbound.get(timeout=2)["payload"]
        self.assertTrue(self.qtile.requests.empty(), "desktop switch must follow terminal success")
        self.complete(request)
        _, error = process.communicate(timeout=3)
        self.assertEqual(process.returncode, 0, error)
        self.assertEqual(self.qtile.requests.get(timeout=0.2), FOCUS_COMMAND)
        self.assertTrue(self.qtile.requests.empty())
        self.assertTrue(self.outbound.empty(), "ordinary URL is submitted exactly once")
        self.assertFalse(self.launcher.log.exists())
        self.assertFalse(self.launcher.update_log.exists())

    def test_cold_luakit_keeps_lease_identity_and_url_out_of_chrome_argv(self):
        url = "https://example.test/cold-private"
        process = self.start([ROOT / "scripts/luakit", url], running=False)
        request = self.outbound.get(timeout=2)["payload"]
        self.assertEqual(request["url"], url)
        self.assertTrue(self.qtile.requests.empty())
        self.complete(request)
        _, error = process.communicate(timeout=3)
        self.assertEqual(process.returncode, 0, error)
        self.assertEqual(self.qtile.requests.get(timeout=0.2), FOCUS_COMMAND)
        self.assertNotIn(url.encode(), self.launcher.log.read_bytes().split(b"\0"))
        self.assertEqual(self.launcher.update_log.read_text(), str(self.launcher.base / "profile"))
        self.assertTrue(self.outbound.empty())

    def test_current_group_url_never_contacts_qtile(self):
        process = self.start(
            [ROOT / "scripts/chromium", "--current-group", "https://example.test/current"],
            extra_environment={"ABUSH_ORDINARY_OPEN_FOCUS_DESKTOP": "1"},
        )
        request = self.outbound.get(timeout=2)["payload"]
        self.complete(request)
        _, error = process.communicate(timeout=3)
        self.assertEqual(process.returncode, 0, error)
        self.assertTrue(self.qtile.requests.empty())

    def test_local_file_path_preserves_exact_uri_before_desktop_switch(self):
        path = self.launcher.base / "file with space #1.html"
        path.write_text("<title>Offline ordinary file</title>")
        process = self.start([ROOT / "scripts/luakit", str(path)])
        request = self.outbound.get(timeout=2)["payload"]
        self.assertEqual(request["url"], path.as_uri())
        self.assertTrue(self.qtile.requests.empty())
        self.complete(request)
        _, error = process.communicate(timeout=3)
        self.assertEqual(process.returncode, 0, error)
        self.assertEqual(self.qtile.requests.get(timeout=0.2), FOCUS_COMMAND)
        self.assertTrue(self.qtile.requests.empty())
        self.assertTrue(self.outbound.empty())

    def test_failed_or_unknown_open_never_switches_desktop_or_retries(self):
        for completion in ("not_applied", "unknown"):
            with self.subTest(completion=completion):
                process = self.start(
                    [ROOT / "scripts/luakit", f"https://example.test/{completion}"])
                request = self.outbound.get(timeout=2)["payload"]
                self.complete(request, completion)
                _, error = process.communicate(timeout=3)
                self.assertNotEqual(process.returncode, 0)
                self.assertIn(completion, error)
                self.assertTrue(self.qtile.requests.empty())
                self.assertTrue(self.outbound.empty())
                self.assertFalse(self.launcher.log.exists())

    def test_focus_failure_reports_already_opened_without_repeating_navigation(self):
        self.qtile.response = b'[1,"No such group"]'
        process = self.start([ROOT / "scripts/luakit", "https://example.test/focus-failure"])
        request = self.outbound.get(timeout=2)["payload"]
        self.complete(request)
        _, error = process.communicate(timeout=3)
        self.assertNotEqual(process.returncode, 0)
        self.assertIn("URL opened", error)
        self.assertIn("desktop switch failed", error)
        self.assertEqual(self.qtile.requests.get(timeout=0.2), FOCUS_COMMAND)
        self.assertTrue(self.outbound.empty())
        self.assertTrue(self.qtile.requests.empty())

    def test_cancelling_launcher_pid_closes_its_request_without_late_focus(self):
        process = self.start([ROOT / "scripts/luakit", "https://example.test/cancel"])
        request = self.outbound.get(timeout=2)["payload"]
        process.send_signal(signal.SIGTERM)
        process.communicate(timeout=2)
        self.assertEqual(process.returncode, -signal.SIGTERM)
        cancelled = self.outbound.get(timeout=2)
        self.assertEqual(cancelled["event"], "ordinary_open_cancel")
        self.assertEqual(cancelled["payload"]["request_id"], request["request_id"])
        self.complete(request)
        self.assertTrue(self.qtile.requests.empty())

    def test_lost_desktop_receipt_never_retries_the_open_or_desktop_effect(self):
        self.qtile.response = b""
        process = self.start([ROOT / "scripts/luakit", "https://example.test/lost-desktop-receipt"])
        request = self.outbound.get(timeout=2)["payload"]
        self.complete(request)
        _, error = process.communicate(timeout=3)
        self.assertNotEqual(process.returncode, 0)
        self.assertIn("URL opened", error)
        self.assertIn("Do not retry the URL automatically", error)
        self.assertEqual(self.qtile.requests.get(timeout=0.2), FOCUS_COMMAND)
        self.assertTrue(self.qtile.requests.empty())
        self.assertTrue(self.outbound.empty())

    def test_signal_while_desktop_receipt_is_held_exits_without_repeat(self):
        release = threading.Event()
        self.qtile.before_reply = lambda: release.wait(2)
        process = self.start([ROOT / "scripts/luakit", "https://example.test/cancel-desktop"])
        request = self.outbound.get(timeout=2)["payload"]
        self.complete(request)
        try:
            self.assertEqual(self.qtile.requests.get(timeout=2), FOCUS_COMMAND)
            process.send_signal(signal.SIGTERM)
            process.communicate(timeout=2)
            self.assertEqual(process.returncode, -signal.SIGTERM)
        finally:
            release.set()
        self.assertTrue(self.qtile.requests.empty())
        self.assertTrue(self.outbound.empty())

    def test_no_argument_launch_switches_desktop_before_native_chrome_without_url(self):
        def before_reply():
            self.assertFalse(self.launcher.log.exists(), "desktop receipt must precede launch")

        self.qtile.before_reply = before_reply
        process = self.start([ROOT / "scripts/luakit"], running=False)
        _, error = process.communicate(timeout=3)
        self.assertEqual(process.returncode, 0, error)
        self.assertEqual(self.qtile.requests.get(timeout=0.2), FOCUS_COMMAND)
        self.assertTrue(self.outbound.empty())
        self.assertIn(b"--restore-last-session", self.launcher.log.read_bytes().split(b"\0"))
        self.assertEqual(self.launcher.update_log.read_text(), str(self.launcher.base / "profile"))

    def test_no_argument_focus_failure_is_visible_without_starting_chrome(self):
        self.qtile.response = b'[2,"command exception"]'
        process = self.start([ROOT / "scripts/luakit"])
        _, error = process.communicate(timeout=3)
        self.assertNotEqual(process.returncode, 0)
        self.assertIn("Qtile desktop switch command failed", error)
        self.assertFalse(self.launcher.log.exists())
        self.assertTrue(self.outbound.empty())

    def test_no_argument_current_group_bypasses_desktop_ipc(self):
        process = self.start([ROOT / "scripts/chromium", "--current-group"])
        _, error = process.communicate(timeout=3)
        self.assertEqual(process.returncode, 0, error)
        self.assertTrue(self.qtile.requests.empty())
        self.assertTrue(self.outbound.empty())
        self.assertTrue(self.launcher.log.is_file())


class ClientDesktopBoundaryTest(unittest.TestCase):
    def setUp(self):
        self.base = ordinary_tests.temporary_directory(self)
        self.request_id = "a" * 32
        self.url = "https://example.test/desktop-boundary"
        self.applied = host.ordinary_open_response(self.request_id, "applied")
        environment = {
            "XDG_RUNTIME_DIR": str(self.base / "runtime"),
            "ABUSH_CHROME_PROFILE": str(self.base / "profile"),
            "ABUSH_ORDINARY_OPEN_REQUEST_ID": self.request_id,
            "ABUSH_ORDINARY_OPEN_TIMEOUT": "5",
            "ABUSH_ORDINARY_OPEN_FOCUS_DESKTOP": "1",
        }
        self.enterContext(mock.patch.dict(os.environ, environment, clear=True))
        self.enterContext(mock.patch.object(client.sys, "argv", [str(CLIENT), self.url]))
        self.connection = mock.MagicMock()
        self.connection.__enter__.return_value = self.connection
        self.connect = self.enterContext(mock.patch.object(
            client, "connect", return_value=self.connection))
        self.receive = self.enterContext(mock.patch.object(
            client, "receive", return_value=self.applied))
        self.focus = self.enterContext(mock.patch.object(client, "focus_browser_desktop"))

    def assert_one_submission(self):
        self.connect.assert_called_once()
        self.connection.sendall.assert_called_once()
        frame = self.connection.sendall.call_args.args[0]
        self.assertEqual(client.struct.unpack(">I", frame[:4])[0], len(frame) - 4)
        request = json.loads(frame[4:])
        self.assertEqual(request["request_id"], self.request_id)
        self.assertEqual(request["url"], self.url)

    def test_only_explicit_launcher_opt_in_switches_after_applied(self):
        for opt_in in (None, "0", "1"):
            with self.subTest(opt_in=opt_in):
                self.connect.reset_mock()
                self.connection.sendall.reset_mock()
                self.focus.reset_mock()
                if opt_in is None:
                    os.environ.pop("ABUSH_ORDINARY_OPEN_FOCUS_DESKTOP", None)
                else:
                    os.environ["ABUSH_ORDINARY_OPEN_FOCUS_DESKTOP"] = opt_in
                client.main()
                self.assert_one_submission()
                self.assertEqual(self.focus.call_count, opt_in == "1")
                if opt_in == "1":
                    deadline = self.focus.call_args.args[0]
                    self.assertGreater(deadline, time.monotonic())
                    self.assertLessEqual(deadline, time.monotonic() + 5)

    def test_invalid_or_mismatched_completions_never_switch_or_resubmit(self):
        for update in (
            {"version": 2}, {"version": True}, {"version": 1.0},
            {"request_id": "b" * 32}, {"operation": "accepted"},
            {"completion": "accepted"}, {"completion": []}, {"extra": True},
        ):
            with self.subTest(update=update):
                self.connect.reset_mock()
                self.connection.sendall.reset_mock()
                self.focus.reset_mock()
                self.receive.return_value = {**self.applied, **update}
                with self.assertRaisesRegex(RuntimeError, "invalid ordinary-open completion"):
                    client.main()
                self.focus.assert_not_called()
                self.assert_one_submission()

    def test_incomplete_open_receipts_never_switch_or_resubmit(self):
        for error in (EOFError("lost reply"), TimeoutError("reply deadline"),
                      OSError("connection failed"), ValueError("bad JSON")):
            with self.subTest(error=error):
                self.connect.reset_mock()
                self.connection.sendall.reset_mock()
                self.receive.side_effect = error
                with self.assertRaises(type(error)):
                    client.main()
                self.focus.assert_not_called()
                self.assert_one_submission()

    def test_applied_after_caller_deadline_never_dispatches_desktop_effect(self):
        clock = [1000.0]
        os.environ["ABUSH_ORDINARY_OPEN_DEADLINE_MS"] = "1005000"

        def complete(_connection):
            clock[0] = 1005.1
            return self.applied

        self.receive.side_effect = complete
        self.focus.side_effect = REAL_DESKTOP_FOCUS
        with mock.patch.object(client.time, "time", side_effect=lambda: clock[0]), \
                mock.patch.object(client.time, "monotonic", return_value=50), \
                mock.patch.object(client.socket, "socket") as create:
            with self.assertRaisesRegex(RuntimeError, "URL opened; desktop switch failed"):
                client.main()
        create.assert_not_called()
        self.assert_one_submission()

    def test_cold_lease_only_never_submits_or_switches_desktop(self):
        metadata = self.base / "metadata.json"
        os.environ.update({
            "ABUSH_ORDINARY_OPEN_WRITE_LEASE": "1",
            "ABUSH_ORDINARY_OPEN_LEASE_ONLY": "1",
            "ABUSH_ORDINARY_OPEN_LEASE_METADATA": str(metadata),
        })
        client.main()
        self.connect.assert_not_called()
        self.focus.assert_not_called()
        lease = pathlib.Path(json.loads(metadata.read_text())["lease_path"])
        self.assertEqual(json.loads(lease.read_text())["url"], self.url)


class DesktopIPCUnitTest(unittest.TestCase):
    def setUp(self):
        self.base = ordinary_tests.temporary_directory(self)
        self.qtile = QtileFixture(self, self.base / "q")
        self.environment = mock.patch.dict(os.environ, {"ABUSH_QTILE_SOCKET": str(self.qtile.path)})
        self.environment.start()
        self.addCleanup(self.environment.stop)

    def connection(self, chunks):
        connection = mock.MagicMock()
        connection.__enter__.return_value = connection
        connection.getsockopt.return_value = client.struct.pack("3i", 4242, os.getuid(), os.getgid())
        connection.recv.side_effect = chunks
        return connection

    def test_exact_group_command_uses_real_json_ipc_eof_and_success_reply(self):
        client.focus_browser_desktop()
        self.assertEqual(self.qtile.requests.get(timeout=0.2), FOCUS_COMMAND)
        self.assertTrue(self.qtile.requests.empty())

    def test_fragmented_reply_is_assembled_before_success(self):
        connection = self.connection([b"[0", b",", b"nu", b"ll]", b""])
        with mock.patch.object(client.socket, "socket", return_value=connection):
            client.focus_browser_desktop()
        self.assertEqual(json.loads(connection.sendall.call_args.args[0]), FOCUS_COMMAND)
        connection.shutdown.assert_called_once_with(socket.SHUT_WR)
        self.assertEqual(connection.recv.call_count, 5)

    def test_command_error_malformed_and_oversized_replies_fail_closed_once(self):
        for response in (
            b'[1,"No such group"]', b'[2,"Exception"]', b"[true,null]", b"[0.0,null]",
            b"[]", b"[0]", b"[0,null,0]", b'{"status":0}', b"null", b"[0,true]",
            b"", b"[0,", b"not-json", b"\xff", b" " * (client.QTILE_MAX_REPLY + 1),
        ):
            with self.subTest(response=response[:40]):
                connection = self.connection([response, b""])
                with mock.patch.object(client.socket, "socket", return_value=connection):
                    with self.assertRaises((RuntimeError, ValueError)):
                        client.focus_browser_desktop()
                connection.sendall.assert_called_once()

    def test_reply_without_eof_or_transport_error_is_not_retried(self):
        for mode in ("no eof", "connect", "send"):
            with self.subTest(mode=mode):
                connection = self.connection([b"[0,null]", TimeoutError("reply did not finish")])
                if mode == "connect":
                    connection.connect.side_effect = ConnectionRefusedError("offline Qtile")
                elif mode == "send":
                    connection.sendall.side_effect = OSError("send outcome unknown")
                with mock.patch.object(client.socket, "socket", return_value=connection) as create:
                    with self.assertRaises(OSError):
                        client.focus_browser_desktop()
                create.assert_called_once()
                connection.connect.assert_called_once()
                self.assertEqual(connection.sendall.call_count, mode != "connect")
                if mode == "no eof":
                    self.assertEqual(connection.recv.call_count, 2)

    def test_installed_qtile_dispatches_the_fixed_json_command_without_gui(self):
        if importlib.util.find_spec("libqtile") is None:
            self.skipTest("Qtile is not installed")
        from libqtile.command.base import CommandObject
        from libqtile.command.interface import IPCCommandServer
        from libqtile.group import _Group
        from libqtile.ipc import _IPC
        from types import SimpleNamespace

        group = _Group("f")
        trace = []
        screen = SimpleNamespace(group=object())

        def set_group(target):
            trace.append(("group", target.name))
            screen.group = target

        screen.set_group = mock.Mock(side_effect=set_group)
        core = SimpleNamespace(check_screen_fullscreen_background=mock.Mock(
            side_effect=lambda _screen: trace.append(("background", None))))

        class OfflineQtile(CommandObject):
            locked = False
            groups_map = {"f": group}
            current_screen = screen

            def _items(self, name):
                if name == "group":
                    return False, list(self.groups_map)

            def _select(self, name, selector):
                if name == "group":
                    return self.groups_map.get(selector)

        manager = OfflineQtile()
        manager.core = core
        group.qtile = manager
        dispatcher = IPCCommandServer(manager)

        def handle(data):
            request, is_json = _IPC.unpack(data)
            self.assertTrue(is_json)
            return _IPC.pack(dispatcher.call(request), is_json=is_json)

        self.qtile.handler = handle
        client.focus_browser_desktop()
        self.assertEqual(self.qtile.requests.get(timeout=0.2), FOCUS_COMMAND)
        self.assertEqual(trace, [("group", "f"), ("background", None)])
        trace.clear()
        client.focus_browser_desktop()
        self.assertEqual(self.qtile.requests.get(timeout=0.2), FOCUS_COMMAND)
        self.assertEqual(trace, [("background", None)], "toscreen must not toggle f away")
        for mode in ("missing group", "locked"):
            with self.subTest(mode=mode):
                trace.clear()
                manager.groups_map = {} if mode == "missing group" else {"f": group}
                manager.locked = mode == "locked"
                with self.assertRaisesRegex(RuntimeError, "command failed"):
                    client.focus_browser_desktop()
                self.assertEqual(self.qtile.requests.get(timeout=0.2), FOCUS_COMMAND)
                self.assertEqual(trace, [])
        self.assertTrue(self.qtile.requests.empty())

    def test_expired_deadline_does_not_create_or_send_any_socket(self):
        with mock.patch.object(client.socket, "socket") as create:
            with self.assertRaises(TimeoutError):
                client.focus_browser_desktop(time.monotonic() - 1)
        create.assert_not_called()

    def test_trickle_reply_cannot_extend_the_single_absolute_budget(self):
        clock = [10.0]
        connection = self.connection([])

        def trickle(_size):
            clock[0] += 0.6
            return b" "

        connection.recv.side_effect = trickle
        with mock.patch.object(client.time, "monotonic", side_effect=lambda: clock[0]), \
                mock.patch.object(client.socket, "socket", return_value=connection):
            with self.assertRaises(TimeoutError):
                client.focus_browser_desktop()
        self.assertEqual(connection.recv.call_count, 4)
        self.assertLessEqual(max(call.args[0] for call in connection.settimeout.call_args_list), 2)
        self.assertLess(connection.settimeout.call_args_list[-1].args[0], 0.3)

    def test_peer_mismatch_or_socket_replacement_is_rejected_before_send(self):
        for mode in ("peer", "replacement", "connect timeout"):
            with self.subTest(mode=mode):
                connection = self.connection([b"[0,null]", b""])
                clock = [10.0]
                if mode == "peer":
                    connection.getsockopt.return_value = client.struct.pack(
                        "3i", 4242, os.getuid() + 1, os.getgid())
                if mode == "replacement":
                    connection.connect.side_effect = lambda _: self.qtile.path.rename(
                        self.base / "old")
                if mode == "connect timeout":
                    connection.connect.side_effect = lambda _: clock.__setitem__(0, 12.0)
                with mock.patch.object(client.time, "monotonic", side_effect=lambda: clock[0]), \
                        mock.patch.object(client.socket, "socket", return_value=connection):
                    with self.assertRaises((OSError, RuntimeError, TimeoutError)):
                        client.focus_browser_desktop()
                connection.sendall.assert_not_called()
                if mode == "replacement":
                    (self.base / "old").rename(self.qtile.path)

    def test_unsafe_socket_paths_never_connect(self):
        regular = self.base / "file"
        regular.write_text("untouched")
        symlink = self.base / "link"
        symlink.symlink_to(self.qtile.path)
        for path in (regular, symlink, self.base / "missing", pathlib.Path("relative.sock")):
            with self.subTest(path=path), mock.patch.dict(os.environ, {"ABUSH_QTILE_SOCKET": str(path)}), \
                    mock.patch.object(client.socket, "socket") as create:
                with self.assertRaises((OSError, RuntimeError)):
                    client.focus_browser_desktop()
                create.assert_not_called()
        self.qtile.path.chmod(0o660)
        with mock.patch.object(client.socket, "socket") as create:
            with self.assertRaisesRegex(RuntimeError, "unsafe"):
                client.focus_browser_desktop()
        create.assert_not_called()

    def test_socket_lookup_respects_cache_display_and_fallback_order(self):
        environment = {"HOME": str(self.base), "XDG_CACHE_HOME": str(self.base / "xdg"),
                       "DISPLAY": ":9", "WAYLAND_DISPLAY": "wayland-7"}
        with mock.patch.dict(os.environ, environment, clear=True):
            self.assertEqual(client.desktop_socket_path(),
                             self.base / "xdg/qtile/qtilesocket.wayland-7")
            del os.environ["WAYLAND_DISPLAY"]
            self.assertEqual(client.desktop_socket_path(), self.base / "xdg/qtile/qtilesocket.:9")
            del os.environ["DISPLAY"]
            fallback = self.base / "xdg/qtile/qtilesocket.:0"
            fallback.parent.mkdir(parents=True)
            fallback.touch()
            self.assertEqual(client.desktop_socket_path(), fallback)


if __name__ == "__main__":
    unittest.main()
