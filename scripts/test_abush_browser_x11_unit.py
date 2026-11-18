#!/usr/bin/env python3

import importlib.util
from contextlib import contextmanager
import json
import pathlib
import socket
import stat
import struct
import unittest
from types import SimpleNamespace
from unittest import mock

from libqtile.command.base import CommandObject, expose_command
from libqtile.command.interface import IPCCommandServer

ROOT = pathlib.Path(__file__).resolve().parents[1]
BINDING_PATH = ROOT / "chromium/external-links/x11_binding.py"
spec = importlib.util.spec_from_file_location("x11_binding", BINDING_PATH)
binding = importlib.util.module_from_spec(spec)
spec.loader.exec_module(binding)
qtile_spec = importlib.util.spec_from_file_location(
    "offline_qtile", ROOT / "scripts/test_abush_browser_qtile.py")
qtile = importlib.util.module_from_spec(qtile_spec)
qtile_spec.loader.exec_module(qtile)


class FakeAdapter:
    def __init__(self):
        self.values = {
            (11, "_NET_WM_NAME"): "ordinary",
            (11, "WM_CLASS"): "chromium\0Chromium",
            (22, "_NET_WM_NAME"): "another ordinary window",
            (22, "WM_CLASS"): "chromium\0Chromium",
        }
        self.finished = False

    def windows(self):
        return [11, 22]

    def title(self, window):
        return self.values.get((window, "_NET_WM_NAME"), "")

    def wm_class(self, window):
        return self.values.get((window, "WM_CLASS"), "").split("\0")

    def pid(self, _window):
        return 4242

    def set_text(self, window, name, value, property_type="UTF8_STRING"):
        self.values[(window, name)] = str(value)

    def property(self, window, name, property_type="UTF8_STRING"):
        return self.values.get((window, name), "")

    def finish(self):
        self.finished = True


class XcffibPropertyAdapterTest(unittest.TestCase):
    class Value:
        def __init__(self, raw):
            self.raw = raw

        def buf(self):
            return self.raw

    @staticmethod
    def adapter(raw, value_type, value_format):
        reply = mock.Mock(
            type=value_type,
            format=value_format,
            value=XcffibPropertyAdapterTest.Value(raw),
        )
        core = mock.Mock()
        core.GetProperty.return_value.reply.return_value = reply
        adapter = binding.XcffibAdapter.__new__(binding.XcffibAdapter)
        adapter.xproto = mock.Mock()
        adapter.xproto.Atom.Any = 0
        adapter.xproto.Atom.CARDINAL = 6
        adapter.xproto.Atom.WINDOW = 33
        adapter.core = core
        adapter.atom = mock.Mock(side_effect={
            "_NET_WM_PID": 100,
            "_NET_ACTIVE_WINDOW": 101,
            "UTF8_STRING": 102,
        }.get)
        adapter.root = 1
        return adapter

    def test_binary_cardinal_pid_and_window_are_read_as_32_bit_values(self):
        pid = self.adapter(struct.pack("=I", 424242), 6, 32)
        self.assertEqual(pid.pid(22), 424242)
        pid.core.GetProperty.assert_called_once_with(False, 22, 100, 6, 0, 4096)

        active = self.adapter(struct.pack("=I", 0x12345678), 33, 32)
        self.assertEqual(active.active_window(), 0x12345678)
        active.core.GetProperty.assert_called_once_with(False, 1, 101, 33, 0, 4096)

    def test_cardinal_type_or_width_mismatch_is_rejected(self):
        self.assertIsNone(self.adapter(struct.pack("=I", 42), 33, 32).pid(22))
        self.assertIsNone(self.adapter(b"42", 6, 8).pid(22))

    def test_browser_process_matches_exact_chrome_executable_not_argument_substrings(self):
        root = pathlib.Path(self._testMethodName)
        self.assertFalse(binding.browser_process(True, root),
                         "bool must not be accepted as a typed X11 PID")
        with mock.patch.object(binding.pathlib.Path, "read_bytes") as read_bytes:
            for executable in sorted(binding.BROWSER_EXECUTABLES):
                with self.subTest(format="nul", executable=executable):
                    read_bytes.return_value = (
                        f"/usr/bin/{executable}\0--user-data-dir=/profile\0".encode())
                    self.assertTrue(binding.browser_process(42, root))
                with self.subTest(format="packed", executable=executable):
                    read_bytes.return_value = (
                        f"/usr/bin/{executable} --user-data-dir=/profile".encode())
                    self.assertTrue(binding.browser_process(42, root))
            read_bytes.return_value = b"/opt/google/chrome/chrome --type=renderer"
            self.assertTrue(binding.browser_process(42, root),
                            "the X11 PID check authenticates the exact executable")
            for command in (
                b"/usr/bin/python3\0script.py\0--name=chromium\0",
                b"/usr/bin/python3 script.py --name=chromium",
                b"/usr/bin/not-google-chrome --user-data-dir=/profile",
                b"\0/usr/bin/google-chrome\0",
                b"",
                b"   ",
                b"'/usr/bin/google-chrome --user-data-dir=/profile",
            ):
                with self.subTest(command=command):
                    read_bytes.return_value = command
                    self.assertFalse(binding.browser_process(42, root))
            read_bytes.side_effect = OSError("gone")
            self.assertFalse(binding.browser_process(42, root))

    def test_qtile_ipc_call_uses_exact_owned_socket_and_protocol(self):
        path = pathlib.Path("/runtime/qtilesocket.:0")
        socket_info = mock.Mock(st_mode=stat.S_IFSOCK | 0o600, st_uid=binding.os.getuid(),
                                st_dev=11, st_ino=22)
        path_mock = mock.Mock()
        path_mock.lstat.return_value = socket_info
        path_mock.is_symlink.return_value = False
        response = binding.marshal.dumps((0, {"id": 22, "group": "f"}))
        frame = binding.QTILE_IPC_HEADER.pack(len(response)) + response
        client = mock.Mock()
        client.__enter__ = mock.Mock(return_value=client)
        client.__exit__ = mock.Mock(return_value=False)
        chunks = iter([frame[:5], frame[5:], b""])
        client.recv.side_effect = lambda _size: next(chunks)
        credentials = struct.pack("3i", 999, binding.os.getuid(), 999)
        client.getsockopt.return_value = credentials
        with mock.patch.object(binding.pathlib, "Path", return_value=path_mock), \
                mock.patch.object(binding.socket, "socket", return_value=client):
            result = binding.qtile_ipc_call([], "prepare_ordinary_window", ("22",), path=path)
        self.assertEqual(result, {"id": 22, "group": "f"})
        client.connect.assert_called_once_with(str(path_mock))
        sent = client.sendall.call_args.args[0]
        size = binding.QTILE_IPC_HEADER.unpack(sent[:binding.QTILE_IPC_HEADER.size])[0]
        self.assertEqual(binding.marshal.loads(sent[binding.QTILE_IPC_HEADER.size:
                                                   binding.QTILE_IPC_HEADER.size + size]),
                         ([], "prepare_ordinary_window", ("22",), {}, True))
        client.shutdown.assert_called_once_with(socket.SHUT_WR)

    def test_qtile_ipc_rejects_unsafe_socket_and_response(self):
        path = pathlib.Path("/runtime/qtilesocket.:0")
        for mode, uid in ((stat.S_IFREG | 0o600, binding.os.getuid()),
                          (stat.S_IFSOCK | 0o644, binding.os.getuid()),
                          (stat.S_IFSOCK | 0o600, binding.os.getuid() + 1)):
            path_mock = mock.Mock()
            path_mock.lstat.return_value = mock.Mock(
                st_mode=mode, st_uid=uid, st_dev=11, st_ino=22)
            path_mock.is_symlink.return_value = False
            with self.subTest(mode=mode, uid=uid), \
                    mock.patch.object(binding.pathlib, "Path", return_value=path_mock), \
                    self.assertRaisesRegex(binding.X11Unavailable, "unsafe"):
                binding.qtile_ipc_call([], "info", path=path)
        path_mock = mock.Mock()
        path_mock.lstat.return_value = mock.Mock(
            st_mode=stat.S_IFSOCK | 0o600, st_uid=binding.os.getuid(), st_dev=11, st_ino=22)
        path_mock.is_symlink.return_value = False
        client = mock.Mock()
        client.__enter__ = mock.Mock(return_value=client)
        client.__exit__ = mock.Mock(return_value=False)
        client.getsockopt.return_value = struct.pack("3i", 999, binding.os.getuid(), 999)
        client.recv.side_effect = [binding.QTILE_IPC_HEADER.pack(100) + b"short", b""]
        with mock.patch.object(binding.pathlib, "Path", return_value=path_mock), \
                mock.patch.object(binding.socket, "socket", return_value=client), \
                self.assertRaisesRegex(binding.X11Unavailable, "invalid size"):
            binding.qtile_ipc_call([], "info", path=path)

    def test_qtile_ipc_timeout_tracks_remaining_budget_and_never_sends_after_expiry(self):
        for expire_during_connect in (False, True):
            with self.subTest(expire_during_connect=expire_during_connect):
                clock = [10.0]
                path = mock.Mock()
                path.lstat.return_value = mock.Mock(
                    st_mode=stat.S_IFSOCK | 0o600, st_uid=binding.os.getuid(),
                    st_dev=11, st_ino=22)
                path.is_symlink.return_value = False
                client = mock.MagicMock()
                client.__enter__.return_value = client
                client.getsockopt.return_value = struct.pack("3i", 999, binding.os.getuid(), 999)
                client.connect.side_effect = lambda _path: clock.__setitem__(
                    0, 10.125 if expire_during_connect else 10.0625)

                def receive(_size):
                    clock[0] = 10.125
                    return b"late response"

                client.recv.side_effect = receive
                with mock.patch.object(binding.pathlib, "Path", return_value=path), \
                        mock.patch.object(binding.socket, "socket", return_value=client), \
                        mock.patch.object(binding.time, "monotonic", side_effect=lambda: clock[0]), \
                        self.assertRaisesRegex(binding.X11Unavailable, "deadline expired"):
                    binding.qtile_ipc_call([], "info", path="/runtime/socket", deadline=10.125)
                timeouts = [call.args[0] for call in client.settimeout.call_args_list]
                self.assertTrue(all(0 < timeout <= 0.125 for timeout in timeouts))
                if expire_during_connect:
                    client.sendall.assert_not_called()
                    client.recv.assert_not_called()
                else:
                    self.assertEqual(timeouts[-1], 0.0625)
                    client.sendall.assert_called_once()
                    client.recv.assert_called_once()

    def test_browser_incarnation_parses_start_time_after_parenthesized_process_name(self):
        stat_fields = ["S"] + ["0"] * 18 + ["987654"]
        with mock.patch.object(binding.pathlib.Path, "read_text", side_effect=[
                "system-boot\n", "4242 (chrome worker (main)) " + " ".join(stat_fields)]):
            self.assertEqual(binding.browser_incarnation(4242),
                             ["system-boot", 4242, "987654"])
        with mock.patch.object(binding.pathlib.Path, "read_text", side_effect=OSError("gone")):
            self.assertIsNone(binding.browser_incarnation(4242))
        self.assertIsNone(binding.browser_incarnation(True))

    def test_run_qtile_uses_in_process_ipc_by_default(self):
        command = binding.qtile_command("prepare_ordinary_window", 22)
        with mock.patch.object(binding, "qtile_ipc_command",
                               return_value={"id": 22, "group": "f"}) as ipc, \
                mock.patch.object(binding.subprocess, "run") as process:
            output = binding.run_qtile(command)
        self.assertEqual(json.loads(output), {"id": 22, "group": "f"})
        ipc.assert_called_once_with(command)
        process.assert_not_called()


class OrdinaryX11Test(unittest.TestCase):
    def setUp(self):
        self.process_patch = mock.patch.object(binding, "browser_process", return_value=True)
        self.process_patch.start()
        self.addCleanup(self.process_patch.stop)
        incarnation_patch = mock.patch.object(
            binding, "browser_incarnation", return_value=["boot", 4242, "start"])
        incarnation_patch.start()
        self.addCleanup(incarnation_patch.stop)

    def test_ordinary_prepare_uses_unique_exact_xid_without_focus(self):
        nonce = "a" * 32
        adapter = FakeAdapter()
        adapter.values[(11, "_NET_WM_NAME")] = f"__ABUSH_ORDINARY__:{nonce} - Google Chrome"
        commands = []

        def run(command, **_kwargs):
            if "doc" in command:
                name = command[-1]
                return mock.Mock(returncode=0, stdout=json.dumps(
                    f"{name}(manager, xid: int, expected_json: str = '')\n"))
            commands.append(command)
            return mock.Mock(
                returncode=0, stdout=json.dumps({
                    "id": 11, "group": "f", "name": adapter.title(11)}), stderr="")

        result = binding.prepare_ordinary_window(
            {"nonce": nonce}, adapter=adapter, attempts=1, run=run)
        self.assertEqual(result, {"xid": 11, "nonce": nonce})
        self.assertEqual(adapter.values[(11, "_ABUSH_ORDINARY_NONCE")], nonce)
        self.assertEqual(len(commands), 2)
        self.assertEqual(commands[0], [
            "qtile", "cmd-obj", "--object", "window", "11", "--function", "info"])
        self.assertEqual(commands[1][:-1], binding.qtile_command("prepare_ordinary_window", 11))
        self.assertEqual(json.loads(commands[1][-1]), {
            "version": 1, "nonce": nonce, "identity": ["boot", 4242, "start"]})
        self.assertNotIn("focus", commands[1])
        self.assertNotIn("toscreen", commands[1])

    def prepare_fixture(self, probe=None):
        nonce = "a" * 32
        adapter = FakeAdapter()
        adapter.values[(22, "_NET_WM_NAME")] = f"__ABUSH_ORDINARY__:{nonce} - Google Chrome"
        window = qtile.FakeWindow(group="e", name="previous cached title")
        window.metadata = {}
        window.incarnation = ["boot", 4242, "start"]
        manager = SimpleNamespace(windows_map={22: window}, groups_map={"f": qtile.FakeGroup("f")})
        namespace = qtile.load_browser_config({})
        trace = []

        def ipc(selectors, function, arguments=(), deadline=None):
            self.assertIsNotNone(deadline)
            if function == "doc":
                return f"{arguments[0]}(manager, xid: int, expected_json: str = '')\n"
            if function == "info":
                self.assertEqual(selectors, [("window", 22)])
                self.assertEqual(arguments, ())
                self.assertEqual(adapter.property(22, binding.ORDINARY_NONCE_PROPERTY), nonce)
                self.assertEqual(adapter.title(22), f"__ABUSH_ORDINARY__:{nonce} - Google Chrome")
                info = {"id": 22, "name": window.name, "group": window.group.name}
                trace.append(("info", window.name))
                if probe is not None:
                    probe(adapter, window, info, len(trace))
                return info
            self.assertEqual(selectors, [])
            self.assertEqual(function, "prepare_ordinary_window")
            self.assertEqual(arguments[0], "22")
            trace.append(("prepare", window.name))
            window.metadata = {name: value for (xid, name), value in adapter.values.items()
                               if xid == 22}
            return namespace[function](manager, 22, arguments[1])

        return nonce, adapter, window, manager, trace, ipc

    def assert_no_prepare_effects(self, window, manager, trace):
        self.assertFalse(any(event == "prepare" for event, _name in trace))
        window.togroup.assert_not_called()
        window.focus.assert_not_called()
        window.window.set_property.assert_not_called()
        manager.groups_map["f"].toscreen.assert_not_called()
        self.assertTrue(window.can_steal_focus)

    def test_prepare_waits_for_cached_qtile_name_before_single_real_command(self):
        def publish_cached_title(adapter, window, _info, count):
            self.assertEqual(count, 1 if window.name == "previous cached title" else 2)
            window.name = adapter.title(22)

        nonce, adapter, window, manager, trace, ipc = self.prepare_fixture(publish_cached_title)
        with mock.patch.object(binding, "qtile_ipc_call", side_effect=ipc):
            result = binding.prepare_ordinary_window(
                {"nonce": nonce}, adapter=adapter, attempts=3, interval=0)
        self.assertEqual(result, {"xid": 22, "nonce": nonce})
        title = adapter.title(22)
        self.assertEqual(trace, [("info", "previous cached title"), ("info", title),
                                 ("prepare", title)])
        window.togroup.assert_called_once_with("f", switch_group=False)
        window.focus.assert_not_called()
        manager.groups_map["f"].toscreen.assert_not_called()
        self.assertFalse(window.can_steal_focus)

    def test_prepare_readiness_failure_never_dispatches_prepare(self):
        for failure in ("stale", "wrong_nonce", "wrong_xid", "missing_name", "ipc"):
            with self.subTest(failure=failure):
                def probe(_adapter, _window, info, _count):
                    if failure == "wrong_nonce":
                        info["name"] = f"__ABUSH_ORDINARY__:{'b' * 32}"
                    elif failure == "wrong_xid":
                        info["id"] = 11
                    elif failure == "missing_name":
                        del info["name"]
                    elif failure == "ipc":
                        raise binding.X11Unavailable("modeled Qtile readiness IPC failure")

                nonce, adapter, window, manager, trace, ipc = self.prepare_fixture(probe)
                with mock.patch.object(binding, "qtile_ipc_call", side_effect=ipc), \
                        self.assertRaises(binding.X11Unavailable):
                    binding.prepare_ordinary_window(
                        {"nonce": nonce}, adapter=adapter, attempts=3, interval=0)
                self.assert_no_prepare_effects(window, manager, trace)
                self.assertLessEqual(len(trace), 3)
                self.assertEqual(len(trace), 1 if failure in {"wrong_xid", "ipc"} else 3)

    def test_prepare_revalidates_identity_before_next_observation_and_effect(self):
        for phase in ("waiting", "ready"):
            for change in ("nonce", "incarnation", "pid", "legacy", "tab_switch", "class"):
                with self.subTest(phase=phase, change=change):
                    def invalidate(adapter, window, _info, _count):
                        if change == "nonce":
                            adapter.values[(22, binding.ORDINARY_NONCE_PROPERTY)] = "b" * 32
                        elif change == "incarnation":
                            binding.browser_incarnation.return_value = ["boot", 4242, "reused"]
                        elif change == "pid":
                            adapter.pid = lambda _window: 4243
                            binding.browser_incarnation.side_effect = lambda pid: [
                                "boot", pid, "start"]
                        elif change == "legacy":
                            adapter.values[(22, "_ABUSH_BINDING_NONCE")] = "legacy"
                        elif change == "tab_switch":
                            adapter.values[(22, "_NET_WM_NAME")] = "other tab - Google Chrome"
                        else:
                            adapter.values[(22, "WM_CLASS")] = "unrelated"

                    nonce, adapter, window, manager, trace, ipc = self.prepare_fixture(invalidate)
                    if phase == "ready":
                        window.name = adapter.title(22)
                    with mock.patch.object(binding, "browser_incarnation",
                                           return_value=["boot", 4242, "start"]), \
                            mock.patch.object(binding, "qtile_ipc_call", side_effect=ipc), \
                            self.assertRaisesRegex(binding.X11Unavailable, "identity|incarnation"):
                        binding.prepare_ordinary_window(
                            {"nonce": nonce}, adapter=adapter, attempts=3, interval=0)
                    self.assertEqual(len(trace), 1)
                    self.assert_no_prepare_effects(window, manager, trace)

    def test_prepare_deadline_is_shared_with_raw_title_discovery(self):
        clock = [0.0]

        def exhaust_budget(_adapter, _window, info, _count):
            info["name"] = adapter.title(22)
            clock[0] = 5.0

        nonce, adapter, window, manager, trace, ipc = self.prepare_fixture(exhaust_budget)
        windows = adapter.windows

        def discover():
            clock[0] = 4.9
            return windows()

        adapter.windows = discover
        with mock.patch.object(binding.time, "monotonic", side_effect=lambda: clock[0]), \
                mock.patch.object(binding, "qtile_ipc_call", side_effect=ipc), \
                self.assertRaisesRegex(binding.X11Unavailable, "readiness timed out"):
            binding.prepare_ordinary_window(
                {"nonce": nonce}, adapter=adapter, attempts=100, interval=0)
        self.assertEqual(len(trace), 1)
        self.assert_no_prepare_effects(window, manager, trace)

    def test_prepare_caller_deadline_blocks_late_discovery_and_native_writes(self):
        for observation in ("windows", "pid", "title", "property"):
            with self.subTest(observation=observation):
                clock = [10.0]
                nonce, adapter, window, manager, trace, ipc = self.prepare_fixture()
                original = getattr(adapter, observation)

                def late(*arguments):
                    result = original(*arguments)
                    clock[0] = 10.1
                    return result

                setattr(adapter, observation, late)
                with mock.patch.object(binding.time, "monotonic", side_effect=lambda: clock[0]), \
                        mock.patch.object(binding, "qtile_ipc_call", side_effect=ipc), \
                        self.assertRaisesRegex(binding.X11Unavailable, "deadline expired"):
                    binding.prepare_ordinary_window(
                        {"nonce": nonce}, adapter=adapter, deadline=10.1)
                self.assertNotIn((22, binding.ORDINARY_NONCE_PROPERTY), adapter.values)
                self.assertFalse(adapter.finished)
                self.assert_no_prepare_effects(window, manager, trace)

    def test_prepare_injected_run_receives_remaining_budget_and_rejects_late_probe(self):
        clock = [10.0]
        nonce, adapter, window, manager, trace, _ipc = self.prepare_fixture()
        calls = []

        def run(command, **options):
            if "doc" in command:
                return mock.Mock(returncode=0, stdout=json.dumps(
                    f"{command[-1]}(manager, xid: int, expected_json: str = '')\n"))
            calls.append((command, options))
            clock[0] = 10.125
            return mock.Mock(returncode=0, stdout=json.dumps({
                "id": 22, "group": "e", "name": adapter.title(22)}))

        with mock.patch.object(binding.time, "monotonic", side_effect=lambda: clock[0]), \
                self.assertRaisesRegex(binding.X11Unavailable, "deadline expired"):
            binding.prepare_ordinary_window(
                {"nonce": nonce}, adapter=adapter, deadline=10.125, run=run)
        self.assertEqual(len(calls), 1)
        self.assertEqual(calls[0][1]["timeout"], 0.125)
        self.assert_no_prepare_effects(window, manager, trace)

    def test_prepare_command_failure_is_not_retried(self):
        nonce, adapter, window, manager, trace, ipc = self.prepare_fixture()
        window.name = adapter.title(22)
        window.incarnation = ["boot", 4242, "different"]
        with mock.patch.object(binding, "qtile_ipc_call", side_effect=ipc), \
                self.assertRaises(ValueError):
            binding.prepare_ordinary_window(
                {"nonce": nonce}, adapter=adapter, attempts=3, interval=0)
        self.assertEqual([event for event, _name in trace], ["info", "prepare"])
        window.togroup.assert_not_called()
        window.focus.assert_not_called()

    def test_prepare_unknown_effect_is_not_retried(self):
        nonce, adapter, window, manager, trace, ipc = self.prepare_fixture()
        window.name = adapter.title(22)

        def lose_completion(selectors, function, arguments=(), deadline=None):
            result = ipc(selectors, function, arguments, deadline=deadline)
            if function == "prepare_ordinary_window":
                raise binding.X11Unavailable("modeled lost prepare completion")
            return result

        with mock.patch.object(binding, "qtile_ipc_call", side_effect=lose_completion), \
                self.assertRaisesRegex(binding.X11Unavailable, "lost prepare completion"):
            binding.prepare_ordinary_window(
                {"nonce": nonce}, adapter=adapter, attempts=3, interval=0)
        self.assertEqual([event for event, _name in trace], ["info", "prepare"])
        window.togroup.assert_called_once_with("f", switch_group=False)
        window.focus.assert_not_called()
        manager.groups_map["f"].toscreen.assert_not_called()

    def test_ordinary_prepare_refuses_managed_candidate(self):
        nonce = "b" * 32
        adapter = FakeAdapter()
        adapter.values[(22, "_NET_WM_NAME")] = f"__ABUSH_ORDINARY__:{nonce}"
        adapter.values[(22, "WM_WINDOW_ROLE")] = "abush-browser"
        adapter.values[(22, "_ABUSH_BINDING_NONCE")] = nonce
        adapter.values[(22, "_ABUSH_REMOTE_ID")] = "r_12345678"
        adapter.values[(22, "_ABUSH_WORKSPACE_ID")] = "workspace"
        adapter.values[(22, "_ABUSH_WINDOW_GENERATION")] = "1"
        adapter.values[(22, "_ABUSH_CONTROL_STATE")] = "remote"
        adapter.values[(22, "_ABUSH_DISPLAY_TITLE")] = "local"
        before = adapter.values.copy()
        run = mock.Mock()
        with self.assertRaises(binding.X11Unavailable):
            binding.prepare_ordinary_window({"nonce": nonce}, adapter=adapter, attempts=1, run=run)
        self.assertEqual(adapter.values, before)
        run.assert_not_called()

    def test_incomplete_legacy_markers_exclude_prepare_verify_and_activation(self):
        nonce = "a" * 32
        cases = [(name, "legacy") for name in binding.LEGACY_BROWSER_PROPERTIES]
        cases += [("WM_WINDOW_ROLE", "abush-browser"), ("WM_CLASS", "chromium\0AbushBrowser")]
        for name, value in cases:
            with self.subTest(marker=name):
                adapter = FakeAdapter()
                adapter.values[(11, "_NET_WM_NAME")] = f"__ABUSH_ORDINARY__:{nonce}"
                adapter.values[(11, binding.ORDINARY_NONCE_PROPERTY)] = nonce
                adapter.values[(11, name)] = value
                before = adapter.values.copy()
                run = mock.Mock()
                with self.assertRaises(binding.X11Unavailable):
                    binding.prepare_ordinary_window(
                        {"nonce": nonce}, adapter=adapter, attempts=1, run=run)
                with self.assertRaisesRegex(ValueError, "identity mismatch"):
                    binding.verify_prepared_ordinary_window(
                        {"nonce": nonce, "xid": 11}, adapter=adapter)
                with self.assertRaisesRegex(ValueError, "identity mismatch"):
                    binding.activate_ordinary_window(
                        {"nonce": nonce, "xid": 11}, adapter=adapter, run=run)
                self.assertEqual(adapter.values, before)
                self.assertFalse(adapter.finished)
                run.assert_not_called()

    def test_ordinary_prepare_refuses_non_chrome_pid(self):
        nonce = "e" * 32
        adapter = FakeAdapter()
        adapter.values[(11, "_NET_WM_NAME")] = f"__ABUSH_ORDINARY__:{nonce}"
        adapter.pid = lambda _window: 4242
        with mock.patch.object(binding, "browser_process", return_value=False), \
                self.assertRaises(binding.X11Unavailable):
            binding.prepare_ordinary_window(
                {"nonce": nonce}, adapter=adapter, attempts=1,
                run=lambda *_args, **_kwargs: mock.Mock(
                    returncode=0, stdout=json.dumps({"id": 11, "group": "f"})))

    def test_prepared_identity_requires_exact_title_nonce_pid_and_xid(self):
        nonce = "f" * 32
        adapter = FakeAdapter()
        adapter.values[(11, "_NET_WM_NAME")] = f"__ABUSH_ORDINARY__:{nonce}"
        adapter.values[(11, binding.ORDINARY_NONCE_PROPERTY)] = nonce
        adapter.pid = lambda _window: 4242
        with mock.patch.object(binding, "browser_process", return_value=True):
            self.assertTrue(binding.verify_prepared_ordinary_window(
                {"nonce": nonce, "xid": 11}, adapter=adapter))
        with mock.patch.object(binding, "browser_process", return_value=False), \
                self.assertRaisesRegex(ValueError, "identity mismatch"):
            binding.verify_prepared_ordinary_window(
                {"nonce": nonce, "xid": 11}, adapter=adapter)
        adapter.values[(11, "_NET_WM_NAME")] = "different title"
        with mock.patch.object(binding, "browser_process", return_value=True), \
                self.assertRaisesRegex(ValueError, "identity mismatch"):
            binding.verify_prepared_ordinary_window(
                {"nonce": nonce, "xid": 11}, adapter=adapter)

    def test_qtile_info_is_strict_json_and_activation_verifier_polls_all_fields(self):
        nonce = "d" * 32
        adapter = FakeAdapter()
        adapter.values[(11, "_ABUSH_ORDINARY_NONCE")] = nonce
        active = iter([22, 11])
        adapter.active_window = lambda: next(active)
        outputs = iter([
            json.dumps({"id": 11, "group": "e"}),
            json.dumps({"id": 11, "group": "f"}),
        ])

        def run(_command, **_kwargs):
            return mock.Mock(returncode=0, stdout=next(outputs), stderr="")

        self.assertTrue(binding.verify_ordinary_activation(
            adapter, 11, nonce, run=run, attempts=2, interval=0))
        bad = lambda *_args, **_kwargs: mock.Mock(
            returncode=0, stdout="{'id': 11, 'group': 'f'}", stderr="")
        with self.assertRaisesRegex(binding.X11Unavailable, "invalid Qtile"):
            binding.qtile_window_info(11, run=bad)

    def test_ordinary_activate_focuses_exact_xid_and_rejects_string_bool(self):
        nonce = "c" * 32
        adapter = FakeAdapter()
        adapter.values[(11, "_ABUSH_ORDINARY_NONCE")] = nonce
        adapter.values[(11, "_NET_WM_NAME")] = f"__ABUSH_ORDINARY__:{nonce}"
        commands = []

        def run(command, **_kwargs):
            if "doc" in command:
                name = command[-1]
                return mock.Mock(returncode=0, stdout=json.dumps(
                    f"{name}(manager, xid: int, expected_json: str = '')\n"))
            commands.append(command)
            return mock.Mock(
                returncode=0, stdout=json.dumps({"id": 11, "group": "f"}), stderr="")

        result = binding.activate_ordinary_window(
            {"nonce": nonce, "xid": 11}, adapter=adapter, run=run,
            verify=lambda xid: xid == 11)
        self.assertEqual(result["xid"], 11)
        self.assertEqual(commands[-1][:-1], binding.qtile_activate_command(11))
        self.assertEqual(json.loads(commands[-1][-1]), {
            "version": 1, "nonce": nonce, "identity": ["boot", 4242, "start"]})
        self.assertNotIn("True", commands[-1])
        self.assertNotIn("False", commands[-1])
        for xid in (True, "11"):
            with self.assertRaises(ValueError):
                binding.activate_ordinary_window(
                    {"nonce": nonce, "xid": xid}, adapter=adapter, run=run)

    def test_duplicate_nonce_and_wrong_incarnation_fail_before_writes(self):
        nonce = "a" * 32
        adapter = FakeAdapter()
        for xid in (11, 22):
            adapter.values[(xid, "_NET_WM_NAME")] = f"__ABUSH_ORDINARY__:{nonce}"
        run = mock.Mock()
        with self.assertRaisesRegex(ValueError, "ambiguous"):
            binding.prepare_ordinary_window({"nonce": nonce}, adapter=adapter, attempts=1, run=run)
        adapter.values[(22, "_NET_WM_NAME")] = "unrelated"
        with self.assertRaisesRegex(binding.X11Unavailable, "incarnation"):
            binding.prepare_ordinary_window(
                {"nonce": nonce}, adapter=adapter, attempts=1, run=run,
                expected_identity=["boot", 4242, "restarted"])
        self.assertFalse(adapter.finished)
        run.assert_not_called()

    def test_ordinary_title_matching_accepts_chrome_suffix_only(self):
        nonce = "a" * 32
        expected = f"__ABUSH_ORDINARY__:{nonce}"
        for suffix in ("", " - Google Chrome", " - Chromium"):
            self.assertTrue(binding.ordinary_title_matches(expected + suffix, nonce))
        self.assertFalse(binding.ordinary_title_matches(expected + " - Firefox", nonce))
        self.assertFalse(binding.ordinary_title_matches("prefix " + expected, nonce))

    def test_managed_protocol_functions_are_absent(self):
        for name in (
            "bind_workspace_window", "verify_workspace_window", "clear_workspace_window",
            "managed_metadata", "managed_window", "admit_candidate", "update_candidate_lease",
            "quarantine_qtile", "notify_qtile", "restore_qtile_group", "rollback_binding",
            "qtile_move_command", "qtile_candidate_lease_command",
        ):
            self.assertFalse(hasattr(binding, name), name)


class OrdinaryIPCCompatibilityTest(unittest.TestCase):
    setUp = OrdinaryX11Test.setUp

    def fixture(self):
        nonce = "a" * 32
        adapter = FakeAdapter()
        adapter.values[(22, "_NET_WM_NAME")] = f"__ABUSH_ORDINARY__:{nonce} - Google Chrome"
        adapter.values[(22, binding.ORDINARY_NONCE_PROPERTY)] = nonce
        return nonce, adapter

    @contextmanager
    def transport(self, adapter, legacy=False, hook=None, doc_result=None, effect_error=False):
        namespace = qtile.load_browser_config({})
        window = qtile.FakeWindow(group="f", name=adapter.title(22))
        window.incarnation = ["boot", 4242, "start"]
        window.metadata = {name: value for (xid, name), value in adapter.values.items() if xid == 22}
        calls = []

        class WindowNode(CommandObject):
            def _items(self, name):
                return None

            def _select(self, name, sel):
                return None

            @expose_command()
            def info(self):
                return {"id": 22, "group": window.group.name, "name": window.name}

        node = WindowNode()

        class RootNode(CommandObject):
            locked = False

            def _items(self, name):
                return (False, [22]) if name == "window" else None

            def _select(self, name, sel):
                return node if (name, sel) == ("window", 22) else None

        root = RootNode()
        root.windows_map = {22: window}
        root.groups_map = {"f": qtile.FakeGroup("f")}

        def dispatch(name, manager, xid, *arguments):
            result = namespace[name](manager, xid, *arguments)
            if effect_error:
                raise TypeError("modeled error after effect")
            return result

        def prepare_ordinary_window(manager, xid: int):
            return dispatch("prepare_ordinary_window", manager, xid)

        def activate_ordinary_window(manager, xid: int):
            return dispatch("activate_ordinary_window", manager, xid)

        def current_prepare(manager, xid: int, expected_json: str = ""):
            return dispatch("prepare_ordinary_window", manager, xid, expected_json)

        def current_activate(manager, xid: int, expected_json: str = ""):
            return dispatch("activate_ordinary_window", manager, xid, expected_json)

        root._commands = root._commands.copy()
        root._commands.update({
            "prepare_ordinary_window": prepare_ordinary_window if legacy else
                current_prepare if effect_error else namespace["prepare_ordinary_window"],
            "activate_ordinary_window": activate_ordinary_window if legacy else
                current_activate if effect_error else namespace["activate_ordinary_window"],
        })
        server = IPCCommandServer(root)
        path = mock.Mock()
        path.lstat.return_value = mock.Mock(
            st_mode=stat.S_IFSOCK | 0o600, st_uid=binding.os.getuid(), st_dev=11, st_ino=22)
        path.is_symlink.return_value = False

        def client_socket(*_args):
            client = mock.MagicMock()
            client.__enter__.return_value = client
            client.getsockopt.return_value = struct.pack("3i", 999, binding.os.getuid(), 999)
            response = []

            def send(frame):
                size = binding.QTILE_IPC_HEADER.unpack(frame[:4])[0]
                self.assertEqual(len(frame[4:]), size)
                data = binding.marshal.loads(frame[4:])
                calls.append(data)
                selectors, name, args, _kwargs, lifted = data
                self.assertTrue(lifted)
                if hook is not None:
                    hook(name, adapter)
                window.metadata = {key: value for (xid, key), value in adapter.values.items()
                                   if xid == 22}
                result = server.call(data)
                if name == "doc" and doc_result is not None:
                    result = (0, doc_result)
                payload = binding.marshal.dumps(result)
                response.extend([binding.QTILE_IPC_HEADER.pack(len(payload)) + payload, b""])

            client.sendall.side_effect = send
            client.recv.side_effect = lambda _size: response.pop(0)
            return client

        with mock.patch.object(binding, "qtile_socket_path", return_value=path), \
                mock.patch.object(binding.socket, "socket", side_effect=client_socket), \
                mock.patch.object(binding.subprocess, "run", side_effect=AssertionError("process")):
            yield calls, window, root

    def invoke(self, command, nonce, adapter, deadline=None):
        options = {} if deadline is None else {"deadline": deadline}
        if command == "prepare_ordinary_window":
            return binding.prepare_ordinary_window(
                {"nonce": nonce}, adapter=adapter, attempts=1, **options)
        return binding.activate_ordinary_window(
            {"nonce": nonce, "xid": 22}, adapter=adapter, verify=lambda xid: xid == 22,
            **options)

    def test_real_root_doc_lift_args_and_marshal_frames_negotiate_both_signatures(self):
        for legacy in (False, True):
            for command in ("prepare_ordinary_window", "activate_ordinary_window"):
                with self.subTest(legacy=legacy, command=command):
                    nonce, adapter = self.fixture()
                    with self.transport(adapter, legacy=legacy) as (calls, window, root):
                        self.assertEqual(self.invoke(command, nonce, adapter),
                                         {"xid": 22, "nonce": nonce})
                    self.assertEqual(calls[0][:3], ([], "doc", (command,)))
                    effects = [data for data in calls if data[1] == command]
                    self.assertEqual(len(effects), 1)
                    self.assertEqual(effects[0][2][0], "22")
                    self.assertEqual(len(effects[0][2]), 1 if legacy else 2)
                    if not legacy:
                        self.assertEqual(json.loads(effects[0][2][1]), {
                            "version": 1, "nonce": nonce, "identity": ["boot", 4242, "start"]})
                    if command.startswith("prepare"):
                        window.togroup.assert_called_once_with("f", switch_group=False)
                        window.focus.assert_not_called()
                        root.groups_map["f"].toscreen.assert_not_called()
                    else:
                        window.focus.assert_called_once_with(warp=False)
                        root.groups_map["f"].toscreen.assert_called_once_with()

    def test_unknown_capability_fails_before_native_write_or_effect(self):
        for command in ("prepare_ordinary_window", "activate_ordinary_window"):
            for doc in ({}, "", "other(manager, xid: int)\n", f"{command}(manager, xid)",
                        f"{command}(manager, xid: int)", f"{command}(manager, xid)\n",
                        f"{command}(manager, xid: int, *args)\n"):
                with self.subTest(command=command, doc=doc):
                    nonce, adapter = self.fixture()
                    before = adapter.values.copy()
                    with self.transport(adapter, doc_result=doc) as (calls, window, _root), \
                            self.assertRaises(binding.X11Unavailable):
                        self.invoke(command, nonce, adapter)
                    self.assertEqual([data[1] for data in calls], ["doc"])
                    self.assertEqual(adapter.values, before)
                    self.assertFalse(adapter.finished)
                    window.togroup.assert_not_called()
                    window.focus.assert_not_called()

    def test_error_after_effect_never_retries_with_either_signature(self):
        for legacy in (False, True):
            for command in ("prepare_ordinary_window", "activate_ordinary_window"):
                with self.subTest(legacy=legacy, command=command):
                    nonce, adapter = self.fixture()
                    with self.transport(adapter, legacy=legacy, effect_error=True) as state, \
                            self.assertRaisesRegex(binding.X11Unavailable, "Qtile command failed"):
                        self.invoke(command, nonce, adapter)
                    calls, window, _root = state
                    self.assertEqual(sum(data[1] == command for data in calls), 1)
                    self.assertEqual(window.togroup.call_count + window.focus.call_count, 1)

    def test_legacy_native_binding_and_deadline_revalidated_around_effect(self):
        for command in ("prepare_ordinary_window", "activate_ordinary_window"):
            for phase in ("doc", command):
                for change in ("nonce", "title", "pid", "incarnation", "class", "process",
                               *binding.LEGACY_BROWSER_PROPERTIES, "WM_WINDOW_ROLE", "deadline"):
                    with self.subTest(command=command, phase=phase, change=change):
                        nonce, adapter = self.fixture()
                        clock = [10.0]

                        def invalidate(name, adapter):
                            if name != phase:
                                return
                            if change == "nonce":
                                adapter.values[(22, binding.ORDINARY_NONCE_PROPERTY)] = "b" * 32
                            elif change == "title":
                                adapter.values[(22, "_NET_WM_NAME")] = "other page"
                            elif change == "pid":
                                adapter.pid = lambda _window: 4243
                                binding.browser_incarnation.side_effect = lambda pid: [
                                    "boot", pid, "start"]
                            elif change == "incarnation":
                                binding.browser_incarnation.return_value = ["boot", 4242, "new"]
                            elif change == "class":
                                adapter.values[(22, "WM_CLASS")] = "unrelated"
                            elif change == "process":
                                binding.browser_process.return_value = False
                            elif change == "deadline":
                                clock[0] = 10.25
                            else:
                                adapter.values[(22, change)] = (
                                    "abush-browser" if change == "WM_WINDOW_ROLE" else "legacy")

                        with mock.patch.object(binding, "browser_process", return_value=True), \
                                mock.patch.object(binding, "browser_incarnation",
                                                  return_value=["boot", 4242, "start"]), \
                                mock.patch.object(binding.time, "monotonic",
                                                  side_effect=lambda: clock[0]), \
                                self.transport(adapter, legacy=True, hook=invalidate) as state, \
                                self.assertRaises((binding.X11Unavailable, ValueError)):
                            self.invoke(command, nonce, adapter, deadline=10.25)
                        calls, _window, _root = state
                        self.assertEqual(sum(data[1] == command for data in calls),
                                         0 if phase == "doc" else 1)


if __name__ == "__main__":
    unittest.main()
