#!/usr/bin/env python3

import importlib.util
import json
import pathlib
import socket
import stat
import struct
import unittest
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
BINDING_PATH = ROOT / "chromium/external-links/x11_binding.py"
spec = importlib.util.spec_from_file_location("x11_binding", BINDING_PATH)
binding = importlib.util.module_from_spec(spec)
spec.loader.exec_module(binding)


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
            commands.append(command)
            return mock.Mock(
                returncode=0, stdout=json.dumps({"id": 11, "group": "f"}), stderr="")

        result = binding.prepare_ordinary_window(
            {"nonce": nonce}, adapter=adapter, attempts=1, run=run)
        self.assertEqual(result, {"xid": 11, "nonce": nonce})
        self.assertEqual(adapter.values[(11, "_ABUSH_ORDINARY_NONCE")], nonce)
        self.assertEqual(commands[0][:-1], binding.qtile_command("prepare_ordinary_window", 11))
        self.assertEqual(json.loads(commands[0][-1]), {
            "version": 1, "nonce": nonce, "identity": ["boot", 4242, "start"]})
        self.assertNotIn("focus", commands[0])
        self.assertNotIn("toscreen", commands[0])

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


if __name__ == "__main__":
    unittest.main()
