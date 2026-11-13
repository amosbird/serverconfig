#!/usr/bin/env python3

import importlib.util
import io
import json
import pathlib
import socket
import stat
import struct
import unittest
from contextlib import redirect_stdout
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
BINDING_PATH = ROOT / "chromium/external-links/x11_binding.py"
ACCEPTANCE_PATH = ROOT / "scripts/test_abush_browser_x11.py"
spec = importlib.util.spec_from_file_location("x11_binding", BINDING_PATH)
binding = importlib.util.module_from_spec(spec)
spec.loader.exec_module(binding)
spec = importlib.util.spec_from_file_location("x11_acceptance", ACCEPTANCE_PATH)
acceptance = importlib.util.module_from_spec(spec)
spec.loader.exec_module(acceptance)


class FakeAdapter:
    def __init__(self):
        self.values = {
            (11, "_NET_WM_NAME"): "ordinary",
            (11, "WM_CLASS"): "chromium\0Chromium",
            (22, "_NET_WM_NAME"):
                "__ABUSH_BIND__:0123456789abcdef0123456789abcdef:a - Google Chrome",
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

    def delete_property(self, window, name):
        self.values.pop((window, name), None)

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

    def test_candidate_lease_commands_are_exact_and_verified(self):
        nonce = "a" * 32
        register = binding.qtile_candidate_lease_command("register", nonce)
        release = binding.qtile_candidate_lease_command("release", nonce)
        self.assertIn("register_abush_browser_candidate", register)
        self.assertIn("release_abush_browser_candidate", release)
        self.assertEqual(register[-1], nonce)
        self.assertEqual(release[-1], nonce)
        with self.assertRaises(ValueError):
            binding.qtile_candidate_lease_command("register", "bad")

        run = mock.Mock(return_value=mock.Mock(
            returncode=0,
            stdout=json.dumps({"accepted": True, "nonce": nonce}),
            stderr="",
        ))
        self.assertEqual(binding.update_candidate_lease("register", nonce, run),
                         {"accepted": True, "nonce": nonce})
        run.assert_called_once()
        wrong = mock.Mock(return_value=mock.Mock(
            returncode=0,
            stdout=json.dumps({"accepted": True, "nonce": "b" * 32}),
            stderr="",
        ))
        with self.assertRaisesRegex(binding.X11Unavailable, "not confirmed"):
            binding.update_candidate_lease("register", nonce, wrong)

    def test_run_qtile_uses_in_process_ipc_by_default(self):
        command = binding.qtile_move_command(22, "f")
        with mock.patch.object(binding, "qtile_ipc_command",
                               return_value={"id": 22, "group": "f"}) as ipc, \
                mock.patch.object(binding.subprocess, "run") as process:
            output = binding.run_qtile(command)
        self.assertEqual(json.loads(output), {"id": 22, "group": "f"})
        ipc.assert_called_once_with(command)
        process.assert_not_called()


class X11BindingTest(unittest.TestCase):
    def setUp(self):
        self.process_patch = mock.patch.object(binding, "browser_process", return_value=True)
        self.process_patch.start()
        self.addCleanup(self.process_patch.stop)
        self.candidate_patch = mock.patch.object(binding, "quarantine_qtile", return_value={
            "id": 22,
            "group": "__abush_quarantine",
            "current_group_before": "e",
            "current_group_after": "e",
            "active_xid_before": 11,
            "active_xid_after": 11,
            "current_group_preserved": True,
            "active_xid_preserved": True,
        })
        self.candidate_mock = self.candidate_patch.start()
        self.addCleanup(self.candidate_patch.stop)
        self.qtile_patch = mock.patch.object(binding, "notify_qtile", return_value={
            "id": 22,
            "group": "c",
            "current_group_before": "e",
            "current_group_after": "e",
            "active_xid_before": 11,
            "active_xid_after": 11,
            "current_group_preserved": True,
            "active_xid_preserved": True,
        })
        self.qtile_patch.start()
        self.addCleanup(self.qtile_patch.stop)
        self.qtile_info_patch = mock.patch.object(
            binding, "qtile_window_info", return_value={"id": 22, "group": "c"})
        self.qtile_info_patch.start()
        self.addCleanup(self.qtile_info_patch.stop)
        self.qtile_restore_patch = mock.patch.object(binding, "restore_qtile_group")
        self.qtile_restore_patch.start()
        self.addCleanup(self.qtile_restore_patch.stop)

    def test_exact_nonce_chrome_match_and_metadata(self):
        adapter = FakeAdapter()
        payload = {
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678",
            "workspace_id": "workspace",
            "generation": 7,
            "control_state": "remote",
            "display_title": "Abush Browser — local · workbench",
        }
        result = binding.bind_workspace_window(payload, adapter=adapter, attempts=1)
        self.assertEqual(result["xid"], 22)
        self.assertEqual(result["observed"]["_ABUSH_REMOTE_ID"], "r_12345678")
        self.assertEqual(result["observed"]["_ABUSH_WINDOW_GENERATION"], "7")
        self.assertEqual(result["observed"]["WM_WINDOW_ROLE"], "abush-browser")
        self.assertEqual(adapter.values[(22, "WM_CLASS")], "abush-browser\0AbushBrowser\0")
        self.assertEqual(adapter.values[(11, "_NET_WM_NAME")], "ordinary")
        self.assertNotIn((22, binding.CANDIDATE_NONCE_PROPERTY), adapter.values)
        self.candidate_mock.assert_called_once_with(22, payload["nonce"], adapter, None)
        self.assertTrue(adapter.finished)

    def test_bind_failure_after_side_effect_restores_old_properties_and_group(self):
        adapter = FakeAdapter()
        old = {
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678", "workspace_id": "workspace-old",
            "generation": 7, "control_state": "remote", "display_title": "old title",
        }
        binding.bind_workspace_window(old, adapter=adapter, attempts=1)
        before = binding.property_snapshot(adapter, 22)
        self.qtile_info_patch.stop()
        self.qtile_restore_patch.stop()
        groups = iter([{"id": 22, "group": "c"}, {"id": 22, "group": "f"}])
        transitions = []

        def run(command, **_kwargs):
            if command[-1] == "info":
                return mock.Mock(returncode=0, stdout=json.dumps(next(groups)), stderr="")
            transitions.append(command)
            group = "c" if "restore_abush_browser_window" in command else "f"
            return mock.Mock(returncode=0, stdout=json.dumps({
                "id": 22, "group": group, "current_group_before": "e",
                "current_group_after": "e", "active_xid_before": 11,
                "active_xid_after": 11,
            }), stderr="")

        with mock.patch.object(binding, "notify_qtile", side_effect=RuntimeError("after move")):
            with self.assertRaisesRegex(RuntimeError, "after move"):
                binding.bind_workspace_window(
                    {**old, "workspace_id": "workspace-new", "generation": 8},
                    adapter=adapter, attempts=1, run=run)
        self.assertEqual(binding.property_snapshot(adapter, 22), before)
        self.assertEqual(transitions[-1], binding.qtile_restore_command(22, "c"))

    def test_bind_twice_uses_stable_abush_identity(self):
        adapter = FakeAdapter()
        payload = {
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678", "workspace_id": "workspace",
            "generation": 7, "control_state": "remote",
            "display_title": "Abush Browser — local · workbench",
        }
        first = binding.bind_workspace_window(payload, adapter=adapter, attempts=1)
        second = binding.bind_workspace_window(payload, adapter=adapter, attempts=1)
        self.assertEqual(first["xid"], second["xid"])
        self.assertEqual(second["xid"], 22)

    def test_bind_same_xid_from_old_to_new_workspace(self):
        adapter = FakeAdapter()
        old = {
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678", "workspace_id": "workspace-old",
            "generation": 7, "control_state": "remote",
            "display_title": "Abush Browser — local · workbench",
        }
        new = {**old, "workspace_id": "workspace-new", "generation": 8}
        first = binding.bind_workspace_window(old, adapter=adapter, attempts=1)
        self.assertEqual(adapter.values[(22, "WM_CLASS")], "abush-browser\0AbushBrowser\0")
        self.assertEqual(adapter.values[(22, "_NET_WM_NAME")],
                         "Abush Browser — local · workbench")
        second = binding.bind_workspace_window(new, adapter=adapter, attempts=1)
        self.assertEqual(first["xid"], second["xid"])
        self.assertEqual(second["xid"], 22)
        self.assertEqual(second["observed"]["_ABUSH_BINDING_NONCE"], old["nonce"])
        self.assertEqual(second["observed"]["_ABUSH_WORKSPACE_ID"], "workspace-new")
        self.assertEqual(second["observed"]["_ABUSH_WINDOW_GENERATION"], "8")

    def test_existing_and_fresh_distinct_candidates_are_ambiguous(self):
        adapter = FakeAdapter()
        payload = {
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678", "workspace_id": "workspace",
            "generation": 7, "control_state": "remote",
            "display_title": "Abush Browser — local · workbench",
        }
        adapter.values[(11, "WM_WINDOW_ROLE")] = "abush-browser"
        adapter.values[(11, "_ABUSH_BINDING_NONCE")] = payload["nonce"]
        adapter.values[(11, "_ABUSH_REMOTE_ID")] = payload["remote_key"]
        with self.assertRaisesRegex(ValueError, "ambiguous"):
            binding.bind_workspace_window(payload, adapter=adapter, attempts=1)

    def test_duplicate_existing_and_fresh_same_xid_is_deduplicated(self):
        adapter = FakeAdapter()
        payload = {
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678", "workspace_id": "workspace",
            "generation": 7, "control_state": "remote",
            "display_title": "Abush Browser — local · workbench",
        }
        adapter.values[(22, "WM_WINDOW_ROLE")] = "abush-browser"
        adapter.values[(22, "_ABUSH_BINDING_NONCE")] = payload["nonce"]
        adapter.values[(22, "_ABUSH_REMOTE_ID")] = payload["remote_key"]
        result = binding.bind_workspace_window(payload, adapter=adapter, attempts=1)
        self.assertEqual(result["xid"], 22)

    def test_rebind_preserves_pid_spoof_protection(self):
        adapter = FakeAdapter()
        payload = {
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678", "workspace_id": "workspace-old",
            "generation": 7, "control_state": "remote",
            "display_title": "Abush Browser — local · workbench",
        }
        binding.bind_workspace_window(payload, adapter=adapter, attempts=1)
        adapter.pid = lambda _window: 4242
        with mock.patch.object(binding, "browser_process", return_value=False), \
                self.assertRaisesRegex(ValueError, "not a Chrome process"):
            binding.bind_workspace_window({**payload, "workspace_id": "workspace-new"},
                                          adapter=adapter, attempts=1)

    def test_rejects_non_positive_generation(self):
        for generation in (0, -1):
            with self.subTest(generation=generation), self.assertRaises(ValueError):
                binding.bind_workspace_window({
                    "nonce": "0123456789abcdef0123456789abcdef",
                    "remote_key": "r_12345678", "workspace_id": "workspace",
                    "generation": generation, "control_state": "remote",
                    "display_title": "Abush Browser — local · workbench",
                }, adapter=FakeAdapter(), attempts=1)

    def test_title_matching_accepts_chrome_suffix_only(self):
        expected = "__ABUSH_BIND__:0123456789abcdef0123456789abcdef:a"
        self.assertTrue(binding.binding_title_matches(expected, expected))
        self.assertTrue(binding.binding_title_matches(expected + " - Google Chrome", expected))
        self.assertTrue(binding.binding_title_matches(expected + " - Chromium", expected))
        self.assertFalse(binding.binding_title_matches(expected + " - Firefox", expected))
        self.assertFalse(binding.binding_title_matches("prefix " + expected, expected))

    def test_verifies_required_properties_after_flushing(self):
        adapter = FakeAdapter()
        original = adapter.property

        def missing_role(window, name, property_type="UTF8_STRING"):
            if name == "WM_WINDOW_ROLE":
                return ""
            return original(window, name, property_type)

        adapter.property = missing_role
        with self.assertRaisesRegex(binding.X11Unavailable, "verification failed"):
            binding.bind_workspace_window({
                "nonce": "0123456789abcdef0123456789abcdef",
                "remote_key": "r_12345678", "workspace_id": "workspace",
                "generation": 7, "control_state": "remote",
                "display_title": "Abush Browser — local · workbench",
            }, adapter=adapter, attempts=1)

    def test_verify_workspace_window_requires_full_exact_identity(self):
        adapter = FakeAdapter()
        payload = {
            "xid": 22,
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678",
            "workspace_id": "workspace",
            "generation": 7,
            "control_state": "remote",
            "display_title": "Abush Browser — local · workbench",
        }
        binding.bind_workspace_window(
            {key: value for key, value in payload.items() if key != "xid"},
            adapter=adapter,
            attempts=1,
        )
        self.assertEqual(binding.verify_workspace_window(payload, adapter=adapter),
                         {"verified": True, "xid": 22})
        self.assertEqual(binding.verify_workspace_window(
            {**payload, "workspace_id": "reused"}, adapter=adapter),
            {"verified": False, "xid": 22})

    def test_clear_workspace_window_requires_exact_identity_and_verifies_cleanup(self):
        adapter = FakeAdapter()
        payload = {
            "xid": 22,
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678",
            "workspace_id": "workspace",
            "generation": 7,
            "control_state": "remote",
            "display_title": "Abush Browser — local · workbench",
        }
        binding.bind_workspace_window(
            {key: value for key, value in payload.items() if key != "xid"},
            adapter=adapter,
            attempts=1,
        )
        run = mock.Mock(return_value=mock.Mock(
            returncode=0, stdout=json.dumps({"id": 22, "group": "f"}), stderr=""))
        result = binding.clear_workspace_window(payload, adapter=adapter, run=run)
        self.assertEqual(result, {"cleared": True, "xid": 22, "reclassified_group": "f"})
        self.assertIsNone(binding.managed_metadata(adapter, 22))
        replay = binding.clear_workspace_window(payload, adapter=adapter, run=run)
        self.assertEqual(replay, {"cleared": True, "xid": 22, "reclassified_group": "f"})
        adapter = FakeAdapter()
        binding.bind_workspace_window(
            {key: value for key, value in payload.items() if key != "xid"},
            adapter=adapter,
            attempts=1,
        )
        with self.assertRaisesRegex(ValueError, "identity mismatch"):
            binding.clear_workspace_window({**payload, "workspace_id": "other"},
                                           adapter=adapter, run=run)
        self.assertIsNotNone(binding.managed_metadata(adapter, 22),
                             "mismatched cleanup must leave the verified owner untouched")

    def test_rejects_bad_nonce(self):
        with self.assertRaises(ValueError):
            binding.bind_workspace_window({
                "nonce": "guessable", "remote_key": "r_12345678",
                "workspace_id": "workspace", "generation": 1,
                "control_state": "remote",
                "display_title": "Abush Browser — local · workbench",
            }, adapter=FakeAdapter(), attempts=1)

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
        self.assertEqual(commands, [binding.qtile_move_command(11, "f")])
        self.assertNotIn("focus", commands[0])
        self.assertNotIn("toscreen", commands[0])

    def test_ordinary_prepare_refuses_managed_candidate(self):
        nonce = "b" * 32
        adapter = FakeAdapter()
        adapter.values[(11, "_NET_WM_NAME")] = "ordinary"
        adapter.values[(22, "_NET_WM_NAME")] = f"__ABUSH_ORDINARY__:{nonce}"
        adapter.values[(22, "WM_WINDOW_ROLE")] = "abush-browser"
        adapter.values[(22, "_ABUSH_BINDING_NONCE")] = nonce
        adapter.values[(22, "_ABUSH_REMOTE_ID")] = "r_12345678"
        adapter.values[(22, "_ABUSH_WORKSPACE_ID")] = "workspace"
        adapter.values[(22, "_ABUSH_WINDOW_GENERATION")] = "1"
        adapter.values[(22, "_ABUSH_CONTROL_STATE")] = "remote"
        adapter.values[(22, "_ABUSH_DISPLAY_TITLE")] = "local"
        with self.assertRaises(binding.X11Unavailable):
            binding.prepare_ordinary_window(
                {"nonce": nonce}, adapter=adapter, attempts=1,
                run=lambda *_args, **_kwargs: mock.Mock(
                    returncode=0, stdout=json.dumps({"id": 22, "group": "f"})))

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
        self.qtile_info_patch.stop()
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
        self.assertEqual(commands[-1], binding.qtile_activate_command(11))
        self.assertNotIn("True", commands[-1])
        self.assertNotIn("False", commands[-1])
        with self.assertRaises(ValueError):
            binding.activate_ordinary_window(
                {"nonce": nonce, "xid": True}, adapter=adapter, run=run)


class AcceptanceOutputTest(unittest.TestCase):
    def test_parses_explicit_xprop_fields(self):
        raw = '''WM_WINDOW_ROLE(STRING) = "abush-browser"
_ABUSH_REMOTE_ID(UTF8_STRING) = "r_12345678"
_ABUSH_WORKSPACE_ID(UTF8_STRING) = "workspace"
_ABUSH_WINDOW_GENERATION(UTF8_STRING) = "7"
_ABUSH_CONTROL_STATE(UTF8_STRING) = "remote"
WM_CLASS(STRING) = "chromium", "Chromium"
_NET_WM_NAME(UTF8_STRING) = "Managed page"
'''
        self.assertEqual(acceptance.parse_properties(raw), {
            "wm_role": "abush-browser", "remote_id": "r_12345678",
            "workspace_id": "workspace", "generation": "7", "control_state": "remote",
            "wm_class": ["chromium", "Chromium"], "title": "Managed page",
        })

    def test_acceptance_requires_all_qtile_properties_and_compares_focus(self):
        values = {field: field for field in acceptance.PROPERTY_FIELDS.values()}
        with mock.patch.object(acceptance, "focused", side_effect=["0x1", "0x1"]), \
                mock.patch.object(acceptance, "candidates", return_value=iter(["0x2"])), \
                mock.patch.object(acceptance, "properties", return_value=(values, "raw")):
            output = acceptance.acceptance(0.1)
        self.assertTrue(output["focus_preserved"])
        self.assertTrue(output["qtile_attrs_visible"])
        self.assertEqual(output["workspace_id"], "workspace_id")
        values.pop("control_state")
        with mock.patch.object(acceptance, "focused", side_effect=["0x1", "0x2"]), \
                mock.patch.object(acceptance, "candidates", return_value=iter(["0x2"])), \
                mock.patch.object(acceptance, "properties", return_value=(values, "raw")):
            output = acceptance.acceptance(0.1)
        self.assertFalse(output["focus_preserved"])
        self.assertFalse(output["qtile_attrs_visible"])

    def test_missing_xprop_or_xwininfo_is_a_json_error(self):
        for missing in ("xprop", "xwininfo"):
            def run(command, **_kwargs):
                if command[0] == missing:
                    raise FileNotFoundError(missing)
                return mock.Mock(
                    returncode=0,
                    stdout="_NET_ACTIVE_WINDOW(WINDOW): window id # 0x1\n",
                                 stderr="")

            with self.subTest(missing=missing), \
                    mock.patch.object(acceptance.subprocess, "run", side_effect=run), \
                    mock.patch("sys.argv", ["test_abush_browser_x11.py", "--timeout", "0.01"]):
                stream = io.StringIO()
                with redirect_stdout(stream):
                    acceptance.main()
            output = json.loads(stream.getvalue())
            self.assertEqual(output["error"], f"required command is unavailable: {missing}")
            self.assertFalse(output["remote_available"])


if __name__ == "__main__":
    unittest.main()
