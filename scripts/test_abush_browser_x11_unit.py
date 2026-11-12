#!/usr/bin/env python3

import importlib.util
import io
import json
import pathlib
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
        adapter.xproto.Atom.NONE = 0
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
        with mock.patch.object(binding.pathlib.Path, "read_bytes") as read_bytes:
            read_bytes.return_value = b"/usr/bin/google-chrome-stable\0--type=browser\0"
            self.assertTrue(binding.browser_process(42, root))
            read_bytes.return_value = b"/usr/bin/python3\0script.py\0--name=chromium\0"
            self.assertFalse(binding.browser_process(42, root))


class X11BindingTest(unittest.TestCase):
    def setUp(self):
        self.process_patch = mock.patch.object(binding, "browser_process", return_value=True)
        self.process_patch.start()
        self.addCleanup(self.process_patch.stop)
        self.qtile_patch = mock.patch.object(binding, "notify_qtile", return_value=False)
        self.qtile_patch.start()
        self.addCleanup(self.qtile_patch.stop)

    def test_exact_nonce_chrome_match_and_metadata(self):
        adapter = FakeAdapter()
        result = binding.bind_workspace_window({
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678",
            "workspace_id": "workspace",
            "generation": 7,
            "control_state": "remote",
            "display_title": "Abush Browser — local · workbench",
        }, adapter=adapter, attempts=1)
        self.assertEqual(result["xid"], 22)
        self.assertEqual(result["observed"]["_ABUSH_REMOTE_ID"], "r_12345678")
        self.assertEqual(result["observed"]["_ABUSH_WINDOW_GENERATION"], "7")
        self.assertEqual(result["observed"]["WM_WINDOW_ROLE"], "abush-browser")
        self.assertEqual(adapter.values[(22, "WM_CLASS")], "abush-browser\0AbushBrowser\0")
        self.assertEqual(adapter.values[(11, "_NET_WM_NAME")], "ordinary")
        self.assertTrue(adapter.finished)

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
