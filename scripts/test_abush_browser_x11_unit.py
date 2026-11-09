#!/usr/bin/env python3

import importlib.util
import io
import json
import pathlib
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
            (22, "_NET_WM_NAME"): "__ABUSH_BIND__:0123456789abcdef0123456789abcdef",
            (22, "WM_CLASS"): "chromium\0Chromium",
        }
        self.finished = False

    def windows(self):
        return [11, 22]

    def title(self, window):
        return self.values.get((window, "_NET_WM_NAME"), "")

    def wm_class(self, window):
        return self.values.get((window, "WM_CLASS"), "").split("\0")

    def set_text(self, window, name, value, property_type="UTF8_STRING"):
        self.values[(window, name)] = str(value)

    def property(self, window, name, property_type="UTF8_STRING"):
        return self.values.get((window, name), "")

    def finish(self):
        self.finished = True


class X11BindingTest(unittest.TestCase):
    def test_exact_nonce_chrome_match_and_metadata(self):
        adapter = FakeAdapter()
        result = binding.bind_workspace_window({
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678",
            "workspace_id": "workspace",
            "generation": 7,
            "control_state": "remote",
        }, adapter=adapter, attempts=1)
        self.assertEqual(result["xid"], 22)
        self.assertEqual(result["observed"]["_ABUSH_REMOTE_ID"], "r_12345678")
        self.assertEqual(result["observed"]["_ABUSH_WINDOW_GENERATION"], "7")
        self.assertEqual(adapter.values[(11, "_NET_WM_NAME")], "ordinary")
        self.assertTrue(adapter.finished)

    def test_rejects_bad_nonce(self):
        with self.assertRaises(ValueError):
            binding.bind_workspace_window({
                "nonce": "guessable", "remote_key": "r_12345678",
                "workspace_id": "workspace", "generation": 1,
                "control_state": "remote",
            }, adapter=FakeAdapter(), attempts=1)


class AcceptanceOutputTest(unittest.TestCase):
    def test_parses_explicit_xprop_fields(self):
        raw = '''WM_WINDOW_ROLE(STRING) = "abush-browser-workspace"
_ABUSH_REMOTE_ID(UTF8_STRING) = "r_12345678"
_ABUSH_WORKSPACE_ID(UTF8_STRING) = "workspace"
_ABUSH_WINDOW_GENERATION(UTF8_STRING) = "7"
_ABUSH_CONTROL_STATE(UTF8_STRING) = "remote"
WM_CLASS(STRING) = "chromium", "Chromium"
_NET_WM_NAME(UTF8_STRING) = "Managed page"
'''
        self.assertEqual(acceptance.parse_properties(raw), {
            "wm_role": "abush-browser-workspace", "remote_id": "r_12345678",
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
                return mock.Mock(returncode=0, stdout="_NET_ACTIVE_WINDOW(WINDOW): window id # 0x1\n",
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
