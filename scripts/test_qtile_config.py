#!/usr/bin/env python3

import importlib.util
import pathlib
import unittest
from unittest import mock

CONFIG = pathlib.Path(__file__).resolve().parents[1] / ".config/qtile/config.py"


class QtileConfigTest(unittest.TestCase):
    def test_chatgpt_shortcut_uses_recoverable_scratchpad(self):
        config = CONFIG.read_text()
        self.assertIn('Key([ctrl, alt], "8", toggle_scratchpad("chatgpt"))', config)
        self.assertIn('"chatgpt": Match(wm_class="chatgpt")', config)
        self.assertIn("def recover_scratchpad_dropdowns():", config)
        self.assertIn("DropDownToggler(window, scratchpad.name, config)", config)
        self.assertIn("dropdown.window.focus(warp=True)", config)

    def test_ctrl_alt_9_scratchpad_scan_skips_internal_qtile_windows(self):
        config = CONFIG.read_text()
        self.assertIn('Key([ctrl, alt], "9", toggle_scratchpad("webchat"))', config)
        self.assertIn("from libqtile.backend.base import Window as QtileWindow", config)
        self.assertIn("if isinstance(window, QtileWindow) and match.compare(window)", config)
        self.assertIn(
            "def register_scratchpad_window(window, hide=True):\n"
            "    if not isinstance(window, QtileWindow):\n"
            "        return",
            config,
        )

    def test_ordinary_offline_fixture_runs_exact_ipc_without_managed_gate(self):
        fixture_path = CONFIG.parents[2] / "scripts/browser_lifecycle_native_fixture.py"
        spec = importlib.util.spec_from_file_location("ordinary_native_fixture", fixture_path)
        fixture = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(fixture)
        model = fixture.Model()
        nonce = "a" * 32
        model.sync({"windows": [{"id": 1, "tabs": [
            {"active": True, "title": f"__ABUSH_ORDINARY__:{nonce}"}]}]})
        with mock.patch.object(fixture.binding, "XcffibAdapter", return_value=model.adapter), \
                mock.patch.object(fixture.binding, "browser_process", return_value=True), \
                mock.patch.object(fixture.binding, "browser_incarnation",
                                  return_value=["offline-boot", 4242, "1234"]), \
                mock.patch.object(fixture.binding, "qtile_ipc_call", side_effect=model.ipc), \
                mock.patch.object(fixture.binding.socket, "socket",
                                  side_effect=AssertionError("live socket")), \
                mock.patch.object(fixture.binding.subprocess, "run",
                                  side_effect=AssertionError("live process")):
            prepared = model.request({"operation": "ordinary_prepare", "payload": {"nonce": nonce}})
            self.assertEqual(prepared, {"xid": 101, "nonce": nonce})
            self.assertEqual(model.current_screen.group.name, "e")
            self.assertEqual(model.current_window.wid, 900)
            self.assertFalse(model.windows_map[101].can_steal_focus)
            model.windows_map[101].name = "User page"
            activated = model.request({"operation": "ordinary_activate", "payload": prepared})
            self.assertEqual(activated, prepared)
            self.assertEqual(model.current_screen.group.name, "f")
            self.assertEqual(model.current_window.wid, 101)
            self.assertTrue(model.windows_map[101].can_steal_focus)
        with self.assertRaisesRegex(ValueError, "retired"):
            model.request({"operation": "x11_bind", "payload": {}})
        self.assertEqual(set(model.command_root._commands), {
            "prepare_ordinary_window", "activate_ordinary_window"})
        self.assertFalse(hasattr(model, "core"))


if __name__ == "__main__":
    unittest.main()
