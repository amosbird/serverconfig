#!/usr/bin/env python3

import importlib
import importlib.util
import pathlib
import sys
import unittest
from unittest import mock

from qtile_test_support import FakeWindow, OfflineQtile, hook_counts, window_effects

CONFIG = pathlib.Path(__file__).resolve().parents[1] / ".config/qtile/config.py"


class QtileConfigTest(unittest.TestCase):
    def test_chatgpt_shortcut_uses_recoverable_scratchpad(self):
        with OfflineQtile() as fixture:
            config = fixture.load()
            fixture.manager.rebuild()
            module = fixture.module("scratchpads")
            window = FakeWindow(fixture.manager)
            window.get_wm_class = lambda: ("chatgpt",)
            module.recover_windows(fixture.manager)
            dropdown = fixture.manager.groups_map["scratchpad"].dropdowns["chatgpt"]
            self.assertIs(dropdown.window, window)
            call = next(key for key in config.keys if key.key == "8"
                        and key.modifiers == ["control", "mod1"]).commands[0]
            call.args[0](fixture.manager)
            self.assertTrue(window.has_focus)

    def test_ctrl_alt_9_scratchpad_scan_skips_internal_qtile_windows(self):
        with OfflineQtile() as fixture:
            fixture.load()
            fixture.manager.rebuild()
            internal = type("Internal", (), {"name": "webchat", "wid": 101})()
            fixture.manager.windows_map[101] = internal
            module = fixture.module("scratchpads")
            module.register_window(fixture.manager, internal)
            module.recover_windows(fixture.manager)
            self.assertEqual(fixture.manager.groups_map["scratchpad"].dropdowns, {})

    def test_split_sources_import_without_installing_or_touching_windows(self):
        with OfflineQtile() as fixture:
            from libqtile.core.manager import Qtile
            from libqtile.scratchpad import ScratchPad
            sys.path.insert(0, str(CONFIG.parent))
            before = (Qtile.reload_config, ScratchPad.restore_state, hook_counts())
            for name in ("browser", "bindings", "declarations", "desktop", "patches",
                         "runtime", "scratchpads"):
                importlib.import_module("amos_qtile." + name)
            self.assertEqual((Qtile.reload_config, ScratchPad.restore_state, hook_counts()), before)
            self.assertEqual(window_effects(fixture.manager), [])
            self.assertFalse(any(event[0] == "spawn" for event in fixture.manager.trace))
            import libqtile
            with mock.patch.object(libqtile, "qtile", object()):
                spec = importlib.util.spec_from_file_location("offline_source_config", CONFIG)
                module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(module)
            self.assertTrue(module.keys)
            self.assertEqual((Qtile.reload_config, ScratchPad.restore_state, hook_counts()), before)

    def test_loaded_desktop_defaults_and_dropdowns(self):
        with OfflineQtile() as fixture:
            config = fixture.load()
            expected = dict(follow_mouse_focus=False, bring_front_click="floating_only",
                            floats_kept_above=True, cursor_warp=False, auto_fullscreen=False,
                            focus_on_window_activation="urgent", reconfigure_screens=True,
                            auto_minimize=False, wmname="LG3D")
            for name, value in expected.items():
                self.assertEqual(getattr(config, name), value, name)
            self.assertEqual([group.name for group in config.groups],
                             ["scratchpad", "h", "2", "c", "w", "e", "d", "f", "v", "n", "i", "o"])
            dropdowns = config.groups[0].dropdowns
            self.assertEqual([dd.name for dd in dropdowns], [
                "ioa", "tdesktop", "webchat", "chatgpt", "bookmarks", "stardict", "stalonetray",
            ])
            self.assertEqual([(dd.x, dd.y, dd.width, dd.height, dd.opacity)
                              for dd in dropdowns], [
                (0.26, 0.224, 0.8, 0.35, 1.0), (0.1, 0.1, 0.8, 0.85, 1.0),
                (0.1, 0.1, 0.8, 0.85, 1.0), (0.1, 0.1, 0.8, 0.85, 1.0),
                (0.1, 0.05, 0.8, 0.9, 1.0), (0.25, 0.1, 0.5, 0.8, 0.75),
                (0.1, 0.0, 0.8, 0.35, 0.9)])
            self.assertEqual([(dd.command, dd.on_focus_lost_hide, dd.warp_pointer)
                              for dd in dropdowns], [
                ("systemctl --user start ioagui.service", True, True),
                ("/opt/telegram/Telegram", False, True), ("runchat", False, True),
                ("runai", False, True), ("bookmark-manager", False, True),
                ("kitty -T stardict -e dict.sh", True, True), ("tray", True, True)])

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
