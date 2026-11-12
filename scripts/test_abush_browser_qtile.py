#!/usr/bin/env python3

import ast
import importlib.util
import pathlib
import re
import subprocess
import unittest
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
CONFIG_PATH = ROOT / ".config/qtile/config.py"
BINDING_PATH = ROOT / "chromium/external-links/x11_binding.py"

spec = importlib.util.spec_from_file_location("x11_binding_for_qtile_test", BINDING_PATH)
binding = importlib.util.module_from_spec(spec)
spec.loader.exec_module(binding)


def call_name(node):
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        return f"{call_name(node.value)}.{node.attr}"
    return ""


def literal_keyword(call, name):
    keyword = next(keyword for keyword in call.keywords if keyword.arg == name)
    return ast.literal_eval(keyword.value)


class FakeAdapter:
    def __init__(self):
        self.values = {
            (22, "_NET_WM_NAME"): "__ABUSH_BIND__:0123456789abcdef0123456789abcdef:a",
            (22, "WM_CLASS"): "chromium\0Chromium",
        }

    def windows(self):
        return [22]

    def title(self, window):
        return self.values.get((window, "_NET_WM_NAME"), "")

    def wm_class(self, window):
        return self.values.get((window, "WM_CLASS"), "").split("\0")

    def property(self, window, name, property_type="UTF8_STRING"):
        return self.values.get((window, name), "")

    def set_text(self, window, name, value, property_type="UTF8_STRING"):
        self.values[(window, name)] = str(value)

    def finish(self):
        pass


class FakeGroup:
    def __init__(self, name):
        self.name = name


class FakeWindow:
    def __init__(self, role, group="f", name="", wm_class=None):
        self.role = role
        self.group = FakeGroup(group)
        self.name = name
        self.wm_class = wm_class or ["google-chrome", "Google-chrome"]
        self.can_steal_focus = True
        self.wid = 22
        self.togroup = mock.Mock(side_effect=self._togroup)
        self.focus = mock.Mock()

    def _togroup(self, group, switch_group=False):
        self.group = FakeGroup(group)

    def get_wm_role(self):
        return self.role

    def get_wm_class(self):
        return self.wm_class


class FakeHook:
    class Subscribe:
        @staticmethod
        def group_window_add(function):
            return function

        @staticmethod
        def startup(function):
            return function

        @staticmethod
        def client_name_updated(function):
            return function

    subscribe = Subscribe()


class QtileConfigTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = CONFIG_PATH.read_text()
        cls.tree = ast.parse(cls.source)

    def test_group_c_uses_max_with_native_top_tabs(self):
        group_c = next(
            node
            for node in ast.walk(self.tree)
            if isinstance(node, ast.Call)
            and call_name(node.func) == "Group"
            and node.args
            and ast.literal_eval(node.args[0]) == "c"
        )
        layouts = next(keyword.value for keyword in group_c.keywords if keyword.arg == "layouts")
        self.assertEqual(call_name(layouts.elts[0].func), "layout.Max")
        task_list = next(
            node
            for node in ast.walk(self.tree)
            if isinstance(node, ast.Call) and call_name(node.func) == "widget.TaskList"
        )
        self.assertEqual(literal_keyword(task_list, "highlight_method"), "block")
        self.assertEqual(literal_keyword(task_list, "font"), "Ubuntu Mono")
        self.assertEqual(literal_keyword(task_list, "fontsize"), 32)
        self.assertEqual(literal_keyword(task_list, "padding_y"), 2)
        bar_call = next(
            node
            for node in ast.walk(self.tree)
            if isinstance(node, ast.Call) and call_name(node.func) == "bar.Bar"
        )
        self.assertEqual(ast.literal_eval(bar_call.args[1]), 44)
        self.assertIn('@hook.subscribe.startup\ndef initialize_browser_tabs()', self.source)
        self.assertIn('screen.top.show(screen.group.name == "c")', self.source)
        self.assertIn('lazy.group["c"].toscreen()', self.source)
        self.assertNotIn('lazy.window.togroup("c")', self.source)

    def test_quarantine_group_exists_and_only_ordinary_chrome_has_a_static_rule(self):
        groups = [
            node
            for node in ast.walk(self.tree)
            if isinstance(node, ast.Call) and call_name(node.func) == "Group" and node.args
        ]
        self.assertTrue(any(isinstance(group.args[0], ast.Constant) and
                            group.args[0].value == "__abush_quarantine" for group in groups))
        rules = [
            ast.get_source_segment(self.source, node)
            for node in ast.walk(self.tree)
            if isinstance(node, ast.Call) and call_name(node.func) == "Rule"
        ]
        self.assertFalse(any('role="abush-browser"' in rule for rule in rules),
                         "verified placement must use the exact-XID binder transition")
        self.assertTrue(any('wm_class=["Google-chrome", "Chromium"]' in rule and
                            'group="__abush_quarantine"' in rule for rule in rules))
        candidate = next(index for index, rule in enumerate(rules)
                         if 'group="__abush_quarantine"' in rule)
        ordinary = next(index for index, rule in enumerate(rules)
                        if 'wm_class="Google-chrome"' in rule and 'group="f"' in rule)
        self.assertLess(candidate, ordinary,
                        "candidate quarantine must win before the ordinary Chrome rule")
        self.assertTrue(any('wm_class="Google-chrome"' in rule and 'group="f"' in rule
                            for rule in rules))
        self.assertNotIn("weechat", self.source.lower())
        self.assertFalse((ROOT / "scripts/wee").exists())
        self.assertFalse((ROOT / "scripts/kitty-notify").exists())

    def test_group_add_guard_runs_at_focus_linearization_point(self):
        namespace = {"re": re, "QtileWindow": FakeWindow, "hook": FakeHook}
        functions = {
            node.name: node
            for node in self.tree.body
            if isinstance(node, ast.FunctionDef)
            and node.name in {
                "is_chrome_window",
                "is_abush_browser_verified",
                "is_abush_browser_candidate",
                "prevent_abush_browser_focus_steal",
            }
        }
        binding_title = next(
            node.value
            for node in self.tree.body
            if isinstance(node, ast.Assign)
            and any(isinstance(target, ast.Name) and target.id == "ABUSH_BINDING_TITLE"
                    for target in node.targets)
        )
        module = ast.Module(
            [ast.Assign([ast.Name("ABUSH_BINDING_TITLE", ast.Store())], binding_title),
             *functions.values()],
            [],
        )
        exec(compile(ast.fix_missing_locations(module), str(CONFIG_PATH), "exec"), namespace)

        for window in (
            FakeWindow("abush-browser"),
            FakeWindow(None, name="__ABUSH_BIND__:" + "a" * 32 + ":a - Google Chrome"),
        ):
            namespace["prevent_abush_browser_focus_steal"](FakeGroup("c"), window)
            self.assertFalse(window.can_steal_focus)

        ordinary = FakeWindow("browser", name="GitHub - Google Chrome")
        namespace["prevent_abush_browser_focus_steal"](FakeGroup("f"), ordinary)
        self.assertTrue(ordinary.can_steal_focus)

    def test_place_helper_enforces_three_state_routing_without_focus(self):
        namespace = {"re": re, "QtileWindow": FakeWindow}
        functions = {
            node.name: node
            for node in self.tree.body
            if isinstance(node, ast.FunctionDef)
            and node.name in {
                "is_chrome_window",
                "is_abush_browser_verified",
                "is_abush_browser_candidate",
                "place_abush_browser",
            }
        }
        binding_title = next(
            node.value
            for node in self.tree.body
            if isinstance(node, ast.Assign)
            and any(isinstance(target, ast.Name) and target.id == "ABUSH_BINDING_TITLE"
                    for target in node.targets)
        )
        module = ast.Module(
            [ast.Assign([ast.Name("ABUSH_BINDING_TITLE", ast.Store())], binding_title),
             *functions.values()],
            [],
        )
        exec(compile(ast.fix_missing_locations(module), str(CONFIG_PATH), "exec"), namespace)

        verified = FakeWindow("abush-browser", wm_class=["abush-browser", "AbushBrowser"])
        self.assertTrue(namespace["place_abush_browser"](verified))
        self.assertFalse(verified.can_steal_focus)
        verified.togroup.assert_called_once_with("c", switch_group=False)

        candidate = FakeWindow(None, name="__ABUSH_BIND__:" + "a" * 32 + ":a - Google Chrome")
        self.assertTrue(namespace["place_abush_browser"](candidate))
        self.assertFalse(candidate.can_steal_focus)
        candidate.togroup.assert_called_once_with("__abush_quarantine", switch_group=False)
        self.assertNotIn(mock.call("c", switch_group=False), candidate.togroup.mock_calls)

        stale_shell = FakeWindow("browser", group="c", name="GitHub - Google Chrome")
        self.assertTrue(namespace["place_abush_browser"](stale_shell))
        stale_shell.togroup.assert_called_once_with("f", switch_group=False)

        quarantined_shell = FakeWindow("browser", group="__abush_quarantine",
                                       name="GitHub - Google Chrome")
        self.assertTrue(namespace["place_abush_browser"](quarantined_shell))
        quarantined_shell.togroup.assert_called_once_with("f", switch_group=False)

        ordinary = FakeWindow("browser", group="f", name="GitHub - Google Chrome")
        self.assertFalse(namespace["place_abush_browser"](ordinary))
        ordinary.togroup.assert_not_called()

        impostor = FakeWindow(None, name="__ABUSH_BIND__:" + "b" * 32 + ":a",
                              wm_class=["kitty", "kitty"])
        self.assertFalse(namespace["place_abush_browser"](impostor))
        self.assertTrue(impostor.can_steal_focus)
        impostor.togroup.assert_not_called()

    def test_title_update_reclassifies_a_stale_candidate_shell(self):
        namespace = {"re": re, "QtileWindow": FakeWindow, "hook": FakeHook}
        function_names = {
            "is_chrome_window", "is_abush_browser_verified", "is_abush_browser_candidate",
            "place_abush_browser", "reclassify_abush_browser",
        }
        functions = [node for node in self.tree.body if isinstance(node, ast.FunctionDef)
                     and node.name in function_names]
        binding_title = next(
            node.value for node in self.tree.body if isinstance(node, ast.Assign)
            and any(isinstance(target, ast.Name) and target.id == "ABUSH_BINDING_TITLE"
                    for target in node.targets)
        )
        module = ast.Module(
            [ast.Assign([ast.Name("ABUSH_BINDING_TITLE", ast.Store())], binding_title), *functions],
            [],
        )
        exec(compile(ast.fix_missing_locations(module), str(CONFIG_PATH), "exec"), namespace)
        window = FakeWindow("browser", group="__abush_quarantine",
                            name="Restored work - Google Chrome")
        namespace["reclassify_abush_browser"](window)
        window.togroup.assert_called_once_with("f", switch_group=False)
    def test_exact_xid_prepare_and_activate_helpers_preserve_focus_policy(self):
        namespace = {"re": re, "QtileWindow": FakeWindow}
        function_names = {
            "is_chrome_window", "is_abush_browser_verified", "is_abush_browser_candidate",
            "qtile_window_by_xid", "prepare_ordinary_window", "activate_ordinary_window",
        }
        functions = [node for node in self.tree.body if isinstance(node, ast.FunctionDef)
                     and node.name in function_names]
        binding_title = next(
            node.value for node in self.tree.body if isinstance(node, ast.Assign)
            and any(isinstance(target, ast.Name) and target.id == "ABUSH_BINDING_TITLE"
                    for target in node.targets)
        )
        module = ast.Module(
            [ast.Assign([ast.Name("ABUSH_BINDING_TITLE", ast.Store())], binding_title), *functions],
            [],
        )
        exec(compile(ast.fix_missing_locations(module), str(CONFIG_PATH), "exec"), namespace)
        window = FakeWindow("browser", group="e", name="GitHub - Google Chrome")
        group = FakeGroup("f")
        group.toscreen = mock.Mock()
        manager = mock.Mock(windows_map={22: window}, groups_map={"f": group})

        prepared = namespace["prepare_ordinary_window"](manager, 22)
        self.assertEqual(prepared, {"id": 22, "group": "f"})
        self.assertFalse(window.can_steal_focus)
        window.togroup.assert_called_once_with("f", switch_group=False)
        group.toscreen.assert_not_called()
        window.focus.assert_not_called()

        activated = namespace["activate_ordinary_window"](manager, 22)
        self.assertEqual(activated, {"id": 22, "group": "f"})
        self.assertTrue(window.can_steal_focus)
        group.toscreen.assert_called_once_with()
        window.focus.assert_called_once_with(warp=False)
        with self.assertRaisesRegex(ValueError, "unknown X11"):
            namespace["prepare_ordinary_window"](manager, 23)


class BinderQtileNotificationTest(unittest.TestCase):
    def payload(self):
        return {
            "nonce": "0123456789abcdef0123456789abcdef",
            "remote_key": "r_12345678",
            "workspace_id": "workspace",
            "generation": 7,
            "control_state": "remote",
            "display_title": "Abush Browser — local · workbench",
        }

    def test_qtile_command_targets_exact_xid_without_switching(self):
        command = binding.qtile_move_command(22)
        self.assertEqual(command[:3], ["qtile", "cmd-obj", "--object"])
        self.assertIn("22", command)
        self.assertEqual(command[-2:], ["--args", "22"])
        self.assertEqual(command[3:6], ["root", "--function", "place_abush_browser_window"])
        self.assertNotIn("--kwargs", command)
        self.assertFalse(any(argument.startswith("switch_group=") for argument in command))
        self.assertNotIn("focus", command)
        self.assertNotIn("toscreen", command)

    def test_binding_notifies_after_verified_role_and_reports_move(self):
        adapter = FakeAdapter()
        notifier = mock.Mock(return_value=True)
        result = binding.bind_workspace_window(
            self.payload(), adapter=adapter, attempts=1, qtile_notifier=notifier
        )
        notifier.assert_called_once_with(22)
        self.assertEqual(
            adapter.property(22, "_ABUSH_DISPLAY_TITLE"),
            "Abush Browser — local · workbench",
        )
        self.assertEqual(
            adapter.property(22, "_NET_WM_VISIBLE_NAME"),
            "Abush Browser — local · workbench",
        )
        self.assertNotIn("_NET_WM_VISIBLE_NAME", result["observed"])
        self.assertTrue(result["qtile_moved"])

    def test_qtile_notification_failures_are_safe(self):
        for side_effect in (FileNotFoundError(), subprocess.TimeoutExpired("qtile", 2)):
            with self.subTest(error=type(side_effect).__name__):
                run = mock.Mock(side_effect=side_effect)
                self.assertFalse(binding.notify_qtile(22, run=run))
        run = mock.Mock(return_value=mock.Mock(returncode=1, stdout="", stderr="failed"))
        self.assertFalse(binding.notify_qtile(22, run=run))
        run.assert_called_once_with(
            binding.qtile_move_command(22),
            capture_output=True,
            text=True,
            check=False,
            timeout=2,
        )
        wrong = mock.Mock(return_value=mock.Mock(
            returncode=0, stdout='{"id":22,"group":"f"}', stderr=""))
        self.assertFalse(binding.notify_qtile(22, run=wrong))


if __name__ == "__main__":
    unittest.main()
