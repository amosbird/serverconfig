#!/usr/bin/env python3

import ast
import json
import pathlib
import re
import unittest
from types import SimpleNamespace
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
CONFIG_PATH = ROOT / ".config/qtile/config.py"


def call_name(node):
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        return f"{call_name(node.value)}.{node.attr}"
    return ""


def literal_keyword(call, name):
    keyword = next(keyword for keyword in call.keywords if keyword.arg == name)
    return ast.literal_eval(keyword.value)


class FakeGroup:
    def __init__(self, name):
        self.name = name
        self.toscreen = mock.Mock()


class FakeWindow:
    def __init__(self, role="browser", group="f", name="", wm_class=None, metadata=None):
        self.role = role
        self.group = FakeGroup(group) if group is not None else None
        self.name = name
        self.wm_class = wm_class or ["google-chrome", "Google-chrome"]
        self.can_steal_focus = True
        self.wid = 22
        self.metadata = metadata or {}
        self.window = mock.Mock()
        self.window.get_property.side_effect = lambda name, _type, unpack=None: \
            self.metadata.get(name)
        self.window.get_name.side_effect = lambda: self.name
        self.incarnation = ["boot", 4242, "123"]
        self.togroup = mock.Mock(side_effect=self._togroup)
        self.focus = mock.Mock()

    def _togroup(self, group, switch_group=False):
        self.group = FakeGroup(group)

    def get_wm_role(self):
        return self.role

    def get_wm_class(self):
        return self.wm_class


def load_browser_config(namespace):
    tree = ast.parse(CONFIG_PATH.read_text())
    names = {
        "qtile_window_by_xid", "prepare_ordinary_window", "activate_ordinary_window",
        "ordinary_browser_incarnation", "x11_text_property", "is_chrome_window",
        "has_legacy_browser_binding", "before_window_created", "install_browser_commands",
    }
    nodes = []
    for node in tree.body:
        if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id in {
                    "ORDINARY_NONCE", "LEGACY_BROWSER_PROPERTIES"}
                for target in node.targets):
            nodes.append(node)
        elif isinstance(node, ast.FunctionDef) and node.name in names:
            node.decorator_list = []
            nodes.append(node)
    namespace.update(json=json, re=re, pathlib=pathlib, QtileWindow=FakeWindow)
    exec(compile(ast.fix_missing_locations(ast.Module(nodes, [])), str(CONFIG_PATH), "exec"),
         namespace)
    namespace["ordinary_browser_incarnation"] = lambda window: window.incarnation
    return namespace


class QtileConfigTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = CONFIG_PATH.read_text()
        cls.tree = ast.parse(cls.source)

    def test_group_c_uses_max_with_native_top_tabs(self):
        group_c = next(
            node for node in ast.walk(self.tree)
            if isinstance(node, ast.Call) and call_name(node.func) == "Group" and node.args
            and isinstance(node.args[0], ast.Constant) and node.args[0].value == "c")
        layouts = next(keyword.value for keyword in group_c.keywords if keyword.arg == "layouts")
        self.assertEqual(call_name(layouts.elts[0].func), "layout.Max")
        task_list = next(node for node in ast.walk(self.tree)
                         if isinstance(node, ast.Call) and call_name(node.func) == "widget.TaskList")
        self.assertEqual(literal_keyword(task_list, "highlight_method"), "block")
        self.assertEqual(literal_keyword(task_list, "font"), "Ubuntu Mono")
        self.assertEqual(literal_keyword(task_list, "fontsize"), 32)
        self.assertEqual(literal_keyword(task_list, "padding_y"), 2)
        bar_call = next(node for node in ast.walk(self.tree)
                        if isinstance(node, ast.Call) and call_name(node.func) == "bar.Bar")
        self.assertEqual(ast.literal_eval(bar_call.args[1]), 44)
        self.assertIn('@hook.subscribe.startup\ndef initialize_browser_tabs()', self.source)
        self.assertIn('screen.top.show(screen.group.name == "c")', self.source)
        self.assertIn('lazy.group["c"].toscreen()', self.source)

    def test_no_managed_gate_routing_or_control_initialized(self):
        for retired in (
            "__abush_quarantine", "handle_MapRequest", "handle_DestroyNotify", "on_config_load",
            "install_abush_premap_gate", "ABUSH_CANDIDATE_WINDOWS", "ABUSH_CANDIDATE_LEASES",
            "claim_abush_browser_candidate", "place_abush_browser", "recover_abush_browsers",
            "reclassify_abush_browser", "prevent_abush_browser_focus_steal", "__ABUSH_BIND__",
        ):
            self.assertNotIn(retired, self.source)
        root = type("OfflineQtile", (), {"_commands": {"unrelated": object()}})
        ns = load_browser_config({"Qtile": root,
                                  "expose_command": lambda name: lambda function: function})
        ns["install_browser_commands"]()
        self.assertEqual(set(root._commands), {
            "unrelated", "prepare_ordinary_window", "activate_ordinary_window"})

    def test_normal_chrome_rule_remains_f(self):
        rules = [ast.get_source_segment(self.source, node) for node in ast.walk(self.tree)
                 if isinstance(node, ast.Call) and call_name(node.func) == "Rule"]
        self.assertTrue(any('wm_class="Google-chrome"' in rule and 'group="f"' in rule
                            for rule in rules))
        self.assertFalse(any('group="c"' in rule or 'abush' in rule.lower() for rule in rules))
        self.assertNotIn("weechat", self.source.lower())
        self.assertFalse((ROOT / "scripts/wee").exists())
        self.assertFalse((ROOT / "scripts/kitty-notify").exists())

    def test_client_new_leaves_chrome_and_legacy_windows_untouched(self):
        ns = load_browser_config({})
        for window in (FakeWindow(group="c"), FakeWindow(role="abush-browser", group="c"),
                       FakeWindow(group="f", name="__ABUSH_BIND__:" + "a" * 32 + ":a")):
            ns["before_window_created"](window)
            window.togroup.assert_not_called()
            window.focus.assert_not_called()
            window.window.set_property.assert_not_called()
            self.assertTrue(window.can_steal_focus)

    def test_exact_prepare_then_activation_trace(self):
        ns = load_browser_config({})
        nonce = "a" * 32
        window = FakeWindow(group="e", name=f"__ABUSH_ORDINARY__:{nonce} - Google Chrome",
                            metadata={"_ABUSH_ORDINARY_NONCE": nonce})
        group = FakeGroup("f")
        manager = SimpleNamespace(windows_map={22: window}, groups_map={"f": group})
        expected = json.dumps({"version": 1, "nonce": nonce, "identity": window.incarnation})
        observed = []

        def move(destination, switch_group=False):
            observed.append((window.can_steal_focus, destination, switch_group))
            window._togroup(destination, switch_group)

        window.togroup.side_effect = move
        self.assertEqual(ns["prepare_ordinary_window"](manager, 22, expected),
                         {"id": 22, "group": "f"})
        self.assertEqual(observed, [(False, "f", False)])
        group.toscreen.assert_not_called()
        window.focus.assert_not_called()
        window.name = "User page - Google Chrome"
        self.assertEqual(ns["activate_ordinary_window"](manager, 22, expected),
                         {"id": 22, "group": "f"})
        self.assertTrue(window.can_steal_focus)
        group.toscreen.assert_called_once_with()
        window.focus.assert_called_once_with(warp=False)

    def test_exact_identity_failures_precede_mutation(self):
        ns = load_browser_config({})
        nonce = "b" * 32
        for command in ("prepare_ordinary_window", "activate_ordinary_window"):
            for change in ("nonce", "identity", "version", "extra", "legacy"):
                with self.subTest(command=command, change=change):
                    window = FakeWindow(name=f"__ABUSH_ORDINARY__:{nonce}",
                                        metadata={"_ABUSH_ORDINARY_NONCE": nonce})
                    manager = SimpleNamespace(windows_map={22: window},
                                              groups_map={"f": FakeGroup("f")})
                    expected = {"version": 1, "nonce": nonce, "identity": window.incarnation}
                    if change == "nonce":
                        expected["nonce"] = "c" * 32
                    elif change == "identity":
                        expected["identity"] = ["boot", 4242, "reused"]
                    elif change == "version":
                        expected["version"] = True
                    elif change == "extra":
                        expected["other"] = 1
                    else:
                        window.metadata["_ABUSH_BINDING_NONCE"] = "old-incomplete-binding"
                    with self.assertRaises(ValueError):
                        ns[command](manager, 22, json.dumps(expected))
                    window.togroup.assert_not_called()
                    window.focus.assert_not_called()
                    manager.groups_map["f"].toscreen.assert_not_called()
                    window.window.set_property.assert_not_called()
                    self.assertTrue(window.can_steal_focus)

    def test_invalid_xids_and_legacy_binding_fail_closed(self):
        ns = load_browser_config({})
        for role, classes, metadata in (
            ("abush-browser", ["Google-chrome"], {}),
            ("browser", ["AbushBrowser"], {}),
            *(("browser", ["Google-chrome"], {name: "legacy"})
              for name in ns["LEGACY_BROWSER_PROPERTIES"]),
        ):
            window = FakeWindow(role=role, wm_class=classes, metadata=metadata)
            manager = SimpleNamespace(windows_map={22: window})
            for command in ("prepare_ordinary_window", "activate_ordinary_window"):
                with self.assertRaises(ValueError):
                    ns[command](manager, 22)
            window.togroup.assert_not_called()
            window.focus.assert_not_called()
        manager = SimpleNamespace(windows_map={22: FakeWindow()})
        for xid in (True, "22", 0, -1, 23):
            with self.assertRaises(ValueError):
                ns["prepare_ordinary_window"](manager, xid)


if __name__ == "__main__":
    unittest.main()
