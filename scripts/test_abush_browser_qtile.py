#!/usr/bin/env python3

import ast
import importlib.util
import json
import os
import tempfile
import pathlib
import re
import subprocess
import unittest
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
CONFIG_PATH = ROOT / ".config/qtile/config.py"
BINDING_PATH = ROOT / "chromium/external-links/x11_binding.py"
TEMP_ROOT = pathlib.Path(subprocess.check_output(
    ["git", "rev-parse", "--git-common-dir"], cwd=ROOT, text=True).strip()).resolve() / \
    "../.worktrees/.tmp"
TEMP_ROOT.mkdir(parents=True, exist_ok=True)
TEST_DIRECTORY = tempfile.TemporaryDirectory(dir=TEMP_ROOT)
TEST_STATE = pathlib.Path(TEST_DIRECTORY.name)

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
            (1, "_NET_ACTIVE_WINDOW"): 11,
        }

    def windows(self):
        return [22]

    def title(self, window):
        return self.values.get((window, "_NET_WM_NAME"), "")

    def wm_class(self, window):
        return self.values.get((window, "WM_CLASS"), "").split("\0")

    def pid(self, _window):
        return 4242

    def property(self, window, name, property_type="UTF8_STRING"):
        return self.values.get((window, name), "")

    def active_window(self):
        return self.values.get((1, "_NET_ACTIVE_WINDOW"))

    def set_text(self, window, name, value, property_type="UTF8_STRING"):
        self.values[(window, name)] = str(value)

    def finish(self):
        pass


class FakeGroup:
    def __init__(self, name):
        self.name = name


class FakeWindow:
    def __init__(self, role, group="f", name="", wm_class=None, metadata=None):
        self.role = role
        self.group = FakeGroup(group) if group is not None else None
        self.name = name
        self.wm_class = wm_class or ["google-chrome", "Google-chrome"]
        self.can_steal_focus = True
        self.wid = 22
        self.metadata = ({
            "_ABUSH_BINDING_NONCE": "a" * 32,
            "_ABUSH_REMOTE_ID": "r_12345678",
            "_ABUSH_WORKSPACE_ID": "workspace",
            "_ABUSH_WINDOW_GENERATION": "7",
            "_ABUSH_CONTROL_STATE": "remote",
            "_ABUSH_DISPLAY_TITLE": "local",
        } if role == "abush-browser" else {}) if metadata is None else metadata
        self.window = mock.Mock()
        self.window.get_property.side_effect = lambda name, _type, unpack=None: \
            self.metadata.get(name)
        self.window.set_property.side_effect = lambda name, value, *_args: \
            self.metadata.__setitem__(name, value)
        self.qtile = mock.Mock(current_screen=mock.Mock(group=FakeGroup("e")),
                               current_window=mock.Mock(wid=99))
        self.incarnation = ["boot", 4242, "123"]
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


def load_browser_config(namespace):
    tree = ast.parse(CONFIG_PATH.read_text())
    names = {
        "admit_ordinary_premap", "finish_abush_browser_candidate", "abush_premap_browser_terminated",
        "qtile_window_by_xid", "prepare_ordinary_window", "activate_ordinary_window",
        "passive_window_move", "register_abush_browser_candidate",
        "release_abush_browser_candidate", "quarantine_abush_browser_window",
        "place_abush_browser_window", "reclassify_ordinary_window",
        "restore_abush_browser_window", "expire_abush_browser_candidate_leases",
        "abush_browser_incarnation", "save_abush_candidate_claim", "abush_candidate_claim",
        "claim_abush_browser_candidate", "x11_text_property", "is_chrome_window",
        "abush_browser_metadata", "is_abush_browser_verified", "has_abush_candidate_title",
        "is_abush_browser_candidate", "prevent_abush_browser_focus_steal",
        "place_abush_browser", "recover_abush_browsers", "reclassify_abush_browser",
        "before_window_created", "release_abush_candidate_window",
        "reserve_abush_premap", "abush_premap_identity", "arm_abush_browser_candidate",
        "admit_abush_browser_candidate", "handle_abush_map_request",
        "handle_abush_destroy_notify", "install_abush_premap_gate",
        "abush_premap_epoch", "abush_premap_journal_path", "save_abush_premap",
        "load_abush_premap", "persist_abush_premap",
    }
    nodes = []
    for node in tree.body:
        if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id.startswith("ABUSH_")
                for target in node.targets):
            nodes.append(node)
        elif isinstance(node, ast.FunctionDef) and node.name in names:
            node.decorator_list = []
            nodes.append(node)
    namespace.update(json=json, os=os, re=re, pathlib=pathlib, QtileWindow=FakeWindow)
    namespace.setdefault("time", mock.Mock(monotonic=mock.Mock(return_value=100.0)))
    exec(compile(ast.fix_missing_locations(ast.Module(nodes, [])), str(CONFIG_PATH), "exec"),
         namespace)
    namespace["abush_browser_incarnation"] = lambda window: window.incarnation
    namespace["abush_premap_journal_path"] = lambda: TEST_STATE / "premap.json"
    namespace["abush_premap_browser_terminated"] = lambda identity: False
    namespace["abush_premap_destroyed"] = lambda core, xid: True
    return namespace


class QtileConfigTest(unittest.TestCase):
    def setUp(self):
        self.process_patch = mock.patch.object(binding, "browser_process", return_value=True)
        self.process_patch.start()
        self.addCleanup(self.process_patch.stop)
        incarnation_patch = mock.patch.object(
            binding, "browser_incarnation", return_value=["boot", 4242, "start"])
        incarnation_patch.start()
        self.addCleanup(incarnation_patch.stop)

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

    def test_quarantine_group_exists_and_new_candidate_rule_precedes_ordinary_chrome(self):
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
        self.assertTrue(any('Match(func=claim_abush_browser_candidate)' in rule and
                            'group="__abush_quarantine"' in rule for rule in rules))
        candidate = next(index for index, rule in enumerate(rules)
                         if 'group="__abush_quarantine"' in rule)
        ordinary = next(index for index, rule in enumerate(rules)
                        if 'wm_class="Google-chrome"' in rule and 'group="f"' in rule)
        self.assertLess(candidate, ordinary,
                        "new leased candidate quarantine must win before the ordinary Chrome rule")
        self.assertTrue(any('wm_class="Google-chrome"' in rule and 'group="f"' in rule
                            for rule in rules))
        self.assertNotIn("weechat", self.source.lower())
        self.assertFalse((ROOT / "scripts/wee").exists())
        self.assertFalse((ROOT / "scripts/kitty-notify").exists())

    def test_candidate_rule_requires_single_use_unexpired_nonce_lease(self):
        clock = mock.Mock()
        clock.monotonic.return_value = 100.0
        namespace = {"re": re, "QtileWindow": FakeWindow, "time": clock}
        function_names = {
            "x11_text_property", "is_chrome_window", "expire_abush_browser_candidate_leases",
            "claim_abush_browser_candidate",
        }
        functions = [node for node in self.tree.body if isinstance(node, ast.FunctionDef)
                     and node.name in function_names]
        constant_names = {
            "ABUSH_CANDIDATE_TITLE", "ABUSH_CANDIDATE_NONCE_PROPERTY",
            "ABUSH_CANDIDATE_LEASE_SECONDS", "ABUSH_CANDIDATE_LEASES",
            "ABUSH_CANDIDATE_WINDOWS",
        }
        constants = [
            node for node in self.tree.body if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id in constant_names
                for target in node.targets)
        ]
        module = ast.Module([*constants, *functions], [])
        load_browser_config(namespace)
        nonce = "a" * 32
        marker_a = FakeWindow("browser", group="f",
                              name=f"__ABUSH_BIND__:{nonce}:a - Google Chrome")
        marker_b = FakeWindow("browser", group="f",
                              name=f"__ABUSH_BIND__:{nonce}:b - Google Chrome")

        marker_a.group = None
        marker_b.group = None
        self.assertFalse(namespace["claim_abush_browser_candidate"](marker_a),
                         "an orphan marker title must not classify an ordinary window")
        namespace["ABUSH_CANDIDATE_LEASES"][nonce] = 110.0
        self.assertFalse(namespace["claim_abush_browser_candidate"](marker_b),
                         "marker b is never a new candidate window proof")
        self.assertTrue(namespace["claim_abush_browser_candidate"](marker_a))
        self.assertNotIn(nonce, namespace["ABUSH_CANDIDATE_LEASES"])
        self.assertEqual(namespace["ABUSH_CANDIDATE_WINDOWS"][marker_a.wid]["nonce"], nonce)
        self.assertFalse(namespace["claim_abush_browser_candidate"](marker_a),
                         "a candidate lease must be single-use")

        namespace["ABUSH_CANDIDATE_LEASES"][nonce] = 99.0
        clock.monotonic.return_value = 111.0
        self.assertFalse(namespace["claim_abush_browser_candidate"](marker_a))
        namespace["expire_abush_browser_candidate_leases"]()
        self.assertNotIn(nonce, namespace["ABUSH_CANDIDATE_LEASES"])
        self.assertIn(marker_a.wid, namespace["ABUSH_CANDIDATE_WINDOWS"],
                      "admission expiry must never expire quarantine ownership")

    def test_group_add_guard_runs_at_focus_linearization_point(self):
        namespace = {"re": re, "QtileWindow": FakeWindow, "hook": FakeHook,
                     "time": mock.Mock()}
        functions = {
            node.name: node
            for node in self.tree.body
            if isinstance(node, ast.FunctionDef)
            and node.name in {
                "x11_text_property",
                "abush_browser_metadata",
                "is_chrome_window",
                "is_abush_browser_verified",
                "has_abush_candidate_title",
                "expire_abush_browser_candidate_leases",
                "is_abush_browser_candidate",
                "prevent_abush_browser_focus_steal",
            }
        }
        constants = [
            node for node in self.tree.body if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id in {
                    "ABUSH_CANDIDATE_TITLE", "ABUSH_NONCE",
                    "ABUSH_CANDIDATE_NONCE_PROPERTY", "ABUSH_CANDIDATE_LEASES",
                    "ABUSH_CANDIDATE_WINDOWS", "ABUSH_CONTROL_STATES",
                    "ABUSH_METADATA_PROPERTIES",
                } for target in node.targets)
        ]
        module = ast.Module([*constants, *functions.values()], [])
        load_browser_config(namespace)

        for window in (
            FakeWindow("abush-browser"),
            FakeWindow(None, group="__abush_quarantine",
                       name="__ABUSH_BIND__:" + "a" * 32 + ":a - Google Chrome",
                       metadata={"_ABUSH_CANDIDATE_NONCE": "a" * 32}),
        ):
            namespace["prevent_abush_browser_focus_steal"](FakeGroup("c"), window)
            self.assertFalse(window.can_steal_focus)

        ordinary = FakeWindow("browser", name="GitHub - Google Chrome")
        namespace["prevent_abush_browser_focus_steal"](FakeGroup("f"), ordinary)
        self.assertTrue(ordinary.can_steal_focus)

    def test_place_helper_enforces_three_state_routing_without_focus(self):
        namespace = {"re": re, "QtileWindow": FakeWindow, "time": mock.Mock()}
        namespace["time"].monotonic.return_value = 100.0
        functions = {
            node.name: node
            for node in self.tree.body
            if isinstance(node, ast.FunctionDef)
            and node.name in {
                "x11_text_property",
                "abush_browser_metadata",
                "is_chrome_window",
                "is_abush_browser_verified",
                "has_abush_candidate_title",
                "expire_abush_browser_candidate_leases",
                "is_abush_browser_candidate",
                "place_abush_browser",
            }
        }
        constants = [
            node for node in self.tree.body if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id in {
                    "ABUSH_CANDIDATE_TITLE", "ABUSH_NONCE",
                    "ABUSH_CANDIDATE_NONCE_PROPERTY", "ABUSH_CANDIDATE_LEASES",
                    "ABUSH_CANDIDATE_WINDOWS", "ABUSH_CONTROL_STATES",
                    "ABUSH_METADATA_PROPERTIES",
                } for target in node.targets)
        ]
        module = ast.Module([*constants, *functions.values()], [])
        load_browser_config(namespace)

        verified = FakeWindow("abush-browser", wm_class=["abush-browser", "AbushBrowser"])
        self.assertTrue(namespace["place_abush_browser"](verified))
        self.assertFalse(verified.can_steal_focus)
        verified.togroup.assert_called_once_with("c", switch_group=False)

        candidate = FakeWindow(None, group="__abush_quarantine",
                               name="__ABUSH_BIND__:" + "a" * 32 + ":a - Google Chrome",
                               metadata={"_ABUSH_CANDIDATE_NONCE": "a" * 32})
        namespace["ABUSH_CANDIDATE_WINDOWS"][candidate.wid] = {
            "nonce": "a" * 32, "window": candidate, "xid": candidate.wid,
            "incarnation": candidate.incarnation, "released": False, "placed": False,
        }
        self.assertFalse(namespace["place_abush_browser"](candidate))
        self.assertFalse(candidate.can_steal_focus)
        candidate.togroup.assert_not_called()
        self.assertNotIn(mock.call("c", switch_group=False), candidate.togroup.mock_calls)

        candidate.name = "about:blank - Google Chrome"
        self.assertFalse(namespace["place_abush_browser"](candidate),
                         "claimed candidate must remain continuously quarantined")
        candidate.togroup.assert_not_called()
        namespace["ABUSH_CANDIDATE_WINDOWS"].pop(candidate.wid)

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

        stale_role = FakeWindow("abush-browser", group="c", name="Stale - Google Chrome",
                                metadata={})
        self.assertTrue(namespace["place_abush_browser"](stale_role))
        stale_role.togroup.assert_called_once_with("f", switch_group=False)

        malformed = FakeWindow("abush-browser", group="c", metadata={
            "_ABUSH_BINDING_NONCE": "a" * 32,
            "_ABUSH_REMOTE_ID": "r_12345678",
            "_ABUSH_WORKSPACE_ID": "workspace",
            "_ABUSH_WINDOW_GENERATION": "not-an-int",
            "_ABUSH_CONTROL_STATE": "remote",
            "_ABUSH_DISPLAY_TITLE": "local",
        })
        self.assertTrue(namespace["place_abush_browser"](malformed))
        malformed.togroup.assert_called_once_with("f", switch_group=False)

        impostor = FakeWindow(None, name="__ABUSH_BIND__:" + "b" * 32 + ":a",
                              wm_class=["kitty", "kitty"])
        self.assertFalse(namespace["place_abush_browser"](impostor))
        self.assertTrue(impostor.can_steal_focus)
        impostor.togroup.assert_not_called()

    def test_title_update_reclassifies_a_stale_candidate_shell(self):
        namespace = {"re": re, "QtileWindow": FakeWindow, "hook": FakeHook,
                     "time": mock.Mock()}
        function_names = {
            "x11_text_property", "abush_browser_metadata", "is_chrome_window",
            "is_abush_browser_verified", "has_abush_candidate_title",
            "expire_abush_browser_candidate_leases",
            "is_abush_browser_candidate",
            "place_abush_browser", "reclassify_abush_browser",
        }
        functions = [node for node in self.tree.body if isinstance(node, ast.FunctionDef)
                     and node.name in function_names]
        constants = [
            node for node in self.tree.body if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id in {
                    "ABUSH_CANDIDATE_TITLE", "ABUSH_NONCE",
                    "ABUSH_CANDIDATE_NONCE_PROPERTY", "ABUSH_CANDIDATE_LEASES",
                    "ABUSH_CANDIDATE_WINDOWS", "ABUSH_CONTROL_STATES",
                    "ABUSH_METADATA_PROPERTIES",
                } for target in node.targets)
        ]
        module = ast.Module([*constants, *functions], [])
        load_browser_config(namespace)
        window = FakeWindow("browser", group="__abush_quarantine",
                            name="Restored work - Google Chrome")
        namespace["reclassify_abush_browser"](window)
        window.togroup.assert_called_once_with("f", switch_group=False)

    def test_marker_title_update_never_quarantines_an_existing_ordinary_window(self):
        namespace = {"re": re, "QtileWindow": FakeWindow, "hook": FakeHook,
                     "time": mock.Mock()}
        function_names = {
            "x11_text_property", "abush_browser_metadata", "is_chrome_window",
            "is_abush_browser_verified", "has_abush_candidate_title",
            "expire_abush_browser_candidate_leases",
            "is_abush_browser_candidate",
            "place_abush_browser", "reclassify_abush_browser",
        }
        functions = [node for node in self.tree.body if isinstance(node, ast.FunctionDef)
                     and node.name in function_names]
        constants = [
            node for node in self.tree.body if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id in {
                    "ABUSH_CANDIDATE_TITLE", "ABUSH_NONCE",
                    "ABUSH_CANDIDATE_NONCE_PROPERTY", "ABUSH_CANDIDATE_LEASES",
                    "ABUSH_CANDIDATE_WINDOWS", "ABUSH_CONTROL_STATES",
                    "ABUSH_METADATA_PROPERTIES",
                } for target in node.targets)
        ]
        module = ast.Module([*constants, *functions], [])
        load_browser_config(namespace)
        ordinary = FakeWindow("browser", group="f",
                              name="__ABUSH_BIND__:" + "a" * 32 + ":b - Google Chrome")

        namespace["reclassify_abush_browser"](ordinary)

        ordinary.togroup.assert_not_called()
        self.assertTrue(ordinary.can_steal_focus)

    def test_prepare_ordinary_window_runs_focus_guard_before_move(self):
        namespace = {"re": re, "QtileWindow": FakeWindow}
        function_names = {
            "x11_text_property", "abush_browser_metadata", "is_chrome_window",
            "is_abush_browser_verified", "is_abush_browser_candidate",
            "qtile_window_by_xid", "prepare_ordinary_window",
        }
        functions = [node for node in self.tree.body if isinstance(node, ast.FunctionDef)
                     and node.name in function_names]
        constants = [
            node for node in self.tree.body if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id in {
                    "ABUSH_CANDIDATE_TITLE", "ABUSH_NONCE",
                    "ABUSH_CANDIDATE_NONCE_PROPERTY", "ABUSH_CANDIDATE_LEASES",
                    "ABUSH_CANDIDATE_WINDOWS", "ABUSH_CONTROL_STATES",
                    "ABUSH_METADATA_PROPERTIES",
                } for target in node.targets)
        ]
        module = ast.Module([*constants, *functions], [])
        load_browser_config(namespace)
        window = FakeWindow("browser", group="e", name="GitHub - Google Chrome")
        observed = []

        def move(group, switch_group=False):
            observed.append((window.can_steal_focus, group, switch_group))
            window.group = FakeGroup(group)

        window.togroup.side_effect = move
        manager = mock.Mock(windows_map={22: window})
        namespace["prepare_ordinary_window"](manager, 22)
        self.assertEqual(observed, [(False, "f", False)])

    def test_exact_xid_prepare_and_activate_helpers_preserve_focus_policy(self):
        namespace = {"re": re, "QtileWindow": FakeWindow}
        function_names = {
            "x11_text_property", "abush_browser_metadata", "is_chrome_window",
            "is_abush_browser_verified", "is_abush_browser_candidate",
            "qtile_window_by_xid", "prepare_ordinary_window", "activate_ordinary_window",
        }
        functions = [node for node in self.tree.body if isinstance(node, ast.FunctionDef)
                     and node.name in function_names]
        constants = [
            node for node in self.tree.body if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id in {
                    "ABUSH_CANDIDATE_TITLE", "ABUSH_NONCE",
                    "ABUSH_CANDIDATE_NONCE_PROPERTY", "ABUSH_CANDIDATE_LEASES",
                    "ABUSH_CANDIDATE_WINDOWS", "ABUSH_CONTROL_STATES",
                    "ABUSH_METADATA_PROPERTIES",
                } for target in node.targets)
        ]
        module = ast.Module([*constants, *functions], [])
        load_browser_config(namespace)
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

    def test_exact_xid_candidate_quarantine_requires_nonce_property_and_preserves_focus(self):
        clock = mock.Mock()
        clock.monotonic.return_value = 100.0
        namespace = {"re": re, "QtileWindow": FakeWindow, "time": clock}
        function_names = {
            "qtile_window_by_xid", "x11_text_property", "abush_browser_metadata",
            "is_chrome_window", "is_abush_browser_verified", "passive_window_move",
            "quarantine_abush_browser_window",
        }
        functions = [node for node in self.tree.body if isinstance(node, ast.FunctionDef)
                     and node.name in function_names]
        constants = [
            node for node in self.tree.body if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id in {
                    "ABUSH_NONCE", "ABUSH_CANDIDATE_NONCE_PROPERTY",
                    "ABUSH_CANDIDATE_WINDOWS", "ABUSH_CONTROL_STATES",
                    "ABUSH_METADATA_PROPERTIES",
                } for target in node.targets)
        ]
        module = ast.Module([*constants, *functions], [])
        load_browser_config(namespace)
        nonce = "a" * 32
        candidate = FakeWindow("browser", group="__abush_quarantine",
                               name=f"__ABUSH_BIND__:{nonce}:a - Google Chrome",
                               metadata={"_ABUSH_CANDIDATE_NONCE": nonce})
        current = FakeWindow("browser", group="e")
        current.wid = 99
        screen = mock.Mock(group=FakeGroup("e"))
        manager = mock.Mock(windows_map={22: candidate}, current_screen=screen,
                            current_window=current)
        namespace["ABUSH_CANDIDATE_WINDOWS"][22] = {
            "nonce": nonce, "window": candidate, "xid": 22,
            "incarnation": candidate.incarnation, "released": False, "placed": False,
        }

        result = namespace["quarantine_abush_browser_window"](manager, 22, nonce)

        self.assertEqual(result["group"], "__abush_quarantine")
        self.assertEqual(result["current_group_before"], "e")
        self.assertEqual(result["current_group_after"], "e")
        self.assertEqual(result["active_xid_before"], 99)
        self.assertEqual(result["active_xid_after"], 99)
        self.assertFalse(candidate.can_steal_focus)
        candidate.togroup.assert_not_called()
        candidate.focus.assert_not_called()

        missing_proof = FakeWindow("browser", group="f", name=candidate.name, metadata={})
        manager.windows_map[22] = missing_proof
        namespace["ABUSH_CANDIDATE_WINDOWS"][22] = {
            "nonce": nonce, "window": candidate, "xid": 22,
            "incarnation": candidate.incarnation, "released": False, "placed": False,
        }
        with self.assertRaisesRegex(ValueError, "unverified"):
            namespace["quarantine_abush_browser_window"](manager, 22, nonce)
        missing_proof.togroup.assert_not_called()

    def test_exact_xid_managed_placement_verifies_metadata_and_preserves_focus(self):
        namespace = {
            "json": json,
            "re": re,
            "QtileWindow": FakeWindow,
        }
        function_names = {
            "qtile_window_by_xid", "x11_text_property", "abush_browser_metadata",
            "is_chrome_window", "is_abush_browser_verified", "passive_window_move",
            "place_abush_browser_window", "reclassify_ordinary_window",
        }
        functions = [node for node in self.tree.body if isinstance(node, ast.FunctionDef)
                     and node.name in function_names]
        constants = [
            node for node in self.tree.body if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id in {
                    "ABUSH_NONCE", "ABUSH_CANDIDATE_WINDOWS", "ABUSH_CONTROL_STATES",
                    "ABUSH_METADATA_PROPERTIES",
                } for target in node.targets)
        ]
        module = ast.Module([*constants, *functions], [])
        load_browser_config(namespace)
        window = FakeWindow("abush-browser", group="f")
        current = FakeWindow("browser", group="e")
        current.wid = 99
        screen = mock.Mock(group=FakeGroup("e"))
        manager = mock.Mock(windows_map={22: window}, current_screen=screen,
                            current_window=current)
        expected = {
            "nonce": "a" * 32,
            "remote_key": "r_12345678",
            "workspace_id": "workspace",
            "generation": 7,
            "control_state": "remote",
            "display_title": "local",
        }
        result = namespace["place_abush_browser_window"](
            manager, 22, json.dumps(expected))
        self.assertEqual(result["group"], "c")
        self.assertEqual(result["current_group_before"], "e")
        self.assertEqual(result["current_group_after"], "e")
        self.assertEqual(result["active_xid_before"], 99)
        self.assertEqual(result["active_xid_after"], 99)
        self.assertFalse(window.can_steal_focus)
        window.togroup.assert_called_once_with("c", switch_group=False)
        window.focus.assert_not_called()
        window.role = "browser"
        window.metadata = {}
        reclassified = namespace["reclassify_ordinary_window"](manager, 22)
        self.assertEqual(reclassified["group"], "f")
        window.focus.assert_not_called()
        window.role = "abush-browser"
        with self.assertRaisesRegex(ValueError, "unverified"):
            namespace["place_abush_browser_window"](
                manager, 22, json.dumps({**expected, "generation": 8}))

    def test_restore_helper_rejects_invalid_group_and_preserves_focus(self):
        namespace = {"QtileWindow": FakeWindow}
        function_names = {
            "qtile_window_by_xid", "passive_window_move", "restore_abush_browser_window",
        }
        functions = [node for node in self.tree.body if isinstance(node, ast.FunctionDef)
                     and node.name in function_names]
        module = ast.Module(functions, [])
        load_browser_config(namespace)
        window = FakeWindow("abush-browser", group="c")
        current = FakeWindow("browser", group="e")
        current.wid = 99
        screen = mock.Mock(group=FakeGroup("e"))
        manager = mock.Mock(windows_map={22: window}, groups_map={"c": FakeGroup("c"),
                            "f": FakeGroup("f")}, current_screen=screen,
                            current_window=current)

        restored = namespace["restore_abush_browser_window"](manager, 22, "f")
        self.assertEqual(restored["id"], 22)
        self.assertEqual(restored["group"], "f")
        self.assertEqual(restored["current_group_before"], "e")
        self.assertEqual(restored["current_group_after"], "e")
        self.assertEqual(restored["active_xid_before"], 99)
        self.assertEqual(restored["active_xid_after"], 99)
        window.togroup.assert_called_once_with("f", switch_group=False)
        window.focus.assert_not_called()
        with self.assertRaisesRegex(ValueError, "unknown rollback group"):
            namespace["restore_abush_browser_window"](manager, 22, "missing")
        with self.assertRaisesRegex(ValueError, "unknown X11"):
            namespace["restore_abush_browser_window"](manager, 23, "f")


class CandidateLifecycleTest(unittest.TestCase):
    def setUp(self):
        (TEST_STATE / "premap.json").unlink(missing_ok=True)
        self.ns = load_browser_config({})
        self.nonce = "a" * 32
        self.active = FakeWindow("browser", group="e")
        self.active.wid = 99
        self.manager = mock.Mock(windows_map={}, current_screen=mock.Mock(group=FakeGroup("e")),
                                 current_window=self.active)
        self.manager.groups_map = {name: FakeGroup(name)
                                   for name in ("e", "f", "c", "__abush_quarantine")}
        self.manager.core._abush_premap = None
        self.ns["qtile"] = self.manager
        self.trace = []

    def record(self, event, window):
        self.trace.append((event, window.group.name if window.group else None,
                           self.manager.current_screen.group.name,
                           self.manager.current_window.wid, window.can_steal_focus))

    def manage(self, window):
        window.qtile = self.manager
        original_move = window._togroup

        def move(group, switch_group=False):
            self.ns["prevent_abush_browser_focus_steal"](FakeGroup(group), window)
            original_move(group, switch_group)
            self.record("group_add", window)

        window.togroup.side_effect = move
        self.assertNotIn(window.wid, self.manager.windows_map)
        self.ns["before_window_created"](window)
        self.record("client_new", window)
        # Qtile DGroups._add skips preassigned clients before manager.manage inserts the XID.
        if window.group is None:
            if self.ns["claim_abush_browser_candidate"](window):
                window.togroup("__abush_quarantine")
            elif "Google-chrome" in window.get_wm_class():
                window.togroup("f")
        self.record("dgroups", window)
        self.manager.windows_map[window.wid] = window
        if window.group is None:
            window.togroup(self.manager.current_screen.group.name)
        self.record("managed", window)

    def candidate(self, title=None, premap=False):
        self.ns["ABUSH_CANDIDATE_LEASES"][self.nonce] = 110.0
        window = FakeWindow("browser", group=None,
                            name=title if title is not None else f"__ABUSH_BIND__:{self.nonce}:a")
        if premap:
            window.metadata["_ABUSH_CANDIDATE_NONCE"] = self.nonce
        self.manage(window)
        return window

    def assert_quarantined_trace(self):
        self.assertTrue(self.trace)
        for event, group, current, active, focus in self.trace:
            self.assertEqual((group, current, active, focus),
                             ("__abush_quarantine", "e", 99, False), event)

    def test_manage_title_expiry_release_and_reload_trace(self):
        window = self.candidate()
        for now, title in ((105, "blank"), (111, "restored"), (1000, "new title")):
            self.ns["time"].monotonic.return_value = now
            window.name = title
            self.ns["expire_abush_browser_candidate_leases"]()
            self.ns["reclassify_abush_browser"](window)
            self.record("title", window)
        self.ns["release_abush_browser_candidate"](self.manager, self.nonce)
        durable = window.metadata.copy()
        self.ns = load_browser_config({"qtile": self.manager})
        # A reload reconstructs Python windows but retains the exact X window properties.
        restored = FakeWindow("browser", group="__abush_quarantine", name="restored",
                              metadata=durable)
        restored.qtile = self.manager
        self.manager.windows_map[22] = restored
        self.ns["recover_abush_browsers"]()
        self.record("reload", restored)
        self.assert_quarantined_trace()
        with self.assertRaisesRegex(ValueError, "unverified"):
            self.ns["quarantine_abush_browser_window"](self.manager, 22, self.nonce)
        with self.assertRaisesRegex(ValueError, "cannot release"):
            self.ns["restore_abush_browser_window"](self.manager, 22, "f")

    def test_missing_or_different_title_uses_exact_premap_signal(self):
        for title in ("", "about:blank - Google Chrome"):
            with self.subTest(title=title):
                self.setUp()
                window = self.candidate(title, premap=True)
                self.assert_quarantined_trace()
                self.assertTrue(self.ns["is_abush_browser_candidate"](window))

    def test_orphan_titles_startup_selection_and_chrome_restore_stay_ordinary(self):
        for marker in ("a", "b"):
            for title in (f"__ABUSH_BIND__:{self.nonce}:{marker}", "Restored ordinary"):
                with self.subTest(title=title):
                    self.setUp()
                    window = FakeWindow("browser", group=None, name=title)
                    self.manage(window)
                    self.ns["recover_abush_browsers"]()
                    self.ns["reclassify_abush_browser"](window)
                    self.assertEqual(window.group.name, "f")
                    self.assertNotIn("__abush_quarantine", [row[1] for row in self.trace])
                    with self.assertRaises(ValueError):
                        self.ns["quarantine_abush_browser_window"](
                            self.manager, 22, self.nonce)
                    self.assertEqual(window.group.name, "f")

    def test_xid_reuse_does_not_inherit_claim_or_killed_authority(self):
        old = self.candidate()
        replacement = FakeWindow("browser", group="f", name=old.name)
        self.manager.windows_map[22] = replacement
        self.assertFalse(self.ns["is_abush_browser_candidate"](replacement))
        with self.assertRaises(ValueError):
            self.ns["quarantine_abush_browser_window"](self.manager, 22, self.nonce)
        self.ns["release_abush_candidate_window"](replacement)
        self.assertIs(self.ns["ABUSH_CANDIDATE_WINDOWS"][22]["window"], old)
        self.ns["release_abush_candidate_window"](old)
        self.assertNotIn(22, self.ns["ABUSH_CANDIDATE_WINDOWS"])
        self.assertFalse(self.ns["is_abush_browser_candidate"](replacement))
        with self.assertRaisesRegex(ValueError, "identity is required"):
            self.ns["register_abush_browser_candidate"](self.manager, self.nonce)

    def test_stale_browser_boot_property_is_not_recovered(self):
        old = self.candidate()
        self.ns = load_browser_config({"qtile": self.manager})
        restored = FakeWindow("browser", group="f", metadata=old.metadata.copy())
        restored.incarnation = ["boot", 4242, "new-start-time"]
        self.assertFalse(self.ns["is_abush_browser_candidate"](restored))
        self.assertFalse(self.ns["place_abush_browser"](restored))
        restored.togroup.assert_not_called()

    def test_unversioned_registration_rejected_before_dispatch(self):
        with self.assertRaisesRegex(ValueError, "identity is required"):
            self.ns["register_abush_browser_candidate"](self.manager, self.nonce)
        self.assertEqual(self.manager.windows_map, {})

    def test_passive_active_move_rejected_before_mutation_and_same_group_is_noop(self):
        self.active.togroup.reset_mock()
        with self.assertRaisesRegex(ValueError, "active X11"):
            self.ns["passive_window_move"](self.manager, self.active, "c")
        self.active.togroup.assert_not_called()
        self.assertTrue(self.active.can_steal_focus)
        self.ns["passive_window_move"](self.manager, self.active, "e")
        self.active.togroup.assert_not_called()
        self.assertEqual(self.manager.current_window.wid, 99)

    def test_expired_admission_still_allows_exact_candidate_verification(self):
        window = self.candidate()
        self.ns["time"].monotonic.return_value = 1000
        self.ns["expire_abush_browser_candidate_leases"]()
        result = self.ns["quarantine_abush_browser_window"](self.manager, 22, self.nonce)
        self.assertEqual(result["group"], "__abush_quarantine")
        self.assert_quarantined_trace()

    def test_rollback_after_placement_resets_commit_before_title_recovery(self):
        window = self.candidate()
        verified = FakeWindow("abush-browser")
        candidate_properties = window.metadata.copy()
        window.role = verified.role
        window.metadata.update(verified.metadata)
        self.ns["place_abush_browser_window"](
            self.manager, 22, json.dumps(self.ns["abush_browser_metadata"](window)))
        window.role = "browser"
        claim_property = window.metadata["_ABUSH_CANDIDATE_CLAIM"]
        window.metadata = {**candidate_properties, "_ABUSH_CANDIDATE_CLAIM": claim_property}
        self.ns["restore_abush_browser_window"](self.manager, 22, "__abush_quarantine")
        self.ns["release_abush_browser_candidate"](self.manager, self.nonce)
        self.ns["reclassify_abush_browser"](window)
        self.assertEqual(window.group.name, "__abush_quarantine")
        self.assertFalse(self.ns["ABUSH_CANDIDATE_WINDOWS"][22]["placed"])
        self.assertTrue(self.ns["ABUSH_CANDIDATE_WINDOWS"][22]["released"])

    def test_explicit_release_after_reload_recovers_exact_claim_before_revocation(self):
        window = self.candidate()
        self.ns = load_browser_config({"qtile": self.manager})
        self.ns["release_abush_browser_candidate"](self.manager, self.nonce)
        self.assertTrue(self.ns["ABUSH_CANDIDATE_WINDOWS"][22]["released"])
        self.assertTrue(self.ns["is_abush_browser_candidate"](window))
        with self.assertRaises(ValueError):
            self.ns["quarantine_abush_browser_window"](self.manager, 22, self.nonce)

    def test_metadata_before_exact_placement_does_not_escape_quarantine(self):
        window = self.candidate()
        verified = FakeWindow("abush-browser")
        window.role = verified.role
        window.metadata.update(verified.metadata)
        self.ns["place_abush_browser"](window)
        self.ns["reclassify_abush_browser"](window)
        self.record("metadata_written", window)
        self.assert_quarantined_trace()
        expected = self.ns["abush_browser_metadata"](window)
        self.ns["place_abush_browser_window"](self.manager, 22, json.dumps(expected))
        self.assertEqual(window.group.name, "c")
        self.assertEqual(self.manager.current_window.wid, 99)
        self.ns["release_abush_browser_candidate"](self.manager, self.nonce)
        self.assertFalse(self.ns["ABUSH_CANDIDATE_WINDOWS"][22]["released"])


class PremapBarrierTest(unittest.TestCase):
    def setUp(self):
        (TEST_STATE / "premap.json").unlink(missing_ok=True)
        self.ns = load_browser_config({})
        self.nonce = "a" * 32
        self.identity = ["boot", 4242, "123"]
        self.ns["abush_premap_identity"] = lambda pid: self.identity if pid == 4242 else None
        self.raw = {}
        self.ns["abush_premap_destroyed"] = lambda core, xid: xid not in self.raw
        self.trace = []
        test = self

        class Raw:
            def __init__(self, _conn, xid):
                self.wid = xid
                self.properties = test.raw.setdefault(xid, {
                    "_NET_WM_PID": [4242], "_NET_WM_DESKTOP": 0, "title": "",
                })

            def get_property(self, name, *_args, **_kwargs):
                value = self.properties.get(name, "")
                return [value] if name == "_NET_WM_DESKTOP" else value

            def set_property(self, name, value, *_args):
                if self.properties.get("fail_save") and name == "_ABUSH_CANDIDATE_CLAIM":
                    raise ValueError("persist failed")
                self.properties[name] = value

            def get_wm_class(self):
                return ["google-chrome", "Google-chrome"]

            def get_name(self):
                return self.properties["title"]

        class Core:
            def handle_MapRequest(core, event):
                if event.window in core.qtile.windows_map:
                    return
                raw = Raw(core.conn, event.window)
                window = FakeWindow("browser", group=None, name=raw.get_name(),
                                    metadata=raw.properties)
                window.wid = event.window
                window.qtile = core.qtile
                # Window.__init__.set_group precedes client_new and reads inherited desktop.
                group = core.qtile.groups[raw.properties["_NET_WM_DESKTOP"]]
                test.ns["prevent_abush_browser_focus_steal"](group, window)
                window.group = group
                test.trace.append((event.window, group.name, window.can_steal_focus))
                test.ns["before_window_created"](window)
                core.qtile.windows_map[event.window] = window

            def handle_DestroyNotify(core, event):
                core.qtile.windows_map.pop(event.window, None)

        self.Core = Core
        self.Raw = Raw
        self.ns["x11_core"] = mock.Mock(Core=Core, window=mock.Mock(XWindow=Raw))
        self.ns["install_abush_premap_gate"]()
        self.core = Core()
        self.core.conn = mock.Mock()
        self.manager = mock.Mock(windows_map={}, core=self.core,
                                 current_window=mock.Mock(wid=99))
        self.manager.groups = [FakeGroup(name) for name in ("f", "c", "__abush_quarantine")]
        self.manager.groups_map = {g.name: g for g in self.manager.groups}
        self.manager.current_screen.group = self.manager.groups[0]
        self.core.qtile = self.manager

    def reserve(self):
        return self.ns["register_abush_browser_candidate"](
            self.manager, self.nonce, json.dumps({"identity": self.identity,
                                                 "operation_token": "c" * 32}))

    def arm(self):
        self.reserve()
        self.ns["arm_abush_browser_candidate"](self.manager, self.nonce)

    def request(self, xid):
        self.core.handle_MapRequest(mock.Mock(window=xid))

    def admit(self, xid):
        self.raw[xid]["title"] = f"__ABUSH_BIND__:{self.nonce}:a"
        return self.ns["admit_abush_browser_candidate"](self.manager, xid, self.nonce)

    def finish(self, outcome="fulfilled_cleaned", window_id=10, **changes):
        terminal = {"version": 1, "nonce": self.nonce, "operation_token": "c" * 32,
                    "identity": self.identity, "outcome": outcome, "window_id": window_id,
                    **changes}
        return self.ns["finish_abush_browser_candidate"](self.manager, json.dumps(terminal))

    def test_exact_ordinary_bypasses_unknown_without_mapping_candidate(self):
        for current in ("f", "c"):
            with self.subTest(current=current):
                self.setUp()
                self.manager.current_screen.group = self.manager.groups_map[current]
                self.arm()
                self.request(22)
                self.request(33)
                nonce = "d" * 32
                self.raw[33].update(title=f"__ABUSH_ORDINARY__:{nonce}",
                                    _ABUSH_ORDINARY_NONCE=nonce, _NET_WM_DESKTOP=1)
                expected = json.dumps({"version": 1, "nonce": nonce, "identity": self.identity})
                self.ns["prepare_ordinary_window"](self.manager, 33, expected)
                self.ns["prepare_ordinary_window"](self.manager, 33, expected)
                self.request(33)
                self.assertEqual(self.trace, [(33, "f", False)])
                self.assertNotIn(22, self.manager.windows_map)
                self.assertIn(22, self.core._abush_premap["pending"])
                self.assertFalse(self.ns["release_abush_browser_candidate"](
                    self.manager, self.nonce)["released"])
                self.assertEqual(self.manager.current_screen.group.name, current)
                self.assertEqual(self.manager.current_window.wid, 99)

    def test_exact_ordinary_rejects_stale_or_foreign_evidence(self):
        changes = [{"_NET_WM_PID": []}, {"_ABUSH_PREMAP_TOKEN": "reused"},
                   {"_ABUSH_ORDINARY_NONCE": "e" * 32}, {"title": "New Tab"},
                   {"_ABUSH_CANDIDATE_CLAIM": "claim"}, {"WM_WINDOW_ROLE": "abush-browser"}]
        for change in changes:
            with self.subTest(change=change):
                self.setUp()
                self.arm()
                self.request(33)
                nonce = "d" * 32
                self.raw[33].update(title=f"__ABUSH_ORDINARY__:{nonce}",
                                    _ABUSH_ORDINARY_NONCE=nonce)
                self.raw[33].update(change)
                with self.assertRaises(ValueError):
                    self.ns["prepare_ordinary_window"](self.manager, 33, json.dumps(
                        {"version": 1, "nonce": nonce, "identity": self.identity}))
                self.assertEqual(self.trace, [])

    def test_known_terminals_restart_repeat_and_new_reservation(self):
        for outcome in ("not_invoked", "rejected", "fulfilled_cleaned"):
            with self.subTest(outcome=outcome):
                self.setUp()
                self.arm()
                window_id = 10 if outcome == "fulfilled_cleaned" else None
                receipt = self.finish(outcome, window_id)
                del self.core._abush_premap
                self.assertEqual(self.finish(outcome, window_id), receipt)
                self.assertTrue(self.ns["release_abush_browser_candidate"](
                    self.manager, self.nonce)["released"])
                self.assertEqual(self.finish(outcome, window_id), receipt)
                self.nonce = "b" * 32
                self.arm()

    def test_browser_termination_is_required_not_an_identity_read_failure(self):
        self.arm()
        self.ns["abush_premap_identity"] = lambda pid: None
        self.assertFalse(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])
        self.ns["abush_premap_browser_terminated"] = lambda identity: True
        self.assertTrue(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])

    def test_terminal_live_reused_candidate_and_overflow_remain_denied(self):
        self.arm()
        self.Raw(self.core.conn, 22).properties["title"] = f"__ABUSH_BIND__:{self.nonce}:a"
        self.request(22)
        with self.assertRaisesRegex(ValueError, "live or reused"):
            self.finish()
        self.raw.pop(22)
        self.core._abush_premap["operations"][self.nonce]["overflow"] = True
        with self.assertRaisesRegex(ValueError, "ambiguous"):
            self.finish()
        self.assertFalse(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])

    def test_exact_ordinary_restart_keeps_token_and_other_denials(self):
        self.arm()
        self.request(22)
        self.request(33)
        nonce = "d" * 32
        self.raw[33].update(title=f"__ABUSH_ORDINARY__:{nonce}", _ABUSH_ORDINARY_NONCE=nonce)
        del self.core._abush_premap
        self.ns["install_abush_premap_gate"]()
        self.ns["prepare_ordinary_window"](self.manager, 33, json.dumps(
            {"version": 1, "nonce": nonce, "identity": self.identity}))
        self.assertEqual(self.trace, [(33, "f", False)])
        self.assertIn(22, self.core._abush_premap["pending"])

    def test_terminal_rejects_stale_token_unknown_and_conflicts(self):
        self.arm()
        for changes in ({"operation_token": "e" * 32}, {"identity": ["boot", 4242, "old"]},
                        {"outcome": "unknown"}, {"version": True}):
            with self.assertRaises(ValueError):
                self.finish(**{"outcome": "not_invoked", "window_id": None, **changes})
        self.assertFalse(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])
        self.finish("not_invoked", None)
        with self.assertRaisesRegex(ValueError, "conflicting"):
            self.finish("rejected", None)

    def test_blank_map_late_title_expired_dispatched_and_concurrent_ordinary(self):
        for current in ("f", "c"):
            with self.subTest(current=current):
                self.setUp()
                self.manager.current_screen.group = self.manager.groups_map[current]
                self.request(11)
                self.assertEqual(self.trace, [(11, "f", True)])
                self.arm()
                self.request(11)
                self.request(22)
                self.request(33)
                self.ns["time"].monotonic.return_value = 10000
                self.assertEqual(len(self.trace), 1)
                self.assertNotIn(22, self.manager.windows_map)
                self.ns["release_abush_browser_candidate"](self.manager, self.nonce)
                self.request(22)
                self.assertEqual(len(self.trace), 1, "cancel cannot release ambiguous create")
                result = self.admit(22)
                self.assertEqual(result["first_group"], "__abush_quarantine")
                self.assertEqual(self.trace, [(11, "f", True),
                                             (22, "__abush_quarantine", False), (33, "f", False)])
                self.assertEqual(self.manager.current_screen.group.name, current)
                self.assertEqual(self.manager.current_window.wid, 99)

    def test_pending_limit_fails_closed_and_blocks_admission(self):
        self.arm()
        for xid in range(1000, 1128):
            self.request(xid)
        with self.assertRaisesRegex(ValueError, "pending limit"):
            self.request(1128)
        self.assertEqual(len(self.core._abush_premap["pending"]), 128)
        with self.assertRaisesRegex(ValueError, "not held"):
            self.admit(1000)
        self.assertEqual(self.trace, [])

    def test_unsupported_core_rejects_registration_before_create(self):
        self.Core.handle_MapRequest._abush_gate = False
        with self.assertRaisesRegex(ValueError, "unavailable"):
            self.reserve()
        self.assertEqual(self.trace, [])

    def test_reserved_expiry_rejects_before_dispatch(self):
        self.reserve()
        self.ns["time"].monotonic.return_value = 111
        with self.assertRaisesRegex(ValueError, "expired before dispatch"):
            self.ns["arm_abush_browser_candidate"](self.manager, self.nonce)
        self.assertEqual(self.trace, [])

    def test_save_error_stays_pending_and_reload_keeps_operation(self):
        self.arm()
        self.request(22)
        self.raw[22]["fail_save"] = True
        with self.assertRaisesRegex(ValueError, "persist failed"):
            self.admit(22)
        self.assertEqual(self.trace, [])
        self.assertIn(22, self.core._abush_premap["pending"])
        self.ns["install_abush_premap_gate"]()
        self.assertEqual(self.core._abush_premap["operations"][self.nonce]["phase"], "dispatched")
        del self.raw[22]["fail_save"]
        with self.assertRaisesRegex(ValueError, "not held"):
            self.admit(22)
        self.assertEqual(self.trace, [])

    def test_release_unknown_is_truthful_and_restart_retains_barrier(self):
        self.arm()
        receipt = self.ns["release_abush_browser_candidate"](self.manager, self.nonce)
        self.assertEqual(receipt, {"released": False, "nonce": self.nonce,
            "admission_version": 2, "phase": "dispatched", "denial_retained": True})
        del self.core._abush_premap
        self.request(22)
        self.assertEqual(self.trace, [])
        self.assertIn(22, self.core._abush_premap["pending"])
        self.admit(22)
        self.assertEqual(self.trace, [(22, "__abush_quarantine", False)])

    def test_missing_pid_candidate_never_reaches_constructor(self):
        self.arm()
        self.Raw(self.core.conn, 22).properties.update(
            _NET_WM_PID=[], title=f"__ABUSH_BIND__:{self.nonce}:a")
        self.request(22)
        self.assertEqual(self.trace, [])
        with self.assertRaisesRegex(ValueError, "identity"):
            self.admit(22)

    def test_duplicate_admission_is_exact_and_idempotent(self):
        self.arm()
        self.request(22)
        first = self.admit(22)
        self.assertEqual(self.admit(22), first)
        self.assertEqual(len(self.trace), 1)
        self.raw[22]["_ABUSH_PREMAP_TOKEN"] = "replacement"
        with self.assertRaisesRegex(ValueError, "token changed"):
            self.admit(22)

    def test_reserved_abort_allows_fresh_operation(self):
        self.reserve()
        self.assertTrue(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])
        self.nonce = "b" * 32
        self.arm()
        self.request(22)
        self.admit(22)

    def test_exact_destroy_resolves_and_replays_ordinary_not_quarantine(self):
        self.arm()
        self.Raw(self.core.conn, 22).properties["title"] = f"__ABUSH_BIND__:{self.nonce}:a"
        self.request(22)
        self.request(33)
        self.core.handle_DestroyNotify(mock.Mock(window=22))
        self.raw.pop(22)
        self.assertFalse(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])
        self.finish()
        self.assertTrue(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])
        self.assertEqual(self.trace, [(33, "f", False)])
        self.nonce = "b" * 32
        self.arm()
        self.request(44)
        self.admit(44)

    def test_malformed_journal_fails_closed_and_surfaces_to_native(self):
        (TEST_STATE / "premap.json").write_text("{")
        with self.assertRaisesRegex(ValueError, "restoration failed"):
            self.request(22)
        with self.assertRaisesRegex(ValueError, "restoration failed"):
            self.reserve()
        self.assertEqual(self.trace, [])

    def test_reload_between_reserve_and_arm_blocks_dispatch(self):
        self.reserve()
        self.ns["install_abush_premap_gate"]()
        with self.assertRaisesRegex(ValueError, "expired before dispatch"):
            self.ns["arm_abush_browser_candidate"](self.manager, self.nonce)

    def test_journal_write_failure_blocks_dispatch_and_map(self):
        self.reserve()
        with mock.patch.object(os, "replace", side_effect=OSError("disk failure")):
            with self.assertRaisesRegex(OSError, "disk failure"):
                self.ns["arm_abush_browser_candidate"](self.manager, self.nonce)
            with self.assertRaisesRegex(ValueError, "persistence failed"):
                self.request(22)
        self.assertEqual(self.trace, [])

    def test_process_epoch_reset_blocks_admit_but_never_releases_late_candidate(self):
        self.arm()
        self.request(22)
        del self.core._abush_premap
        original_epoch = self.ns["abush_premap_epoch"]
        self.ns["abush_premap_epoch"] = lambda core: ["new-process", 1]
        self.request(33)
        with self.assertRaisesRegex(ValueError, "not held"):
            self.admit(22)
        self.assertFalse(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])
        self.assertEqual(self.trace, [])
        self.ns["abush_premap_epoch"] = original_epoch

    def test_destroy_reused_xid_does_not_resolve_old_candidate(self):
        self.arm()
        self.Raw(self.core.conn, 22).properties["title"] = f"__ABUSH_BIND__:{self.nonce}:a"
        self.request(22)
        self.ns["abush_premap_destroyed"] = lambda core, xid: False
        self.core.handle_DestroyNotify(mock.Mock(window=22))
        self.assertFalse(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])
        self.request(22)
        self.assertEqual(self.trace, [])

    def test_constructor_failure_retains_exact_candidate_for_destroy_cleanup(self):
        self.arm()
        self.request(22)
        original = self.Core._abush_original_map_request
        self.Core._abush_original_map_request = mock.Mock(side_effect=ValueError("constructor"))
        with self.assertRaisesRegex(ValueError, "constructor"):
            self.admit(22)
        self.assertFalse(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])
        self.Core._abush_original_map_request = original
        self.core.handle_DestroyNotify(mock.Mock(window=22))
        self.raw.pop(22)
        self.assertFalse(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])
        self.finish()
        self.assertTrue(self.ns["release_abush_browser_candidate"](
            self.manager, self.nonce)["released"])
        self.request(33)
        self.assertEqual(self.trace, [(33, "f", True)])

    def test_destroy_xid_reuse_and_incarnation_replacement_reject_old_admit(self):
        self.arm()
        self.request(22)
        old_token = self.raw[22]["_ABUSH_PREMAP_TOKEN"]
        self.core.handle_DestroyNotify(mock.Mock(window=22))
        self.raw.pop(22)
        with self.assertRaisesRegex(ValueError, "not held"):
            self.admit_missing(22)
        self.request(22)
        self.assertNotEqual(self.raw[22]["_ABUSH_PREMAP_TOKEN"], old_token)
        self.identity = ["boot", 4242, "reused"]
        with self.assertRaisesRegex(ValueError, "identity"):
            self.admit(22)
        self.assertEqual(self.trace, [])

    def admit_missing(self, xid):
        return self.ns["admit_abush_browser_candidate"](self.manager, xid, self.nonce)


class BinderQtileNotificationTest(unittest.TestCase):
    def setUp(self):
        self.process_patch = mock.patch.object(binding, "browser_process", return_value=True)
        self.process_patch.start()
        self.addCleanup(self.process_patch.stop)
        incarnation_patch = mock.patch.object(
            binding, "browser_incarnation", return_value=["boot", 4242, "start"])
        incarnation_patch.start()
        self.addCleanup(incarnation_patch.stop)
        self.qtile_info_patch = mock.patch.object(
            binding, "qtile_window_info", return_value={"id": 22, "group": "f"})
        self.qtile_info_patch.start()
        self.addCleanup(self.qtile_info_patch.stop)

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
        expected = binding.expected_managed_metadata(self.payload())
        command = binding.qtile_move_command(22, expected=expected)
        self.assertEqual(command[:3], ["qtile", "cmd-obj", "--object"])
        self.assertIn("22", command)
        self.assertEqual(command[-3:-1], ["--args", "22"])
        self.assertEqual(json.loads(command[-1]), expected)
        self.assertEqual(command[3:6], ["root", "--function", "place_abush_browser_window"])
        self.assertNotIn("--kwargs", command)
        self.assertFalse(any(argument.startswith("switch_group=") for argument in command))
        self.assertNotIn("focus", command)
        self.assertNotIn("toscreen", command)

    def test_binding_notifies_after_verified_role_and_reports_move(self):
        adapter = FakeAdapter()
        notifier = mock.Mock(return_value={
            "id": 22,
            "group": "c",
            "current_group_before": "e",
            "current_group_after": "e",
            "active_xid_before": 11,
            "active_xid_after": 11,
            "current_group_preserved": True,
            "active_xid_preserved": True,
        })
        candidate_notifier = mock.Mock(return_value={
            "id": 22,
            "group": "__abush_quarantine",
            "current_group_before": "e",
            "current_group_after": "e",
            "active_xid_before": 11,
            "active_xid_after": 11,
            "current_group_preserved": True,
            "active_xid_preserved": True,
        })
        result = binding.bind_workspace_window(
            self.payload(), adapter=adapter, attempts=1, qtile_notifier=notifier,
            qtile_candidate_notifier=candidate_notifier,
        )
        candidate_notifier.assert_called_once_with(22, self.payload()["nonce"])
        notifier.assert_called_once_with(22, binding.expected_managed_metadata(self.payload()))
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

    def test_qtile_notification_failures_are_fatal(self):
        adapter = FakeAdapter()
        adapter.active_window = mock.Mock(return_value=11)
        expected = binding.expected_managed_metadata(self.payload())
        for side_effect in (FileNotFoundError(), subprocess.TimeoutExpired("qtile", 2)):
            with self.subTest(error=type(side_effect).__name__), \
                    self.assertRaises(binding.X11Unavailable):
                binding.notify_qtile(22, expected, adapter,
                                     run=mock.Mock(side_effect=side_effect))
        run = mock.Mock(return_value=mock.Mock(returncode=1, stdout="", stderr="failed"))
        with self.assertRaises(binding.X11Unavailable):
            binding.notify_qtile(22, expected, adapter, run=run)
        self.assertEqual(run.call_args.args[0], binding.qtile_move_command(22, expected=expected))
        wrong = mock.Mock(return_value=mock.Mock(
            returncode=0, stdout='{"id":22,"group":"f"}', stderr=""))
        with self.assertRaises(binding.X11Unavailable):
            binding.notify_qtile(22, expected, adapter, run=wrong)


if __name__ == "__main__":
    unittest.main()
