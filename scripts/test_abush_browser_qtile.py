#!/usr/bin/env python3

import importlib.util
import json
import pathlib
import unittest
from types import SimpleNamespace
from unittest import mock

from qtile_test_support import OfflineQtile

ROOT = pathlib.Path(__file__).resolve().parents[1]
CONFIG_PATH = ROOT / ".config/qtile/config.py"


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

    def match(self, rule):
        return rule.compare(self)

    def get_wm_role(self):
        return self.role

    def get_wm_class(self):
        return self.wm_class


def load_browser_config(namespace):
    """Return isolated production globals; fixture overrides never leak to Qtile."""
    spec = importlib.util.spec_from_file_location(
        "offline_browser", CONFIG_PATH.parent / "amos_qtile/browser.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    with OfflineQtile() as fixture:
        fixture.load()
        before_window_created = fixture.module("runtime").before_window_created
    module.__dict__.update(
        QtileWindow=FakeWindow,
        Qtile=type("OfflineCommandRoot", (), {}),
        ordinary_browser_incarnation=lambda window: window.incarnation,
        before_window_created=before_window_created,
    )
    module.__dict__.update(namespace)
    return module.__dict__


class QtileConfigTest(unittest.TestCase):
    def test_group_c_uses_max_with_native_top_tabs(self):
        with OfflineQtile() as fixture:
            config = fixture.load()
            group = next(group for group in config.groups if group.name == "c")
            self.assertEqual(group.layouts[0].name, "max")
            top = config.screens[0].top
            task_list = top.widgets[0]
            self.assertEqual((top.size, task_list.highlight_method, task_list.font,
                              task_list.fontsize, task_list.padding_y),
                             (44, "block", "Ubuntu Mono", 32, 2))
            show = mock.Mock()
            screen = SimpleNamespace(top=SimpleNamespace(show=show),
                                     group=SimpleNamespace(name="c"))
            manager = SimpleNamespace(screens=[screen])
            desktop = fixture.module("desktop")
            desktop.initialize_browser_tabs(manager)
            screen.group.name = "f"
            desktop.show_browser_tabs(manager)
            self.assertEqual(show.call_args_list, [mock.call(True), mock.call(False)])

    def test_no_managed_gate_routing_or_control_initialized(self):
        source = "\n".join(path.read_text() for path in
                           (CONFIG_PATH.parent / "amos_qtile").glob("*.py"))
        for retired in (
            "__abush_quarantine", "handle_MapRequest", "handle_DestroyNotify",
            "install_abush_premap_gate", "ABUSH_CANDIDATE_WINDOWS", "ABUSH_CANDIDATE_LEASES",
            "claim_abush_browser_candidate", "place_abush_browser", "recover_abush_browsers",
            "reclassify_abush_browser", "prevent_abush_browser_focus_steal", "__ABUSH_BIND__",
        ):
            self.assertNotIn(retired, source)
        root = type("OfflineQtile", (), {"_commands": {"unrelated": object()}})
        ns = load_browser_config({"Qtile": root,
                                  "expose_command": lambda name: lambda function: function})
        ns["install_browser_commands"]()
        self.assertEqual(set(root._commands), {
            "unrelated", "prepare_ordinary_window", "activate_ordinary_window"})

    def test_normal_chrome_rule_remains_f(self):
        with OfflineQtile() as fixture:
            config = fixture.load()
            chrome = FakeWindow()
            rules = [rule for rule in config.dgroups_app_rules
                     if rule.matches(chrome)]
            self.assertEqual([rule.group for rule in rules], ["f"])
            self.assertFalse(any(rule.group == "c" for rule in config.dgroups_app_rules))
        self.assertFalse((ROOT / "scripts/wee").exists())
        self.assertFalse((ROOT / "scripts/kitty-notify").exists())

    def test_browser_fixture_globals_are_isolated_and_overrides_are_honored(self):
        root = type("CommandRoot", (), {"_commands": {}})
        class CustomWindow(FakeWindow):
            pass
        incarnation = mock.Mock(return_value=["fixture", 42, "7"])
        ns = load_browser_config({"Qtile": root, "QtileWindow": CustomWindow,
                                  "ordinary_browser_incarnation": incarnation,
                                  "expose_command": lambda name: lambda function: function})
        ns["install_browser_commands"]()
        self.assertIs(root._commands["prepare_ordinary_window"], ns["prepare_ordinary_window"])
        window = CustomWindow(name="__ABUSH_ORDINARY__:" + "a" * 32,
                              metadata={"_ABUSH_ORDINARY_NONCE": "a" * 32})
        expected = json.dumps({"version": 1, "nonce": "a" * 32,
                               "identity": ["fixture", 42, "7"]})
        ns["prepare_ordinary_window"](SimpleNamespace(windows_map={22: window}), 22, expected)
        incarnation.assert_called_once_with(window)
        other = load_browser_config({})
        self.assertIsNot(other["Qtile"], root)
        self.assertIs(other["QtileWindow"], FakeWindow)
        self.assertEqual(other["ordinary_browser_incarnation"](FakeWindow()), ["boot", 4242, "123"])
        with self.assertRaises(ValueError):
            ns["qtile_window_by_xid"](SimpleNamespace(windows_map={22: FakeWindow()}), 22)

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
