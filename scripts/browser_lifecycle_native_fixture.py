#!/usr/bin/env python3
"""Offline native/X11/Qtile endpoint for test_browser_lifecycle_release.js."""

import ast
import importlib.util
import json
import logging
import pathlib
import queue
import re
import sys
import time
from types import SimpleNamespace
from unittest import mock

from libqtile.command.base import expose_command
from libqtile.command.interface import lift_args

ROOT = pathlib.Path(__file__).resolve().parents[1]


def load(name, relative):
    spec = importlib.util.spec_from_file_location(name, ROOT / relative)
    module = importlib.util.module_from_spec(spec)
    with mock.patch.object(logging, "basicConfig"):
        spec.loader.exec_module(module)
    return module


host = load("lifecycle_host", "chromium/external-links/host.py")
binding = load("lifecycle_binding", "chromium/external-links/x11_binding.py")


class Window:
    def __init__(self, model, xid):
        self.model = model
        self.wid = xid
        self.name = ""
        self.props = {"WM_CLASS": "google-chrome\0Google-chrome", "WM_WINDOW_ROLE": "browser"}
        self.group = model.groups_map["f"]
        self.can_steal_focus = True
        self.window = self

    def get_property(self, name, *_args, **_kwargs):
        return self.props.get(name, "")

    def get_wm_class(self):
        return self.props["WM_CLASS"].split("\0")

    def get_wm_role(self):
        return self.props["WM_WINDOW_ROLE"]

    def togroup(self, group, switch_group=False):
        assert switch_group is False
        self.group = self.model.groups_map[group]
        self.model.record("togroup", self.wid)
        self.model.ns["prevent_abush_browser_focus_steal"](self.group, self)

    def focus(self, warp=False):
        assert warp is False
        self.model.current_window = self
        self.model.record("focus", self.wid)


class Group:
    def __init__(self, model, name):
        self.model, self.name = model, name

    def toscreen(self):
        self.model.current_screen.group = self
        self.model.record("toscreen")


class Model:
    def __init__(self):
        self.trace = []
        self.groups_map = {name: Group(self, name) for name in
                           ("f", "e", "c", "__abush_quarantine")}
        self.current_screen = SimpleNamespace(group=self.groups_map["e"])
        self.current_window = SimpleNamespace(wid=900)
        self.windows_map = {}
        self.chrome = []
        self.command_root = type("OfflineQtile", (), {"_commands": {}})
        self.ns = {"re": re, "json": json, "time": time, "QtileWindow": Window,
                   "Qtile": self.command_root, "expose_command": expose_command}
        tree = ast.parse((ROOT / ".config/qtile/config.py").read_text())
        names = {
            "qtile_window_by_xid", "prepare_ordinary_window", "activate_ordinary_window",
            "passive_window_move", "register_abush_browser_candidate",
            "release_abush_browser_candidate", "quarantine_abush_browser_window",
            "place_abush_browser_window", "reclassify_ordinary_window",
            "restore_abush_browser_window", "expire_abush_browser_candidate_leases",
            "claim_abush_browser_candidate", "is_chrome_window", "x11_text_property",
            "abush_browser_metadata", "is_abush_browser_verified", "has_abush_candidate_title",
            "is_abush_browser_candidate", "prevent_abush_browser_focus_steal",
            "reclassify_abush_browser", "place_abush_browser", "install_browser_commands",
        }
        nodes = []
        for node in tree.body:
            if isinstance(node, ast.Assign) and any(
                    isinstance(target, ast.Name) and target.id.startswith("ABUSH_")
                    for target in node.targets):
                nodes.append(node)
            if isinstance(node, ast.FunctionDef) and node.name in names:
                node.decorator_list = []
                nodes.append(node)
        exec(compile(ast.Module(nodes, []), "qtile-browser-helpers", "exec"), self.ns)
        self.ns["install_browser_commands"]()
        self.adapter = Adapter(self)
        self.completions = []
        ordinary = SimpleNamespace(complete=lambda payload: self.completions.append(payload))
        self.broker = host.BrowserBroker(remote_root=ROOT / ".absent-offline-remotes",
                                         outbound=queue.Queue(), ordinary_open=ordinary)
        self.broker.x11_binding = lambda: binding
        peer = SimpleNamespace(connected=True, metadata={"connection_id": "c_12345678"},
                               principal_id="principal-one", complete_ensure=self.complete_ensure)
        self.broker.peers["r_12345678"] = peer
        self.fault = None

    def complete_ensure(self, *args):
        self.completions.append({"ensure": args})

    def record(self, action, xid=None):
        self.trace.append({"action": action, "xid": xid,
                           "desktop": self.current_screen.group.name,
                           "focus": self.current_window.wid,
                           "chrome": self.chrome,
                           "windows": {str(wid): {"group": win.group.name, "title": win.name,
                                                   "props": dict(win.props)}
                                       for wid, win in self.windows_map.items()}})
        self.trace[-1] = json.loads(json.dumps(self.trace[-1]))

    def sync(self, message):
        self.chrome = message["windows"]
        present = {win["id"] + 100 for win in self.chrome}
        for xid in set(self.windows_map) - present:
            del self.windows_map[xid]
        for chrome in self.chrome:
            xid = chrome["id"] + 100
            fresh = xid not in self.windows_map
            window = self.windows_map.setdefault(xid, Window(self, xid))
            active = next((tab for tab in chrome["tabs"] if tab["active"]), None)
            title = (active or {}).get("title", "") + " - Google Chrome"
            if window.name != title:
                window.name = title
                window.props["_NET_WM_NAME"] = title
                if not fresh:
                    self.ns["reclassify_abush_browser"](window)
            if fresh:
                claimed = self.ns["claim_abush_browser_candidate"](window)
                window.togroup("__abush_quarantine" if claimed else "f")
                self.record("map:claimed" if claimed else "map:ordinary", xid)
        self.record(message["action"])
        return {}

    def ipc(self, selectors, function, arguments=(), **_kwargs):
        self.record("ipc:" + function)
        if selectors:
            assert function == "info" and len(selectors) == 1
            window = self.windows_map[selectors[0][1]]
            return {"id": window.wid, "group": window.group.name}
        assert function in self.command_root._commands, "InvalidCommand root: " + function
        cmd = self.command_root._commands[function]
        # Qtile IPC requests use lifted=True; do not pre-cast XIDs in the fake.
        args, kwargs = lift_args(cmd, arguments, {})
        return cmd(self, *args, **kwargs)

    def request(self, request):
        operation = request["operation"]
        self.record("native:" + operation)
        if self.fault == operation:
            self.fault = None
            raise RuntimeError("injected " + operation)
        return self.broker.handle_request(request)


class Adapter:
    def __init__(self, model):
        self.model = model

    def windows(self):
        return list(self.model.windows_map)

    def property(self, xid, name, property_type="UTF8_STRING"):
        return self.model.windows_map[xid].props.get(name, "")

    def title(self, xid):
        return self.property(xid, "_NET_WM_NAME")

    def wm_class(self, xid):
        return self.property(xid, "WM_CLASS").split("\0")

    def pid(self, xid):
        return 4242

    def active_window(self):
        return self.model.current_window.wid

    def set_text(self, xid, name, value, property_type="UTF8_STRING"):
        self.model.windows_map[xid].props[name] = str(value)
        self.model.record("property:" + name, xid)

    def delete_property(self, xid, name):
        self.model.windows_map[xid].props.pop(name, None)
        self.model.record("delete:" + name, xid)

    def finish(self):
        pass


def main():
    model = Model()
    with mock.patch.object(binding, "XcffibAdapter", return_value=model.adapter), \
            mock.patch.object(binding, "browser_process", return_value=True), \
            mock.patch.object(binding, "qtile_ipc_call", side_effect=model.ipc), \
            mock.patch.object(binding.time, "sleep", side_effect=AssertionError("unexpected poll")), \
            mock.patch.object(host.socket, "socket", side_effect=AssertionError("live socket")), \
            mock.patch.object(host.subprocess, "run", side_effect=AssertionError("live process")):
        for line in sys.stdin:
            message = json.loads(line)
            try:
                kind = message["kind"]
                if kind == "reset":
                    model = Model()
                    binding.XcffibAdapter.return_value = model.adapter
                    binding.qtile_ipc_call.side_effect = model.ipc
                    result = {}
                elif kind == "sync":
                    result = model.sync(message)
                elif kind == "fault":
                    model.fault = message["operation"]
                    result = {}
                elif kind == "state":
                    result = {"trace": model.trace, "completions": model.completions}
                else:
                    result = model.request(message["request"])
                reply = {"id": message["id"], "ok": True, "payload": result}
            except Exception as error:
                reply = {"id": message["id"], "ok": False,
                         "error": f"{type(error).__name__}: {error}"}
            print(json.dumps(reply), flush=True)


if __name__ == "__main__":
    main()
