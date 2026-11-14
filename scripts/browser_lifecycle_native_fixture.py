#!/usr/bin/env python3
"""Offline ordinary-window X11/Qtile endpoint; never connects to a live WM."""

import importlib.util
import json
import pathlib
import sys
from types import SimpleNamespace
from unittest import mock

from libqtile.command.base import expose_command
from libqtile.command.interface import lift_args

from test_abush_browser_qtile import FakeWindow, load_browser_config

ROOT = pathlib.Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location(
    "ordinary_fixture_binding", ROOT / "chromium/external-links/x11_binding.py")
binding = importlib.util.module_from_spec(spec)
spec.loader.exec_module(binding)


class Window(FakeWindow):
    def __init__(self, model, xid, title):
        super().__init__(name=title)
        self.wid = xid
        self.model = model
        self.incarnation = ["offline-boot", 4242, "1234"]
        self.metadata.update({"WM_CLASS": "google-chrome\0Google-chrome",
                              "_NET_WM_NAME": title, "WM_WINDOW_ROLE": "browser"})
        self.focus.side_effect = self._focus

    def _togroup(self, group, switch_group=False):
        assert switch_group is False
        self.group = self.model.groups_map[group]
        self.model.record("togroup", self.wid)

    def _focus(self, warp=False):
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
        self.groups_map = {name: Group(self, name) for name in ("f", "e", "c")}
        self.current_screen = SimpleNamespace(group=self.groups_map["e"])
        self.current_window = SimpleNamespace(wid=900)
        self.windows_map = {}
        self.command_root = type("OfflineQtile", (), {"_commands": {}})
        self.ns = load_browser_config({"Qtile": self.command_root,
                                       "expose_command": expose_command})
        self.ns["install_browser_commands"]()
        self.adapter = Adapter(self)
        self.fault = None

    def record(self, action, xid=None):
        self.trace.append({
            "action": action, "xid": xid, "desktop": self.current_screen.group.name,
            "focus": self.current_window.wid,
            "windows": {str(wid): {"group": win.group.name, "title": win.name,
                                   "can_steal_focus": win.can_steal_focus,
                                   "props": dict(win.metadata)}
                        for wid, win in self.windows_map.items()},
        })

    def sync(self, message):
        present = {chrome["id"] + 100 for chrome in message["windows"]}
        for xid in set(self.windows_map) - present:
            del self.windows_map[xid]
        for chrome in message["windows"]:
            xid = chrome["id"] + 100
            active = next((tab for tab in chrome["tabs"] if tab["active"]), None)
            title = (active or {}).get("title", "") + " - Google Chrome"
            if xid not in self.windows_map:
                window = Window(self, xid, title)
                self.windows_map[xid] = window
                self.ns["before_window_created"](window)
                window.group = self.groups_map["f"]
                self.record("map:ordinary", xid)
            window = self.windows_map[xid]
            window.name = title
            window.metadata["_NET_WM_NAME"] = title
            self.record("title:published", xid)
        self.record(message.get("action", "sync"))
        return {}

    def ipc(self, selectors, function, arguments=(), **_kwargs):
        self.record("ipc:" + function)
        if selectors:
            assert function == "info" and len(selectors) == 1
            window = self.windows_map[selectors[0][1]]
            return {"id": window.wid, "group": window.group.name}
        if function not in self.command_root._commands:
            raise ValueError("unsupported ordinary Qtile command: " + function)
        command = self.command_root._commands[function]
        args, kwargs = lift_args(command, arguments, {})
        return command(self, *args, **kwargs)

    def request(self, request):
        operation = request["operation"]
        functions = {
            "ordinary_prepare": binding.prepare_ordinary_window,
            "ordinary_activate": binding.activate_ordinary_window,
            "ordinary_verify": binding.verify_prepared_ordinary_window,
        }
        if operation not in functions:
            raise ValueError("retired or unsupported native operation: " + operation)
        if self.fault == operation:
            self.fault = None
            raise RuntimeError("injected " + operation)
        self.record("native:" + operation)
        result = functions[operation](request["payload"])
        self.record("native:validated:" + operation)
        return result


class Adapter:
    def __init__(self, model):
        self.model = model

    def windows(self):
        return list(self.model.windows_map)

    def property(self, xid, name, property_type="UTF8_STRING"):
        return self.model.windows_map[xid].metadata.get(name, "")

    def title(self, xid):
        return self.property(xid, "_NET_WM_NAME")

    def wm_class(self, xid):
        return self.property(xid, "WM_CLASS").split("\0")

    def pid(self, xid):
        return 4242

    def active_window(self):
        return self.model.current_window.wid

    def set_text(self, xid, name, value, property_type="UTF8_STRING"):
        assert name == binding.ORDINARY_NONCE_PROPERTY
        self.model.windows_map[xid].metadata[name] = str(value)
        self.model.record("property:" + name, xid)

    def finish(self):
        self.model.record("roundtrip")


def main():
    model = Model()
    with mock.patch.object(binding, "XcffibAdapter", side_effect=lambda: model.adapter), \
            mock.patch.object(binding, "browser_process", return_value=True), \
            mock.patch.object(binding, "browser_incarnation",
                              side_effect=lambda pid: ["offline-boot", pid, "1234"]), \
            mock.patch.object(binding, "qtile_ipc_call", side_effect=lambda *args: model.ipc(*args)), \
            mock.patch.object(binding.time, "sleep", side_effect=AssertionError("unexpected poll")), \
            mock.patch.object(binding.socket, "socket", side_effect=AssertionError("live socket")), \
            mock.patch.object(binding.subprocess, "run", side_effect=AssertionError("live process")):
        for line in sys.stdin:
            message = json.loads(line)
            try:
                kind = message["kind"]
                if kind == "reset":
                    model = Model()
                    result = {}
                elif kind == "sync":
                    result = model.sync(message)
                elif kind == "fault":
                    model.fault = message["operation"]
                    result = {}
                elif kind == "state":
                    result = {"trace": model.trace}
                else:
                    result = model.request(message["request"])
                reply = {"id": message["id"], "ok": True, "payload": result}
            except Exception as error:
                reply = {"id": message["id"], "ok": False,
                         "error": f"{type(error).__name__}: {error}"}
            print(json.dumps(reply), flush=True)


if __name__ == "__main__":
    main()
