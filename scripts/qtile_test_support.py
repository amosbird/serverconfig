#!/usr/bin/env python3
"""Offline Qtile lifecycle fixtures: real Python reducers, no backend connection."""

import abc
import contextlib
import copy
import importlib
import pathlib
import subprocess
import sys
from types import SimpleNamespace
from unittest import mock

import libqtile
import xcffib
from libqtile import hook
from libqtile.backend.base import Window
from libqtile.backend.x11 import core as x11_core
from libqtile.backend.x11 import window as x11_window
from libqtile.confreader import Config
from libqtile.core.manager import Qtile
from libqtile.group import _Group
from libqtile.layout.floating import Floating
from libqtile.scratchpad import DropDownToggler, ScratchPad, WindowVisibilityToggler

ROOT = pathlib.Path(__file__).resolve().parents[1]
CONFIG = ROOT / ".config/qtile/config.py"
NATIVE_RELOAD = Qtile.reload_config
NATIVE_RESTORE = ScratchPad.restore_state
NATIVE_GET_STATE = ScratchPad.get_state


def hook_counts():
    return {
        (registry, name): len(callbacks)
        for registry, events in hook.subscriptions.items()
        for name, callbacks in events.items() if callbacks
    }


class OfflineQtile(contextlib.AbstractContextManager):
    """Restore class patches, config modules and the actual global hook registry."""

    def __init__(self):
        self.stack = contextlib.ExitStack()

    def __enter__(self):
        self.modules = {
            name: module for name, module in sys.modules.items()
            if name == "config" or name == "amos_qtile" or name.startswith("amos_qtile.")
        }
        for name in self.modules:
            del sys.modules[name]
        self.path = sys.path[:]
        self.hooks = {
            registry: {name: callbacks[:] for name, callbacks in events.items()}
            for registry, events in hook.subscriptions.items()
        }
        self.ignored_events = x11_core._IGNORED_EVENTS.copy()
        self.event_handlers = x11_core.EVENT_TO_HANDLER.copy()
        self.classes = {
            cls: {name: copy.copy(value) if isinstance(value, (dict, set, list)) else value
                  for name, value in vars(cls).items()} for cls in (
                Qtile, ScratchPad, DropDownToggler, WindowVisibilityToggler,
                x11_window._Window, x11_window.Window, x11_core.Core, _Group, Floating,
            )
        }
        hook.clear()
        for target in (
            "subprocess.Popen", "subprocess.run", "subprocess.call",
            "subprocess.check_call", "subprocess.check_output", "os.system",
            "xcffib.connect",
            "libqtile.core.manager.remove_dbus_rules",
        ):
            self.stack.enter_context(mock.patch(target, side_effect=AssertionError(
                f"real external effect forbidden in offline fixture: {target}"
            )))
        # Removing DBus rules is a native reload effect, not a window effect.
        self.stack.enter_context(mock.patch("libqtile.core.manager.remove_dbus_rules"))
        self.stack.enter_context(mock.patch("libqtile.core.manager.send_notification"))
        self.manager = FakeManager()
        self.stack.enter_context(mock.patch.object(libqtile, "qtile", self.manager))
        return self

    def __exit__(self, *exc):
        try:
            self.stack.close()
        finally:
            for cls, original in self.classes.items():
                for name in set(vars(cls)) - set(original):
                    delattr(cls, name)
                for name, value in original.items():
                    if name not in {"__dict__", "__weakref__"}:
                        setattr(cls, name, value)
            x11_core._IGNORED_EVENTS.clear()
            x11_core._IGNORED_EVENTS.update(self.ignored_events)
            x11_core.EVENT_TO_HANDLER.clear()
            x11_core.EVENT_TO_HANDLER.update(self.event_handlers)
            hook.clear()
            hook.subscriptions.update(self.hooks)
            for name in list(sys.modules):
                if name == "config" or name == "amos_qtile" or name.startswith("amos_qtile."):
                    del sys.modules[name]
            sys.modules.update(self.modules)
            sys.path[:] = self.path
        return False

    def load(self):
        self.manager.config.load()
        return sys.modules["config"]

    def module(self, name):
        return importlib.import_module("amos_qtile." + name)


class FakeGroup:
    def __init__(self, name):
        self.name = name
        self.screen = None
        self.windows = []
        self.current_window = None
        self.layout = SimpleNamespace(name="fixture")
        self.label = name
        self.floating_layout = SimpleNamespace(border_width=8, border_focus="#ffffff")


class FakeScreen:
    def __init__(self, manager, index, group):
        self.manager = manager
        self.index = index
        self.x = self.dx = index * 1920
        self.y = self.dy = 0
        self.width = self.dwidth = 1920
        self.height = self.dheight = 1080
        self.top = None
        self.group = group
        group.screen = self

    def set_group(self, group):
        self.manager.trace.append(("native.screen.set_group", self.index, group.name))
        self.group.screen = None
        self.group = group
        group.screen = self


class FakeTimer:
    def __init__(self, callback, args):
        self.callback = callback
        self.args = args
        self.cancelled = False

    def cancel(self):
        self.cancelled = True

    def fire_even_if_cancelled(self):
        # A dequeued callback can still run after cancellation.
        return self.callback(*self.args)


class FakeManager(Qtile):
    """Use native reload_config with an explicit, limited load_config adapter.

    QtileState.apply and ScratchPad restoration remain native. Group/screen
    construction is simulated; native tiling layout topology is not preserved
    or claimed here. The rebuild and native focus_screen call remain in trace.
    """

    def __init__(self):
        self.trace = []
        self.timers = []
        self.windows_map = {}
        self.groups = []
        self.groups_map = {}
        self.screens = []
        self.current_screen = None
        self.chord_stack = []
        self._mouse_map = {}
        self._state = None
        self.no_spawn = True
        self.config = Config(str(CONFIG))
        self.core = SimpleNamespace(
            name="fixture", ungrab_buttons=lambda: self.trace.append(("native.ungrab_buttons",)),
        )
        self.rebuild()

    def rebuild(self):
        old_groups = {wid: window.group.name for wid, window in self.windows_map.items()
                      if window.group is not None}
        self.trace.append(("fixture.rebuild_groups_and_screens",))
        self.groups = [FakeGroup(name) for name in ("h", "2", "f", "other")]
        self.groups_map = {group.name: group for group in self.groups}
        self.screens = [FakeScreen(self, index, self.groups[index]) for index in range(2)]
        self.current_screen = self.screens[0]
        for definition in self.config.groups:
            if isinstance(definition, libqtile.config.ScratchPad):
                scratchpad = ScratchPad(
                    definition.name, definition.dropdowns, definition.label, definition.single
                )
                # No backend/layout configuration; these are the fields used by restoration.
                scratchpad.qtile = self
                scratchpad.screen = None
                scratchpad.windows = []
                scratchpad.focus_history = []
                self.groups.append(scratchpad)
                self.groups_map[scratchpad.name] = scratchpad
        if "scratchpad" not in self.groups_map:
            scratchpad = ScratchPad("scratchpad")
            scratchpad.qtile = self
            scratchpad.screen = None
            scratchpad.windows = []
            scratchpad.focus_history = []
            self.groups.append(scratchpad)
            self.groups_map[scratchpad.name] = scratchpad
        for wid, name in old_groups.items():
            window = self.windows_map[wid]
            window.group = self.groups_map[name]
            window.group.windows.append(window)
            if window.has_focus:
                window.group.current_window = window

    def load_config(self, initial=False):
        self.trace.append(("native.load_config.adapter",))
        self.config.load()
        self.rebuild()
        if self._state is not None:
            self._state.apply(self)
        hook.fire("startup")
        self._state = None

    def _finalize_configurables(self):
        self.trace.append(("native.finalize",))

    def ungrab_keys(self):
        self.trace.append(("native.ungrab_keys",))

    def focus_screen(self, index, *args, **kwargs):
        self.trace.append(("native.focus_screen", index))
        self.current_screen = self.screens[index]

    def spawn(self, command, *args, **kwargs):
        self.trace.append(("spawn", command))
        return 424242

    def call_later(self, delay, callback, *args):
        timer = FakeTimer(callback, args)
        self.timers.append(timer)
        self.trace.append(("timer", delay))
        return timer

    def update_desktops(self, *args):
        self.trace.append(("native.update_desktops",))


class FakeWindow(Window):
    _visual_fields = {
        "x", "y", "width", "height", "float_x", "float_y", "opacity",
        "floating", "_float_state", "skip_taskbar",
    }

    def __setattr__(self, name, value):
        if name in self._visual_fields and self.__dict__.get("_record", False):
            self.effect("setattr", name, value)
        super().__setattr__(name, value)

    def __init__(self, manager, wid=101, group="h", name="fixture", focused=False):
        self.qtile = manager
        self._wid = wid
        self._group = manager.groups_map[group]
        self.name = name
        self._focused = focused
        self.floating = True
        self._float_state = None
        self.fullscreen = False
        self.maximized = False
        self.minimized = False
        self.hidden = self._group.screen is None
        self.x, self.y, self.width, self.height = 111, 73, 777, 555
        self.float_x, self.float_y = self.x, self.y
        self.borderwidth = 8
        self.bordercolor = "#ffffff"
        self.opacity = 0.87
        self.skip_taskbar = False
        self.window = SimpleNamespace(get_name=lambda: self.name)
        self._group.windows.append(self)
        if focused:
            self._group.current_window = self
        manager.windows_map[wid] = self
        self._record = True

    minimized = False

    @property
    def floating(self):
        return self._floating

    @floating.setter
    def floating(self, value):
        self._floating = value

    @property
    def wid(self):
        return self._wid

    @property
    def group(self):
        return self._group

    @group.setter
    def group(self, value):
        self._group = value

    @property
    def has_focus(self):
        return self._focused

    def effect(self, name, *args):
        self.qtile.trace.append(("window." + name, self.wid, *args, self.snapshot()))

    def snapshot(self):
        return (self.group.name if self.group else None, self.x, self.y,
                self.width, self.height, self.opacity, self.has_focus)

    def togroup(self, group_name=None, **kwargs):
        self.effect("togroup", group_name)
        if self.group and self in self.group.windows:
            self.group.windows.remove(self)
        self.group = self.qtile.groups_map[group_name or self.qtile.current_group.name]
        self.group.windows.append(self)

    def place(self, x, y, width, height, *args, **kwargs):
        self.effect("place", x, y, width, height)
        self.x, self.y, self.width, self.height = x, y, width, height

    def focus(self, *args, **kwargs):
        self.effect("focus")
        self._focused = True
        self.group.current_window = self

    def bring_to_front(self):
        self.effect("bring_to_front")

    def hide(self):
        self.effect("hide")
        self.hidden = True

    def unhide(self):
        self.effect("unhide")
        self.hidden = False

    def kill(self):
        self.effect("kill")

    def enable_floating(self):
        self.effect("enable_floating")
        self.floating = True

    def disable_floating(self):
        self.effect("disable_floating")
        self.floating = False

    def get_pid(self):
        return 424242

    def get_wm_class(self):
        return ("fixture", "fixture")

    def get_wm_role(self):
        return ""

    def get_wm_type(self):
        return "normal"

    def match(self, rule):
        return rule.compare(self)

    def info(self):
        return {"id": self.wid, "name": self.name}


# Unused backend operations must fail rather than opening a real backend.
for _name in FakeWindow.__abstractmethods__:
    def _unsupported(self, *args, _operation=_name, **kwargs):
        raise AssertionError(f"unsupported fixture window operation: {_operation}")
    setattr(FakeWindow, _name, _unsupported)
abc.update_abstractmethods(FakeWindow)


def window_effects(manager):
    return [event for event in manager.trace if event[0].startswith("window.")]
