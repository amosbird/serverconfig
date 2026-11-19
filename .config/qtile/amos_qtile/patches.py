"""Small Qtile 0.37/X11 adapters, installed explicitly and never chained."""

from importlib.metadata import version

import xcffib.xproto
from libqtile.backend.x11 import core as x11_core
from libqtile.backend.x11.window import _Window
from libqtile.core.manager import Qtile
from libqtile.scratchpad import ScratchPad

from . import bindings, runtime, scratchpads


class ConditionalBorderColor(str):
    def __new__(cls, default, matches):
        color = super().__new__(cls, default)
        color.default, color.matches = default, matches
        return color

    def get_border_for_window(self, win):
        for rule, value in self.matches:
            if rule.compare(win):
                return value
        return self.default


class ConditionalBorderWidth(int):
    def __new__(cls, default, matches):
        border_width = super().__new__(cls, default)
        border_width.default, border_width.matches = default, matches
        return border_width

    def get_border_for_window(self, win):
        for rule, value in self.matches:
            if rule.compare(win):
                return value
        return self.default


def center_tray(win):
    if not scratchpads.matches(win.qtile, "stalonetray", win):
        return
    screen = (win.group.screen if win.group else None) or win.qtile.current_screen
    if screen is None:
        return
    x = int(screen.x + (screen.width - win.width) / 2)
    y = int(screen.y + (screen.height - win.height) / 2)
    if (win.x, win.y) != (x, y):
        win._place(x, y, win.width, win.height, win.borderwidth, win.bordercolor, above=True)


def new_place(self, x, y, width, height, borderwidth, bordercolor, above=False,
              margin=None, respect_hints=False):
    if hasattr(borderwidth, "get_border_for_window"):
        old = getattr(self, "_old_bw", borderwidth.default)
        if not isinstance(old, int):
            old = borderwidth.default
        newborder = borderwidth.get_border_for_window(self)
        width += (old - newborder) * 2
        height += (old - newborder) * 2
    else:
        newborder = borderwidth
    self._old_bw = newborder
    if hasattr(bordercolor, "get_border_for_window"):
        bordercolor = bordercolor.get_border_for_window(self)
    self._place(x, y, width, height, newborder, bordercolor, above=above,
                margin=margin, respect_hints=respect_hints)
    center_tray(self)


def process_screens(self, reloading=False):
    # Qtile can invent a group-less screen while the only output is waking up.
    live = [screen for screen in self.screens if hasattr(screen, "group")]
    if len(live) != len(self.screens):
        self.screens = live
        if hasattr(self, "current_screen") and not hasattr(self.current_screen, "group"):
            del self.current_screen
    return self._process_screens_unpatched(reloading)


def restore_state(self, state, restart):
    return scratchpads.restore(self, state, restart)


def reload_config(self):
    """Snapshot before import; evaluate and validate once, then use native rebuilding."""
    state = runtime.get_state(self)
    if state["reload_active"]:
        raise RuntimeError("Qtile config reload already in progress")
    state["reload_snapshot"] = scratchpads.snapshot(self)
    state["reload_active"] = True
    runtime.cancel_input(self)
    original_load = self.config.load
    had_override = "load" in vars(self.config)
    previous_override = vars(self.config).get("load")
    loaded = False

    def load_once():
        nonlocal loaded
        if not loaded:
            original_load()
            self.config.validate()
            loaded = True

    self.config.load = load_once
    try:
        Qtile._amos_reload_config(self)
    finally:
        if had_override:
            self.config.load = previous_override
        else:
            del self.config.load
        state["reload_active"] = False
        state.pop("candidate_declarations", None)
        state.pop("candidate_sources", None)
        runtime.finish_reload(self)


def install(manager):
    if version("qtile").split(".")[:2] != ["0", "37"]:
        raise RuntimeError("Qtile lifecycle adapter supports 0.37; run offline tests before upgrading")
    if not hasattr(Qtile, "_amos_reload_config"):
        Qtile._amos_reload_config = Qtile.reload_config
        Qtile.reload_config = reload_config
    commands = getattr(Qtile, "_commands", {}).copy()
    commands["reload_config"] = Qtile.reload_config
    Qtile._commands = commands
    if not hasattr(ScratchPad, "_amos_restore_state"):
        ScratchPad._amos_restore_state = ScratchPad.restore_state
        ScratchPad.restore_state = restore_state
    if manager.core.name == "x11":
        if not hasattr(_Window, "_place"):
            _Window._place = _Window.place
        _Window.place = new_place
        if not hasattr(Qtile, "_process_screens_unpatched"):
            Qtile._process_screens_unpatched = Qtile._process_screens
        Qtile._process_screens = process_screens
        x11_core._IGNORED_EVENTS.discard(xcffib.xproto.KeyReleaseEvent)
        x11_core.EVENT_TO_HANDLER[xcffib.xproto.KeyReleaseEvent] = "handle_KeyRelease"
        x11_core.Core.handle_KeyRelease = bindings.handle_key_release
