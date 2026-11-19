"""Small Qtile 0.37/X11 adapters, installed explicitly and never chained."""

from importlib.metadata import version
import copy
import sys

import xcffib.xproto
from libqtile import hook
from libqtile.backend.x11 import core as x11_core
from libqtile.backend.x11.window import Window, _Window
from libqtile.group import _Group
from libqtile.layout.floating import Floating
from libqtile.core.manager import Qtile
from libqtile.utils import remove_dbus_rules
from libqtile.scratchpad import ScratchPad

from . import bindings, runtime, scratchpads


__source_code__ = sys._getframe().f_code


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


def on_config_load(self, initial):
    manager = self.qtile
    state = runtime.get_state(manager)
    if initial or not getattr(manager, "_state", None) or "window_snapshot" not in state:
        return x11_core.Core._amos_on_config_load(self, initial)
    # Native X11 re-adds every client with focus and float-rule side effects.
    self.wmname = manager.config.wmname
    self.update_client_lists()
    self.conn.enable_screen_change_notifications()
    self.fire_screen_change(None)
    for wid, (window, group_name) in state["window_snapshot"].items():
        if manager.windows_map.get(wid) is not window or not isinstance(window, Window):
            continue
        group = manager.groups_map.get(group_name)
        if group is None:
            group = manager.current_group
        window.group = group
        if window not in group.windows:
            group.windows.append(window)
        if window.floating and not window.fullscreen:
            if window not in group.floating_layout.clients:
                group.floating_layout.add_client(window)
            saved_visibility = state["window_visibility"][wid]
            new_screen = runtime.screen_rect(group.screen)
            hidden = getattr(window, "hidden", saved_visibility["hidden"])
            if new_screen is None:
                if not hidden:
                    window.hide()
                state["preserve_floating"].add(wid)
            elif (new_screen == saved_visibility["screen"] and not hidden):
                state["preserve_floating"].add(wid)
            else:
                x, y, width, height = saved_visibility["geometry"]
                old_screen = saved_visibility["screen"]
                if old_screen is not None:
                    x += new_screen[0] - old_screen[0]
                    y += new_screen[1] - old_screen[1]
                window.x = max(group.screen.dx, min(x, group.screen.dx
                                                   + group.screen.dwidth - width))
                window.y = max(group.screen.dy, min(y, group.screen.dy
                                                   + group.screen.dheight - height))
                window.float_x = window.x - group.screen.x
                window.float_y = window.y - group.screen.y
        else:
            group.tiled_windows.add(window)
            if group.screen is None and not getattr(window, "hidden", False):
                window.hide()
    for group in manager.groups:
        saved = state["group_snapshot"].get(group.name, {"focus_history": [], "clients": {},
                                                        "layout": None, "monadtall": {}})
        group.focus_history = [window for window in saved["focus_history"]
                               if window in group.windows
                               and manager.windows_map.get(window.wid) is window]
        if saved.get("floating_focus") in group.floating_layout.clients:
            group.floating_layout.focused = saved["floating_focus"]
        for layout in group.layouts:
            ordered = [window for window in saved["clients"].get(layout.name, [])
                       if window in group.tiled_windows]
            ordered.extend(window for window in group.windows
                           if window in group.tiled_windows and window not in ordered)
            if hasattr(getattr(layout, "clients", None), "clients"):
                layout.clients.clients = ordered
                if group.current_window in ordered:
                    layout.clients.current_client = group.current_window
                monad = saved.get("monadtall", {}).get(layout.name)
                if monad is not None and type(layout).__name__ == "MonadTall":
                    layout.ratio, layout.align = monad["ratio"], monad["align"]
                    layout.auto_maximize = monad["auto_maximize"]
                    layout.relative_sizes = list(monad["relative_sizes"])
                    layout.do_normalize = monad["do_normalize"] or len(
                        layout.relative_sizes) != max(0, len(ordered) - 1)
            else:
                for window in ordered:
                    layout.add_client(window)
        if saved["layout"] is not None:
            for index, layout in enumerate(group.layouts):
                if layout.name == saved["layout"]:
                    group.current_layout = index
                    break


def focus_screen(self, index, warp=True):
    state = runtime.get_state(self)
    if "window_snapshot" in state and getattr(self, "_state", None):
        if index < len(self.screens):
            previous = self.current_screen
            self.current_screen = self.screens[index]
            if previous is not self.current_screen:
                hook.fire("current_screen_change")
                hook.fire("setgroup")
        return
    return Qtile._amos_focus_screen(self, index, warp)


def group_focus(self, *args, **kwargs):
    state = runtime.get_state(self.qtile)
    if "window_snapshot" in state and getattr(self.qtile, "_state", None):
        return
    return _Group._amos_focus(self, *args, **kwargs)


def floating_configure(self, window, screen_rect):
    manager = window.qtile
    state = runtime.get_state(manager)
    saved = state.get("window_snapshot", {}).get(window.wid)
    if (saved is not None and saved[0] is window
            and window.wid in state.get("preserve_floating", ())
            and getattr(manager, "_state", None)):
        # Membership was reattached without unmapping; native final layout may observe it.
        return
    return Floating._amos_configure(self, window, screen_rect)


def restore_state(self, state, restart):
    return scratchpads.restore(self, state, restart)


def reload_config(self):
    """Snapshot before import; evaluate and validate once, then use native rebuilding."""
    state = runtime.get_state(self)
    if state["reload_active"]:
        raise RuntimeError("Qtile config reload already in progress")
    recovery = state.get("reload_failure")
    if recovery is None:
        runtime.capture_reload(self)
    state["migration_snapshot"] = False
    state["reload_active"] = True
    self._state = None
    runtime.cancel_input(self)
    old_settings = vars(self.config).copy()
    original_load = self.config.load
    had_override = "load" in vars(self.config)
    previous_override = vars(self.config).get("load")
    loaded = False
    failed = False

    def load_once():
        nonlocal loaded
        if not loaded:
            try:
                original_load()
                self.config.validate()
            except Exception:
                vars(self.config).clear()
                vars(self.config).update(old_settings)
                self.config.load = load_once
                raise
            loaded = True
        else:
            # Native reload clears hooks between its two config.load calls.
            runtime.install_hooks(self)

    self.config.load = load_once
    try:
        if recovery is None:
            Qtile._amos_reload_config(self)
        else:
            # Rebuild fresh configurable objects, retaining the last coherent window snapshot.
            runtime.reconcile_recovery(self)
            load_once()
            self._finalize_configurables()
            hook.clear()
            self.ungrab_keys()
            self.chord_stack.clear()
            self.core.ungrab_buttons()
            self._mouse_map.clear()
            self.groups_map.clear()
            self.groups.clear()
            self.screens.clear()
            remove_dbus_rules()
            self._state = copy.copy(state["native_snapshot"])
            self._state.scratchpads = copy.deepcopy(state["native_snapshot"].scratchpads)
            runtime.install_hooks(self)
            self.load_config()
    except Exception as error:
        failed = True
        self._state = None
        state["reload_failure"] = str(error)
        state["failure_groups"] = list(self.groups)
        state["failure_restored_groups"] = [group for group in self.groups
                                             if getattr(group, "_amos_restored", False)]
        runtime.install_hooks(self)
        runtime.logger.exception("Qtile config rebuild failed; retry explicit full reload")
        runtime.send_notification("Qtile configuration failed",
                                  str(error) + "; retry explicit full reload")
        raise
    finally:
        if had_override:
            self.config.load = previous_override
        else:
            del self.config.load
        if not loaded:
            runtime.install_hooks(self)
        state["reload_active"] = False
        if not failed:
            state.pop("candidate_declarations", None)
            state.pop("candidate_sources", None)
            state.pop("reload_failure", None)
            runtime.finish_reload(self)


def check_compatible(manager):
    if version("qtile").split(".")[:2] != ["0", "37"]:
        raise RuntimeError(
            "Qtile lifecycle adapter supports 0.37; run offline tests before upgrading")
    if manager.core.name not in ("x11", "fixture"):
        raise RuntimeError("This configuration's reload preservation supports X11 only")


def install(manager):
    check_compatible(manager)
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
        if not hasattr(x11_core.Core, "_amos_on_config_load"):
            x11_core.Core._amos_on_config_load = x11_core.Core.on_config_load
            x11_core.Core.on_config_load = on_config_load
        if not hasattr(Qtile, "_amos_focus_screen"):
            Qtile._amos_focus_screen = Qtile.focus_screen
            Qtile.focus_screen = focus_screen
        if not hasattr(_Group, "_amos_focus"):
            _Group._amos_focus = _Group.focus
            _Group.focus = group_focus
        if not hasattr(Floating, "_amos_configure"):
            Floating._amos_configure = Floating.configure
            Floating.configure = floating_configure
