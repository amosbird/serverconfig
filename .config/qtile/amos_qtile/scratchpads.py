"""Scratchpad actions and passive same-process restoration."""

from threading import Lock

from libqtile import hook, qtile
from libqtile.backend.base import Window as QtileWindow
from libqtile.config import Match
from libqtile.lazy import lazy
from libqtile.log_utils import logger
from libqtile.scratchpad import DropDownToggler

from . import runtime


class Shell:
    def __init__(self, window):
        self.scratchpad_name = "scratchpad"
        self.window = window

    def visible(self):
        return (self.window.group is not None
                and self.window.group.name != self.scratchpad_name
                and self.window.group is self.window.qtile.current_group
                and self.window.has_focus)

    def show_left(self):
        self.show_float(0, 0)

    def show_right(self):
        screen = self.window.qtile.current_screen
        if screen is not None and self.window.qtile.screens:
            self.show_float(int(screen.width / 2), 0)

    def toggle_left(self):
        if self.visible() and self.window.float_x == 0:
            self.hide()
        else:
            self.show_left()

    def toggle_right(self):
        screen = self.window.qtile.current_screen
        if screen is None or not self.window.qtile.screens:
            return
        if self.visible() and self.window.float_x == int(screen.width / 2):
            self.hide()
        else:
            self.show_right()

    def show_float(self, x, y):
        win = self.window
        screen = win.qtile.current_screen
        if screen is None or not win.qtile.screens:
            return
        win.togroup(win.qtile.current_group.name)
        win.opacity = 0.95
        win.enable_floating()
        win.place(int(screen.x + x), int(screen.y + y), int(screen.width / 2),
                  int(screen.height), win.qtile.current_group.floating_layout.border_width,
                  win.qtile.current_group.floating_layout.border_focus, above=True)
        win.bring_to_front()
        win.focus()

    def show_tiled(self):
        win = self.window
        win.qtile.groups_map["h"].toscreen()
        win.togroup("h")
        win.disable_floating()
        win.opacity = 1
        win.focus()

    def hide(self):
        self.window.togroup(self.scratchpad_name)


class ShellHolder:
    def __init__(self):
        self.shell = None
        self._spawned = None
        self.spawn_lock = Lock()

    def _show(self, mode):
        if mode == 2:
            self.shell.show_left()
        elif mode == 1:
            self.shell.show_right()
        else:
            self.shell.show_tiled()

    def _spawn(self, mode):
        with self.spawn_lock:
            if self.shell is not None or self._spawned is not None:
                return
            pid = qtile.spawn([
                "kitty", "-T", "urxvt_scratchpad", "-c",
                "/home/amos/.config/kitty/kitty-mux.conf", "--listen-on",
                "unix:/tmp/kitty-mux-socket",
            ], shell=True)
            self._spawned = (Match(net_wm_pid=pid), mode)

    def register(self, window):
        if self.shell is not None or window.name != "urxvt_scratchpad":
            return False
        if self._spawned is not None and not self._spawned[0].compare(window):
            return False
        mode = self._spawned[1] if self._spawned is not None else None
        self.shell = Shell(window)
        self._spawned = None
        if mode is not None:
            self._show(mode)
        return True

    def recover(self, windows):
        windows = list(windows)
        if self.shell is not None and self.shell.window in windows:
            return
        self.shell = None
        found = [window for window in windows
                 if isinstance(window, QtileWindow) and window.name == "urxvt_scratchpad"]
        if not found or self._spawned is not None:
            return
        self.shell = Shell(next((window for window in found if window.has_focus), found[0]))
        if len(found) > 1:
            logger.warning("Multiple scratchpad shells; preserving all %d windows", len(found))

    def on_client_killed(self, client):
        if self.shell is not None and self.shell.window is client:
            self.shell = None
            self._spawned = None

    def toggle_left(self):
        if self.shell:
            self.shell.toggle_left()
        else:
            self._spawn(2)

    def toggle_right(self):
        if self.shell:
            self.shell.toggle_right()
        else:
            self._spawn(1)

    def show_shell(self):
        if self.shell:
            self.shell.show_tiled()
        else:
            self._spawn(0)


def shell_holder(manager):
    state = runtime.get_state(manager)
    if state["shell"] is None:
        state["shell"] = ShellHolder()
    return state["shell"]


@lazy.function
def toggle_shell_left(manager):
    shell_holder(manager).toggle_left()


@lazy.function
def toggle_shell_right(manager):
    shell_holder(manager).toggle_right()


@lazy.function
def show_shell(manager):
    shell_holder(manager).show_shell()


def window_alive(manager, window):
    return window is not None and manager.windows_map.get(window.wid) is window


def consume_pending(scratchpad, name):
    removed = scratchpad._spawned.pop(name, None)
    if removed is not None and not scratchpad._spawned:
        hook.unsubscribe.client_new(scratchpad.on_client_new)


def drop_dropdown(scratchpad, name):
    dropdown = scratchpad.dropdowns.pop(name, None)
    if dropdown is not None:
        dropdown.unsubscribe()
    consume_pending(scratchpad, name)
    if not scratchpad.dropdowns:
        callbacks = hook.subscriptions.get("qtile", {})
        for event, callback in (("client_killed", scratchpad.on_client_killed),
                                ("float_change", scratchpad.on_float_change)):
            if callback in callbacks.get(event, []):
                getattr(hook.unsubscribe, event)(callback)


def _cleanup_hooks(scratchpad):
    hook.subscribe.client_killed(scratchpad.on_client_killed)
    hook.subscribe.float_change(scratchpad.on_float_change)


def attach_existing(scratchpad, name, window, previous=None):
    """Bind without window effects; the constructor is only for newly launched clients."""
    if not window_alive(scratchpad.qtile, window):
        raise ValueError("scratchpad window identity changed")
    if previous is not None and previous.window is not window:
        raise ValueError("scratchpad toggler belongs to another window object")
    config = scratchpad._dropdownconfig[name]
    dropdown = object.__new__(DropDownToggler)
    dropdown.name = name
    dropdown.x, dropdown.y = config.x, config.y
    dropdown.width, dropdown.height = config.width, config.height
    dropdown.geometry = (config.x, config.y, config.width, config.height)
    dropdown.border_width = window.qtile.config.floating_layout.border_width
    dropdown.scratchpad_name, dropdown.window = scratchpad.name, window
    dropdown.on_focus_lost_hide = config.on_focus_lost_hide
    dropdown.warp_pointer = config.warp_pointer
    dropdown.shown = window.group is not None and window.group.name != scratchpad.name
    dropdown._hooked = False
    if previous is not None:
        dropdown.geometry = previous.geometry
        dropdown.border_width = previous.border_width
        dropdown.shown = previous.shown
        dropdown.on_focus_lost_hide = previous.on_focus_lost_hide
        dropdown.warp_pointer = previous.warp_pointer
    scratchpad.dropdowns[name] = dropdown
    consume_pending(scratchpad, name)
    _cleanup_hooks(scratchpad)
    if not runtime.get_state(scratchpad.qtile)["reload_snapshot"]:
        arm_dropdown(dropdown)
    return dropdown


def arm_dropdown(dropdown):
    if dropdown.shown and dropdown.on_focus_lost_hide:
        dropdown.subscribe()


def matches(manager, name, window):
    scratchpad = manager.groups_map.get("scratchpad")
    config = scratchpad._dropdownconfig.get(name) if scratchpad is not None else None
    return config is not None and config.match is not None and config.match.compare(window)


def register_window(manager, window, hide=True):
    if not isinstance(window, QtileWindow):
        return
    scratchpad = manager.groups_map["scratchpad"]
    for name, dropdown in scratchpad.dropdowns.items():
        if dropdown.window is window:
            consume_pending(scratchpad, name)
            return
    for name, config in scratchpad._dropdownconfig.items():
        if config.match is None or not config.match.compare(window):
            continue
        old = scratchpad.dropdowns.get(name)
        if old is not None:
            if window_alive(manager, old.window):
                logger.warning("Scratchpad %s already bound; leaving window %s alone", name, window.wid)
                continue
            drop_dropdown(scratchpad, name)
        consume_pending(scratchpad, name)
        scratchpad.dropdowns[name] = DropDownToggler(window, scratchpad.name, config)
        _cleanup_hooks(scratchpad)
        if hide:
            scratchpad.dropdowns[name].hide()
        return


def recover_windows(manager):
    """Cold discovery is passive too: binding is not a request to show or hide."""
    shell_holder(manager).recover(manager.windows_map.values())
    scratchpad = manager.groups_map["scratchpad"]
    for name, config in scratchpad._dropdownconfig.items():
        if name in scratchpad.dropdowns or name in scratchpad._spawned or config.match is None:
            continue
        found = [window for window in manager.windows_map.values()
                 if isinstance(window, QtileWindow) and config.match.compare(window)]
        if len(found) == 1:
            attach_existing(scratchpad, name, found[0])
        elif len(found) > 1:
            logger.warning("Ambiguous scratchpad %s: %d windows; no automatic adoption", name, len(found))


exclusive_scratchpads = ("chatgpt", "webchat", "tdesktop")


def hide_exclusive(scratchpad, name):
    if name in exclusive_scratchpads:
        for other in exclusive_scratchpads:
            if other != name and other in scratchpad.dropdowns:
                scratchpad.dropdowns[other].hide()


def adopt_window(manager, name):
    scratchpad = manager.groups_map["scratchpad"]
    found = [window for window in manager.windows_map.values()
             if isinstance(window, QtileWindow) and matches(manager, name, window)]
    if len(found) > 1:
        raise ValueError(f"Ambiguous scratchpad {name}: select or close a duplicate explicitly")
    if not found:
        return False
    attach_existing(scratchpad, name, found[0])
    return True


def toggle_scratchpad(name):
    @lazy.function
    def toggle(manager):
        scratchpad = manager.groups_map["scratchpad"]
        if name in scratchpad.dropdowns and not window_alive(
                manager, scratchpad.dropdowns[name].window):
            drop_dropdown(scratchpad, name)
        if name not in scratchpad.dropdowns:
            if not adopt_window(manager, name):
                hide_exclusive(scratchpad, name)
                scratchpad.dropdown_toggle(name)
                return
        dropdown = scratchpad.dropdowns[name]
        if dropdown.window.has_focus:
            dropdown.hide()
            return
        hide_exclusive(scratchpad, name)
        current_group = manager.current_group
        dropdown.window.togroup(current_group.name)
        dropdown.show()
        if manager.current_group is not current_group:
            current_group.toscreen()
        dropdown.window.bring_to_front()
        dropdown.window.focus(warp=True)
    return toggle()


def show_scratchpad(manager, name):
    scratchpad = manager.groups_map["scratchpad"]
    if name in scratchpad.dropdowns:
        hide_exclusive(scratchpad, name)
        dropdown = scratchpad.dropdowns[name]
        dropdown.show()
        dropdown.window.bring_to_front()
        dropdown.window.focus(warp=True)


def snapshot(manager):
    result = {}
    for group in manager.groups:
        if not hasattr(group, "dropdowns"):
            continue
        result[group.name] = {
            "dropdowns": dict(group.dropdowns),
            "spawned": dict(group._spawned),
            "to_hide": list(group._to_hide),
        }
    return result


def restore(scratchpad, state, restart):
    from libqtile.scratchpad import ScratchPad

    if restart:
        return ScratchPad._amos_restore_state(scratchpad, state, restart)
    saved = runtime.get_state(scratchpad.qtile)["reload_snapshot"]
    saved = saved.get(scratchpad.name) if saved is not None else None
    if saved is None:
        # Also protect the first migration from an old config's native reload path.
        for name, wid, _visible in state:
            window = scratchpad.qtile.windows_map.get(wid)
            if name in scratchpad._dropdownconfig and isinstance(window, QtileWindow):
                attach_existing(scratchpad, name, window)
        return []
    for name, previous in saved["dropdowns"].items():
        if name not in scratchpad._dropdownconfig:
            logger.warning("Removed scratchpad %s: leaving its existing window untouched", name)
            continue
        if window_alive(scratchpad.qtile, previous.window):
            attach_existing(scratchpad, name, previous.window, previous)
    for name, match in saved["spawned"].items():
        if name in scratchpad._dropdownconfig and name not in scratchpad.dropdowns:
            scratchpad._spawned[name] = match
    scratchpad._to_hide = [name for name in saved["to_hide"] if name in scratchpad._spawned]
    if scratchpad._spawned:
        hook.subscribe.client_new(scratchpad.on_client_new)
    return []
