"""Manager-owned state and explicit, repeatable configuration installation."""

from pathlib import Path
import subprocess
import sys

from libqtile import hook, qtile
from libqtile.backend.base import Window as QtileWindow
from libqtile.command.base import expose_command
from libqtile.core.manager import Qtile
from libqtile.core.state import QtileState
from libqtile.log_utils import logger
from libqtile.utils import send_notification

from . import bindings, browser, declarations, desktop, patches, scratchpads


__source_code__ = sys._getframe().f_code
SCHEMA = 1


def get_state(manager):
    state = getattr(manager, "_amos_runtime", None)
    if state is None:
        state = {
            "schema": SCHEMA, "shell": None, "previous_focused": [], "hooks": [],
            "input_generation": 0, "input_timer": None, "pending_inputstr": None,
            "super_r_keycodes": [], "reload_snapshot": None, "reload_active": False,
            "declarations": None, "source_digest": {}, "installed": False,
        }
        manager._amos_runtime = state
    if not isinstance(state, dict) or state.get("schema") != SCHEMA:
        raise RuntimeError("Unsupported Qtile runtime state; explicit restart migration required")
    return state


def screen_rect(screen):
    if screen is None:
        return None
    return (screen.x, screen.y, screen.width, screen.height)


def screen_work_area(screen):
    if screen is None:
        return None
    return (screen.dx, screen.dy, screen.dwidth, screen.dheight)


def capture_reload(manager):
    state = get_state(manager)
    state["initializing"] = False
    state["native_snapshot"] = QtileState(manager, restart=False)
    state["reload_snapshot"] = scratchpads.snapshot(manager)
    state["window_snapshot"] = {
        wid: (window, window.group.name if window.group is not None else None)
        for wid, window in manager.windows_map.items() if isinstance(window, QtileWindow)
    }
    state["window_visibility"] = {
        wid: {"screen": screen_rect(window.group.screen) if window.group is not None else None,
              "work_area": (screen_work_area(window.group.screen)
                            if window.group is not None else None),
              "hidden": getattr(window, "hidden", window.group is None
                                or window.group.screen is None),
              "geometry": (window.x, window.y, window.width, window.height)}
        for wid, (window, _group) in state["window_snapshot"].items()
    }
    state["preserve_floating"] = set()
    state["group_snapshot"] = {
        group.name: {
            "focus_history": list(getattr(group, "focus_history", [])),
            "floating_focus": getattr(getattr(group, "floating_layout", None), "focused", None),
            "layout": group.layout.name if getattr(group, "layouts", None) else None,
            "clients": {
                layout.name: list(layout.clients.clients)
                for layout in getattr(group, "layouts", [])
                if hasattr(getattr(layout, "clients", None), "clients")
            },
            "layout_focus": {
                layout.name: layout.clients.current_client
                for layout in getattr(group, "layouts", [])
                if hasattr(getattr(layout, "clients", None), "current_client")
            },
            "monadtall": {
                layout.name: {
                    "ratio": layout.ratio, "align": layout.align,
                    "relative_sizes": list(layout.relative_sizes),
                    "do_normalize": getattr(layout, "do_normalize", True),
                    "auto_maximize": layout.auto_maximize,
                } for layout in getattr(group, "layouts", [])
                if type(layout).__module__ == "libqtile.layout.xmonad"
                and type(layout).__name__ == "MonadTall"
            },
        } for group in manager.groups
    }


def reconcile_recovery(manager):
    """Merge accepted failure-frame arrivals; never replay a consumed launch waiter."""
    state = get_state(manager)
    windows = {wid: pair for wid, pair in state["window_snapshot"].items()
               if manager.windows_map.get(wid) is pair[0]}
    for wid, window in manager.windows_map.items():
        if isinstance(window, QtileWindow) and wid not in windows:
            windows[wid] = (window, window.group.name if window.group is not None else None)
            state["window_visibility"][wid] = {
                "screen": screen_rect(window.group.screen) if window.group is not None else None,
                "work_area": (screen_work_area(window.group.screen)
                              if window.group is not None else None),
                "hidden": getattr(window, "hidden", True),
                "geometry": (window.x, window.y, window.width, window.height),
            }
    state["window_snapshot"] = windows
    for group in manager.groups:
        saved = state["reload_snapshot"].get(group.name)
        failed_groups = state.get("failure_groups", [])
        if (saved is None or not hasattr(group, "dropdowns")
                or not any(group is failed for failed in failed_groups)):
            continue
        for name, dropdown in group.dropdowns.items():
            if scratchpads.window_alive(manager, dropdown.window):
                saved["dropdowns"][name] = dropdown
                saved["spawned"].pop(name, None)
        if any(group is restored for restored in state.get("failure_restored_groups", [])):
            for name in list(saved["spawned"]):
                if name not in group._spawned and name not in group.dropdowns:
                    saved["spawned"].pop(name)
            saved["to_hide"] = list(group._to_hide)
        saved["spawned"].update(group._spawned)


def cancel_input(manager):
    state = get_state(manager)
    state["input_generation"] += 1
    timer, state["input_timer"] = state["input_timer"], None
    if timer is not None:
        timer.cancel()
    state["pending_inputstr"] = None


def _subscribe(manager, event, callback):
    getattr(hook.subscribe, event)(callback)
    get_state(manager)["hooks"].append((event, callback))


def _unsubscribe_owned(manager):
    state = get_state(manager)
    for event, callback in state["hooks"]:
        if callback in hook.subscriptions.get("qtile", {}).get(event, []):
            getattr(hook.unsubscribe, event)(callback)
    state["hooks"].clear()


def config_plan(manager):
    return declarations.plan(manager)


def config_apply(manager, expected_digest: str):
    result = declarations.apply(manager, expected_digest)
    if not result["ok"]:
        logger.error("Qtile config refresh rejected: %s", "; ".join(result["errors"]))
    return result


def refresh_config(manager):
    plan = config_plan(manager)
    result = config_apply(manager, plan["source_digest"]) if plan["ok"] else plan
    if result["ok"]:
        message = ("Scratchpad launch commands/matchers updated; existing windows unchanged"
                   if result.get("changed") else "No supported declaration changes")
    else:
        message = "; ".join(result["errors"])
        if result["requires_full_reload"]:
            message += "; use Super+Shift+0 for explicit full reload"
        logger.error("Qtile config refresh rejected: %s", message)
    send_notification("Qtile configuration", message)
    return result


def _install_commands():
    commands = getattr(Qtile, "_commands", {}).copy()
    for name, function in (("config_plan", config_plan), ("config_apply", config_apply)):
        setattr(Qtile, name, expose_command()(function))
        commands[name] = function
    Qtile._commands = commands
    browser.install_browser_commands()


def install(manager, snapshot, legacy_shell=None, entry=None):
    if not hasattr(manager, "config"):
        return
    state = get_state(manager)
    sources = declarations.capture_running_sources(Path(manager.config.file_path).parent, entry)
    if state.get("reload_active") and getattr(manager, "_state", None):
        raise RuntimeError("Refusing config evaluation during an active rebuild")
    patches.check_compatible(manager)
    if not state["installed"] and legacy_shell is not None:
        state["shell"] = legacy_shell
    if (not state["reload_active"] and not getattr(manager, "_state", None)
            and getattr(manager, "no_spawn", False)
            and manager.groups_map.get("scratchpad") is not None):
        # Existing signal handlers may still hold the old native reload function.
        capture_reload(manager)
        state["migration_snapshot"] = True
    scratchpads.upgrade_shell(manager)
    patches.install(manager)
    _install_commands()
    # Preflight evaluation must not commit new defaults over the running config.
    if state["installed"]:
        state["candidate_declarations"] = snapshot
        state["candidate_sources"] = sources
    else:
        state["declarations"], state["source_digest"] = snapshot, sources
    state["installed"] = True
    if not getattr(manager, "no_spawn", True) or getattr(manager, "_state", None):
        state["initializing"] = True
    install_hooks(manager)
    manager.show_scratchpad = lambda name: scratchpads.show_scratchpad(manager, name)


def install_hooks(manager):
    _unsubscribe_owned(manager)
    for event, callback in (
        ("startup", on_startup), ("startup_complete", on_startup_complete),
        ("startup_once", startup_once), ("setgroup", desktop.show_browser_tabs),
        ("client_new", before_window_created), ("client_managed", after_window_created),
        ("client_killed", window_killed), ("client_focus", client_focused),
    ):
        _subscribe(manager, event, callback)


def on_startup():
    manager = qtile
    state = get_state(manager)
    if "candidate_declarations" in state:
        state["declarations"] = state.pop("candidate_declarations")
        state["source_digest"] = state.pop("candidate_sources")
    bindings.enable_key_release_events(manager)
    desktop.initialize_browser_tabs(manager)
    state["initializing"] = False
    if state["reload_snapshot"] is None:
        scratchpads.recover_windows(manager)
    # Delay focus-lost subscriptions until native state/core reconstruction has finished.
    if not state["reload_active"]:
        finish_reload(manager)


def on_startup_complete():
    bindings.enable_key_release_events(qtile)


def finish_reload(manager):
    state = get_state(manager)
    state["reload_snapshot"] = None
    migrating = state.pop("migration_snapshot", False)
    if not migrating:
        state.pop("window_snapshot", None)
        state.pop("group_snapshot", None)
        state.pop("window_visibility", None)
        state.pop("preserve_floating", None)
        state.pop("native_snapshot", None)
        state.pop("failure_groups", None)
        state.pop("failure_restored_groups", None)
    for group in manager.groups:
        for dropdown in getattr(group, "dropdowns", {}).values():
            scratchpads.arm_dropdown(dropdown)
    holder = scratchpads.shell_holder(manager)
    holder.recover(manager.windows_map.values())


def startup_once():
    subprocess.Popen("startup")


def before_window_created(client):
    classes = client.get_wm_class() or ()
    if "copyq" in classes:
        client.enable_floating()
        client.set_size_floating(2000, 1200)
        client.center()
    elif "kitty" in classes and client.window.get_name() == "float":
        client.enable_floating()
        client.set_size_floating(2000, 1200)
        client.center()
    elif "kitty" in classes and client.window.get_name() == "dtpick":
        client.enable_floating()
        client.set_size_floating(400, 120)
        client.center()
    elif "xfreerdp" in classes:
        client.focus()
        client.togroup("w", switch_group=True)


def after_window_created(client):
    manager = client.qtile
    if scratchpads.shell_holder(manager).register(client):
        return
    bookmarks = scratchpads.matches(manager, "bookmarks", client)
    if get_state(manager).get("initializing"):
        scratchpad = manager.groups_map["scratchpad"]
        for name in scratchpad._dropdownconfig:
            if scratchpads.matches(manager, name, client):
                scratchpads.attach_existing(scratchpad, name, client)
                return
    scratchpads.register_window(manager, client, hide=not bookmarks)
    if bookmarks:
        return
    classes = client.get_wm_class() or ()
    if "Google-chrome" in classes and client.get_wm_role() == "pop-up":
        screen = manager.current_screen
        client.enable_floating()
        client.set_size_floating(int(screen.width * 0.7), int(screen.height * 0.8))
        client.set_position_floating(int(screen.x + screen.width * 0.15),
                                     int(screen.y + screen.height * 0.1))
    elif "chatgpt" in classes:
        client.keep_above()


def window_killed(client):
    manager = client.qtile
    scratchpads.shell_holder(manager).on_client_killed(client)
    state = get_state(manager)
    state["previous_focused"] = [
        window for window in state["previous_focused"] if window is not client]
    scratchpad = manager.groups_map.get("scratchpad")
    if scratchpad is not None:
        for name, dropdown in list(scratchpad.dropdowns.items()):
            if dropdown.window is client:
                scratchpads.drop_dropdown(scratchpad, name)
                break


def client_focused(window):
    if not isinstance(window, QtileWindow):
        return
    if window.name == "urxvt_scratchpad":
        window.border_width = 0
    history = get_state(window.qtile)["previous_focused"]
    if not history or history[-1] is not window:
        history.append(window)
        del history[:-2]
