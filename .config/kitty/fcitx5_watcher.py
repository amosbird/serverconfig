"""
fcitx5_watcher.py - kitty global watcher for per-pane fcitx5 IM state.

Requires kitty built with native fcitx5 D-Bus client (GLFW_IM_MODULE=fcitx5),
which emits on_input_method_changed callbacks when the user toggles IM.

Design:
  - on_input_method_changed: records the current IM for the focused pane
  - on_focus_change(focused=False): saves current IM state (already known)
  - on_focus_change(focused=True): restores saved IM state via D-Bus
  - on_set_user_var: tracks tmux pane context switches
  - on_close: cleans up tracking dicts

Context ID strategy:
  - Base kitty window:  "kitty-<kitty_pid>-<window_id>"
  - Tmux pane:          "kitty-<kitty_pid>-<window_id>-tmux-<host>-<session>-<pane_id>"

No-saved-state policy:
  New contexts always deactivate to keyboard-us. This prevents the previous
  context's IM state from leaking into a new one.

Install: add to kitty.conf:
    watcher fcitx5_watcher.py
"""

import os
from datetime import datetime
from typing import Any

from kitty.boss import Boss
from kitty.window import Window

KITTY_PID = os.getpid()
LOG_FILE = "/tmp/fcitx5-im-debug.log"
LOG_ENABLE = "/tmp/fcitx5-im-debug.enabled"

_active_ctx: dict[int, str] = {}
_im_state: dict[str, str] = {}
_window_focused: dict[int, bool] = {}

_dbus_controller = None
_last_applied: str | None = None


def _log(msg: str) -> None:
    if not os.path.exists(LOG_ENABLE):
        return
    ts = datetime.now().strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
    try:
        with open(LOG_FILE, "a", encoding="utf-8") as f:
            f.write(f"{ts} [kitty-watcher pid={KITTY_PID}] {msg}\n")
    except Exception:
        pass


def _get_controller():
    global _dbus_controller
    if _dbus_controller is not None:
        return _dbus_controller
    try:
        import dbus

        bus = dbus.SessionBus()
        obj = bus.get_object("org.fcitx.Fcitx5", "/controller")
        _dbus_controller = dbus.Interface(obj, "org.fcitx.Fcitx.Controller1")
        return _dbus_controller
    except Exception as e:
        _log(f"dbus_connect FAILED: {e}")
        return None


def _reset_dbus() -> None:
    global _dbus_controller, _last_applied
    _dbus_controller = None
    _last_applied = None


def _apply_im(im_name: str) -> bool:
    global _last_applied
    if _last_applied == im_name:
        _log(f"apply im={im_name} (skip, already applied)")
        return True
    for attempt in range(2):
        ctrl = _get_controller()
        if ctrl is None:
            return False
        try:
            ctrl.SetCurrentIM(im_name)
            _last_applied = im_name
            _log(f"apply im={im_name}")
            return True
        except Exception as e:
            _log(f"apply im={im_name} FAILED (attempt {attempt}): {e}")
            _reset_dbus()
    return False


def _base_ctx(window: Window) -> str:
    return f"kitty-{KITTY_PID}-{window.id}"


def _get_active_ctx(window: Window) -> str:
    return _active_ctx.get(window.id, _base_ctx(window))



def on_input_method_changed(
    boss: Boss, window: Window, data: dict[str, Any]
) -> None:
    global _last_applied
    im_name = data.get("im_name", "")
    if not im_name:
        return
    ctx = _get_active_ctx(window)
    old = _im_state.get(ctx)
    if old == im_name:
        return
    _im_state[ctx] = im_name
    _last_applied = im_name
    _log(f"im_changed window={window.id} ctx={ctx} im={im_name}")


def on_focus_change(
    boss: Boss, window: Window, data: dict[str, Any]
) -> None:
    focused = data.get("focused", False)
    _window_focused[window.id] = bool(focused)

    ctx = _get_active_ctx(window)

    if not focused:
        _log(f"focus_out window={window.id} ctx={ctx} saved={_im_state.get(ctx, '(none)')}")
        return

    saved = _im_state.get(ctx)
    if saved is None:
        _log(f"focus_in window={window.id} ctx={ctx} no_state action=deactivate")
        _apply_im("keyboard-us")
        _im_state[ctx] = "keyboard-us"
    else:
        _log(f"focus_in window={window.id} ctx={ctx} restore={saved}")
        _apply_im(saved)


def on_set_user_var(
    boss: Boss, window: Window, data: dict[str, Any]
) -> None:
    key = data.get("key", "")
    value = data.get("value", "")

    if key != "fcitx5_ctx" or not value:
        return

    parts = value.split(":", 1)
    if len(parts) != 2:
        _log(f"bad_user_var window={window.id} value={value}")
        return

    action, tmux_ctx_suffix = parts
    full_ctx = f"{_base_ctx(window)}-{tmux_ctx_suffix}"

    if window.id not in _window_focused:
        try:
            _window_focused[window.id] = bool(window is boss.active_window)
        except Exception:
            _window_focused[window.id] = True

    is_focused = _window_focused.get(window.id, False)
    _log(
        f"user_var window={window.id} action={action} "
        f"ctx={full_ctx} focused={is_focused}"
    )

    if action == "focus-in":
        old_ctx = _get_active_ctx(window)
        if old_ctx != full_ctx:
            _active_ctx[window.id] = full_ctx
        if is_focused:
            saved = _im_state.get(full_ctx)
            if saved is None:
                upgrading_base = old_ctx == _base_ctx(window)
                if upgrading_base and old_ctx in _im_state:
                    saved = _im_state[old_ctx]
                    _im_state[full_ctx] = saved
                    _log(f"adopt ctx={full_ctx} from={old_ctx} im={saved}")
                else:
                    _log(f"new_ctx ctx={full_ctx} action=deactivate")
                    _apply_im("keyboard-us")
                    _im_state[full_ctx] = "keyboard-us"
                    return
            _log(f"restore ctx={full_ctx} im={saved}")
            _apply_im(saved)
        else:
            _log(f"defer_restore ctx={full_ctx}")

    elif action == "focus-out":
        pass

    elif action == "popup-open":
        popup_ctx = f"{full_ctx}-popup"
        _im_state.pop(popup_ctx, None)
        _im_state[popup_ctx] = "keyboard-us"
        _active_ctx[window.id] = popup_ctx
        if is_focused:
            _log(f"popup_open ctx={popup_ctx} action=deactivate")
            _apply_im("keyboard-us")
        else:
            _log(f"popup_open ctx={popup_ctx} deferred")

    elif action == "popup-close":
        popup_ctx = f"{full_ctx}-popup"
        _im_state.pop(popup_ctx, None)
        _active_ctx[window.id] = full_ctx
        if is_focused:
            saved = _im_state.get(full_ctx)
            if saved is None:
                _log(f"popup_close ctx={full_ctx} no_state action=deactivate")
                _apply_im("keyboard-us")
                _im_state[full_ctx] = "keyboard-us"
            else:
                _log(f"popup_close ctx={full_ctx} restore={saved}")
                _apply_im(saved)
        else:
            _log(f"popup_close ctx={full_ctx} deferred")


def on_close(
    boss: Boss, window: Window, data: dict[str, Any]
) -> None:
    wid = window.id
    _log(f"close window={wid}")
    prefix = _base_ctx(window)
    to_remove = [k for k in _im_state if k.startswith(prefix)]
    for k in to_remove:
        del _im_state[k]
    _active_ctx.pop(wid, None)
    _window_focused.pop(wid, None)
