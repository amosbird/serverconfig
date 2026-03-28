"""
fcitx5_watcher.py - kitty global watcher for fcitx5 IM context switching.

Uses D-Bus directly (no subprocess) for ~8x faster IM state operations.

Design: "save-on-switch for tmux, save-on-focus-out for base kitty,
          no restore on focus-in (fcitx5 handles cross-app focus natively)"

Context ID strategy:
  - Base kitty window:  "kitty-<kitty_pid>-<window_id>"
  - Tmux pane:          "kitty-<kitty_pid>-<window_id>-tmux-<host>-<session>-<pane_id>"
    (constructed by fcitx5-tmux-hook, delivered via OSC 1337 SetUserVar)

Event handling:
  on_focus_change(focused=False):
      Save only non-tmux (base kitty) contexts. Tmux contexts are saved by
      tmux hooks via on_set_user_var, with a window-focused guard to prevent
      corruption from transient popups (copyq etc.).

  on_focus_change(focused=True):
      No-op. fcitx5 handles cross-app focus restoration natively. Kitty-internal
      context switching (tmux panes) is handled by on_set_user_var.

  on_set_user_var "focus-in:<ctx>":
      Save-on-switch: save the OLD context first, then restore the NEW one.

  on_set_user_var "focus-out:<ctx>":
      Save the outgoing pane's state, but ONLY if the kitty window is still
      focused (guards against late events from cross-app focus loss).

  on_close:
      Clean up in-memory tracking dicts.

Install: add to kitty.conf:
    watcher fcitx5_watcher.py
"""

import os
from datetime import datetime
from typing import Any

from kitty.boss import Boss
from kitty.window import Window

KITTY_PID = os.getpid()
STATE_DIR = f"/tmp/fcitx5-ctx-{os.getuid()}"
LOG_FILE = "/tmp/fcitx5-im-debug.log"
LOG_ENABLE = "/tmp/fcitx5-im-debug.enabled"  # touch to enable, rm to disable

# Per kitty-window tracking (keyed by window.id):
_active_ctx: dict[int, str] = {}  # active context ID (base or tmux)
_window_focused: dict[int, bool] = {}  # OS-level focus state
_last_tmux_ctx: dict[int, str] = {}  # most recent tmux ctx (for focus-in race)


# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------


def _log(msg: str) -> None:
    if not os.path.exists(LOG_ENABLE):
        return
    ts = datetime.now().strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
    try:
        with open(LOG_FILE, "a", encoding="utf-8") as f:
            f.write(f"{ts} [kitty-watcher pid={KITTY_PID}] {msg}\n")
    except Exception:
        pass


# ---------------------------------------------------------------------------
# D-Bus interface to fcitx5  (lazy-init, reconnect on failure)
# ---------------------------------------------------------------------------

_dbus_controller = None


def _get_controller():
    """Get (or reconnect) the fcitx5 D-Bus controller proxy."""
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


def _reset_dbus():
    """Force reconnect on next call (e.g. after fcitx5 restart)."""
    global _dbus_controller
    _dbus_controller = None


def _query_im() -> tuple[int, str]:
    """Query current fcitx5 state. Returns (state_int, im_name) or (0, '') on failure.

    state: 0=closed, 1=inactive, 2=active
    im_name: e.g. 'keyboard-us', 'rime'
    """
    ctrl = _get_controller()
    if ctrl is None:
        return (0, "")
    try:
        state = int(ctrl.State())
        im_name = str(ctrl.CurrentInputMethod())
        return (state, im_name)
    except Exception as e:
        _log(f"dbus_query FAILED: {e}")
        _reset_dbus()
        return (0, "")


def _apply_im(state: int, im_name: str) -> bool:
    """Apply an IM state. Returns True on success."""
    ctrl = _get_controller()
    if ctrl is None:
        return False
    try:
        if state == 1:
            ctrl.Deactivate()
        elif state == 2:
            if im_name:
                ctrl.SetCurrentIM(im_name)
            else:
                ctrl.Activate()
        else:
            return False
        return True
    except Exception as e:
        _log(f"dbus_apply state={state} im={im_name} FAILED: {e}")
        _reset_dbus()
        return False


# ---------------------------------------------------------------------------
# State file I/O  (format: "state:im_name", e.g. "1:keyboard-us", "2:rime")
# ---------------------------------------------------------------------------


def _state_to_str(state: int, im_name: str) -> str:
    return f"{state}:{im_name}"


def _str_to_state(s: str) -> tuple[int, str]:
    parts = s.split(":", 1)
    if len(parts) != 2:
        return (0, "")
    try:
        return (int(parts[0]), parts[1])
    except ValueError:
        return (0, "")


def _save_state(ctx_id: str) -> None:
    """Save current fcitx5 IM state to file for ctx_id."""
    state, im_name = _query_im()
    if state == 0:
        _log(f"save ctx={ctx_id} skip=query_failed")
        return
    os.makedirs(STATE_DIR, exist_ok=True)
    state_str = _state_to_str(state, im_name)
    state_file = os.path.join(STATE_DIR, ctx_id)
    tmp_file = f"{state_file}.tmp.{os.getpid()}"
    try:
        with open(tmp_file, "w") as f:
            f.write(state_str + "\n")
        os.replace(tmp_file, state_file)
        _log(f"save ctx={ctx_id} state={state_str}")
    except Exception as e:
        _log(f"save ctx={ctx_id} FAILED: {e}")
        try:
            os.unlink(tmp_file)
        except OSError:
            pass


def _read_saved_state(ctx_id: str) -> tuple[int, str]:
    """Read saved state for ctx_id. Returns (0, '') if missing/invalid."""
    state_file = os.path.join(STATE_DIR, ctx_id)
    try:
        with open(state_file, "r") as f:
            return _str_to_state(f.read().strip())
    except (FileNotFoundError, PermissionError):
        return (0, "")


def _restore_state(ctx_id: str) -> None:
    """Restore IM state for ctx_id. Defaults to inactive/English if no saved state."""
    saved_state, saved_im = _read_saved_state(ctx_id)
    if saved_state == 0:
        # No saved state: default to inactive (English).
        _log(f"restore ctx={ctx_id} no_saved_state action=deactivate")
        _apply_im(1, "keyboard-us")
        return

    if _apply_im(saved_state, saved_im):
        _log(f"restore ctx={ctx_id} applied={_state_to_str(saved_state, saved_im)}")
    else:
        _log(
            f"restore ctx={ctx_id} apply_failed "
            f"saved={_state_to_str(saved_state, saved_im)}"
        )


# ---------------------------------------------------------------------------
# Context helpers
# ---------------------------------------------------------------------------


def _base_ctx(window: Window) -> str:
    return f"kitty-{KITTY_PID}-{window.id}"


def _get_active_ctx(window: Window) -> str:
    return _active_ctx.get(window.id, _base_ctx(window))


# ---------------------------------------------------------------------------
# Event handlers
# ---------------------------------------------------------------------------


def on_focus_change(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    focused = data.get("focused", False)
    _window_focused[window.id] = bool(focused)

    if not focused:
        # Save non-tmux contexts on focus-out (for kitty split/tab switches).
        # Tmux contexts are saved by tmux hooks with a window-focused guard.
        current_ctx = _get_active_ctx(window)
        if "-tmux-" in current_ctx:
            _log(
                f"focus_out window={window.id} ctx={current_ctx} "
                "action=skip_delegate_tmux"
            )
            return
        _log(f"focus_out window={window.id} ctx={current_ctx}")
        _save_state(current_ctx)
        return

    # Focus-in: do NOT restore. fcitx5 handles cross-app focus natively.
    # Kitty-internal context switching (tmux panes) is handled by
    # on_set_user_var focus-in, which arrives shortly after this event.
    _log(f"focus_in window={window.id} action=none")


def on_set_user_var(boss: Boss, window: Window, data: dict[str, Any]) -> None:
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

    _last_tmux_ctx[window.id] = full_ctx

    # Initialize focus tracking if first user_var before any focus event.
    if window.id not in _window_focused:
        try:
            _window_focused[window.id] = bool(window is boss.active_window)
        except Exception:
            _window_focused[window.id] = True

    _log(f"user_var window={window.id} action={action} ctx={full_ctx}")

    if action == "focus-in":
        # Save-on-switch: save OLD context, then restore NEW.
        old_ctx = _get_active_ctx(window)
        if old_ctx != full_ctx:
            _save_state(old_ctx)
        _restore_state(full_ctx)
        _active_ctx[window.id] = full_ctx

    elif action == "focus-out":
        # Only save if the kitty window still has focus.
        if not _window_focused.get(window.id, False):
            _log(
                f"skip_save window={window.id} ctx={full_ctx} reason=window_not_focused"
            )
            return
        _save_state(full_ctx)
        _active_ctx[window.id] = _base_ctx(window)


def on_close(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    wid = window.id
    _log(f"close window={wid}")
    _active_ctx.pop(wid, None)
    _window_focused.pop(wid, None)
    _last_tmux_ctx.pop(wid, None)
