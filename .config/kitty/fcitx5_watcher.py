"""
fcitx5_watcher.py - kitty global watcher for fcitx5 IM context switching.

Uses D-Bus directly (no subprocess) for ~8x faster IM state operations.

Design: per-pane IM state tracking that works correctly across:
  - tmux pane switches (via OSC 1337 SetUserVar from fcitx5-tmux-hook)
  - kitty window switches within the same OS window (stack layout, via on_focus_change)
  - cross-app focus changes (Alt-Tab to/from kitty)
  - popup windows (copyq etc.) that briefly steal focus

Context ID strategy:
  - Base kitty window:  "kitty-<kitty_pid>-<window_id>"
  - Tmux pane:          "kitty-<kitty_pid>-<window_id>-tmux-<host>-<session>-<pane_id>"

Event handling:
  on_focus_change(focused=False):
      Save the active context's IM state.

  on_focus_change(focused=True):
      Restore the active context's IM state.  For a tmux window whose
      UserVar hasn't arrived yet, the base ctx is restored first; the
      tmux UserVar focus-in (arriving ~20-50ms later) overrides it.

  on_set_user_var "focus-in:<ctx>":
      If kitty window focused: save old ctx, update active, restore new ctx.
      If not focused: just update active ctx (defer restore to focus-in).

  on_set_user_var "focus-out:<ctx>":
      Save pane state only if kitty window is focused.

  on_close:
      Clean up in-memory tracking dicts.

No-saved-state policy:
  When restoring a context that has never been seen before (new tmux pane,
  first switch to a non-tmux window), always deactivate to English.  This
  prevents the previous context's IM state from leaking into the new one.

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

_active_ctx: dict[int, str] = {}
_window_focused: dict[int, bool] = {}


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
    global _dbus_controller
    _dbus_controller = None


def _query_im() -> tuple[int, str]:
    """Returns (state_int, im_name) or (0, '') on failure."""
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
# State file I/O
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
    state_file = os.path.join(STATE_DIR, ctx_id)
    try:
        with open(state_file, "r") as f:
            return _str_to_state(f.read().strip())
    except (FileNotFoundError, PermissionError):
        return (0, "")


def _restore_state(ctx_id: str) -> None:
    saved_state, saved_im = _read_saved_state(ctx_id)
    if saved_state == 0:
        # First time seeing this context (new tmux pane, or first switch to a
        # non-tmux window like emacs/dtach).  Default to English so the
        # previous context's IM state doesn't leak through.
        _log(f"restore ctx={ctx_id} no_saved_state action=deactivate")
        _apply_im(1, "keyboard-us")
        _save_state(ctx_id)
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
    was_focused = _window_focused.get(window.id, False)
    _window_focused[window.id] = bool(focused)

    if not focused:
        ctx = _get_active_ctx(window)
        _log(f"focus_out window={window.id} ctx={ctx}")
        _save_state(ctx)
        return

    if not was_focused:
        ctx = _get_active_ctx(window)
        _log(f"focus_in window={window.id} ctx={ctx}")
        _restore_state(ctx)


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

    if window.id not in _window_focused:
        try:
            _window_focused[window.id] = bool(window is boss.active_window)
        except Exception:
            _window_focused[window.id] = True

    is_focused = _window_focused.get(window.id, False)
    _log(f"user_var window={window.id} action={action} ctx={full_ctx} focused={is_focused}")

    if action == "focus-in":
        old_ctx = _get_active_ctx(window)
        if old_ctx != full_ctx:
            if is_focused:
                _save_state(old_ctx)
            _active_ctx[window.id] = full_ctx
        if is_focused:
            saved_state, saved_im = _read_saved_state(full_ctx)
            if saved_state != 0:
                _apply_im(saved_state, saved_im)
                _log(f"restore ctx={full_ctx} applied={_state_to_str(saved_state, saved_im)}")
            else:
                # New tmux pane (split/new-window): default to English so
                # the parent pane's IM state doesn't leak into the new pane.
                _log(f"restore ctx={full_ctx} no_saved_state action=deactivate")
                _apply_im(1, "keyboard-us")
                _save_state(full_ctx)
        else:
            _log(f"defer_restore ctx={full_ctx}")

    elif action == "focus-out":
        if not is_focused:
            _log(f"skip_save ctx={full_ctx} reason=window_not_focused")
            return
        _save_state(full_ctx)


def on_close(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    wid = window.id
    _log(f"close window={wid}")
    _active_ctx.pop(wid, None)
    _window_focused.pop(wid, None)
