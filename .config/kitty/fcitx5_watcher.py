"""
fcitx5_watcher.py - kitty global watcher for fcitx5 IM context switching.

Context ID strategy (shell-agnostic, no shell config needed):
  - kitty focus:  "kitty-<kitty_pid>-<window_id>"
    kitty_pid makes it unique across kitty instances;
    window_id is stable within a kitty process lifetime.
  - remote tmux:  constructed by fcitx5-tmux-hook from tmux session + pane info,
    delivered via OSC 1337 SetUserVar=fcitx5_ctx=<action>:<ctx_id>

Events:
  on_focus_change  - kitty window focus changed
  on_set_user_var  - remote tmux pane switch signal (key="fcitx5_ctx")
  on_close         - cleanup

Install: add to kitty.conf:
    watcher fcitx5_watcher.py
"""

import subprocess
import os
import sys
from typing import Any

from kitty.boss import Boss
from kitty.window import Window

CONTEXT_CMD = os.path.expanduser("~/scripts/fcitx5-context")
KITTY_PID = os.getpid()

_last_focused_ctx: dict[int, str] = {}


def _ctx_for_window(window: Window) -> str:
    return f"kitty-{KITTY_PID}-{window.id}"


def _get_active_ctx(window: Window) -> str:
    """Get the currently active context for a window (base or tmux)."""
    return _last_focused_ctx.get(window.id, _ctx_for_window(window))


def _set_active_ctx(window: Window, ctx: str) -> None:
    _last_focused_ctx[window.id] = ctx


def _fcitx5_context(action: str, ctx_id: str) -> None:
    """Run fcitx5-context synchronously to ensure strict ordering."""
    try:
        subprocess.run(
            [CONTEXT_CMD, action, ctx_id],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            timeout=1,
        )
    except Exception:
        pass


def on_focus_change(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    focused = data.get("focused", False)

    # When losing focus, save the current state of *this* window
    if not focused:
        current_ctx = _get_active_ctx(window)
        _fcitx5_context("save", current_ctx)
        return

    # When gaining focus:
    # 1. We might have switched FROM another window. Save that one's state first.
    #    (But we don't know which one it was easily here, relying on its own focus-out event is safer
    #     but kitty events are async. For now, we rely on the focus-out of the previous window
    #     having already fired or firing soon. If we track global last, we can ensure it.)

    # 2. Restore the state of *this* window.
    #    Crucially: Restore the LAST KNOWN state (which might be a tmux context),
    #    not just the base window context.
    target_ctx = _get_active_ctx(window)
    _fcitx5_context("restore", target_ctx)


def on_set_user_var(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    """
    Remote tmux pane switch signal.
    fcitx5-tmux-hook sends: SetUserVar=fcitx5_ctx=<base64 of "focus-in:<ctx_id>">
    """
    key = data.get("key", "")
    value = data.get("value", "")

    if key != "fcitx5_ctx" or not value:
        return

    # Decode value (kitty passes it as string, but check if we need to handle bytes/padding)
    # The hook sends: base64("action:ctx_id")
    import base64
    try:
        decoded = base64.b64decode(value).decode('utf-8')
    except Exception:
        return

    parts = decoded.split(":", 1)
    if len(parts) != 2:
        return

    action, tmux_ctx_id = parts
    base_ctx = _ctx_for_window(window)
    full_ctx_id = f"{base_ctx}-{tmux_ctx_id}"

    if action == "focus-out":
        # Remote says it's losing focus (e.g. pane switch)
        _fcitx5_context("save", full_ctx_id)

    elif action == "focus-in":
        # Remote says it's gaining focus

        # Optimization: If the target context is already what we think is active,
        # do nothing to avoid double-restore race with on_focus_change(True).
        current_active = _get_active_ctx(window)
        if current_active == full_ctx_id:
            return

        _fcitx5_context("restore", full_ctx_id)
        # Update the memory so on_focus_change(True) restores this later
        _set_active_ctx(window, full_ctx_id)


def on_close(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    if window.id in _last_focused_ctx:
        del _last_focused_ctx[window.id]
