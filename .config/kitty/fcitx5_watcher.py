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
DEBUG = os.environ.get("FCITX5_WATCHER_DEBUG", "")

_last_focused_ctx: str | None = None


def _log(msg: str) -> None:
    if DEBUG:
        print(f"[fcitx5-watcher] {msg}", file=sys.stderr, flush=True)


def _ctx_for_window(window: Window) -> str:
    return f"kitty-{KITTY_PID}-{window.id}"


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
    global _last_focused_ctx
    focused = data.get("focused", False)
    ctx = _ctx_for_window(window)

    if not focused:
        # WM focus-out (e.g. switched to another app): save current state
        if _last_focused_ctx:
            _fcitx5_context("save", _last_focused_ctx)
        return

    # Save current IM state (belongs to previous window) synchronously,
    # then restore this window's state.
    if _last_focused_ctx and _last_focused_ctx != ctx:
        try:
            subprocess.run(
                [CONTEXT_CMD, "save", _last_focused_ctx],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                timeout=1,
            )
        except Exception:
            pass

    _fcitx5_context("restore", ctx)
    _last_focused_ctx = ctx


def on_set_user_var(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    """
    Remote tmux pane switch signal.
    fcitx5-tmux-hook sends: SetUserVar=fcitx5_ctx=<base64 of "focus-in:<ctx_id>">
    """
    global _last_focused_ctx
    key = data.get("key", "")
    value = data.get("value", "")
    _log(f"set_user_var: key={key} value={value}")

    if key != "fcitx5_ctx" or not value:
        return

    parts = value.split(":", 1)
    if len(parts) != 2:
        return

    action, ctx_id = parts
    if action == "focus-out":
        _fcitx5_context("save", f"{_ctx_for_window(window)}-{ctx_id}")
    elif action == "focus-in":
        full_ctx_id = f"{_ctx_for_window(window)}-{ctx_id}"
        _fcitx5_context("restore", full_ctx_id)
        _last_focused_ctx = full_ctx_id


def on_close(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    global _last_focused_ctx
    ctx = _ctx_for_window(window)
    if _last_focused_ctx == ctx:
        _last_focused_ctx = None
