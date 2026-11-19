"""Ordinary browser identity checks and explicitly installed IPC commands."""

import json
import pathlib
import re

from libqtile.backend.base import Window as QtileWindow
from libqtile.command.base import expose_command
from libqtile.core.manager import Qtile


ORDINARY_NONCE = re.compile(r"[0-9a-f]{32}\Z")
LEGACY_BROWSER_PROPERTIES = (
    "_ABUSH_CANDIDATE_NONCE", "_ABUSH_CANDIDATE_CLAIM", "_ABUSH_BINDING_NONCE",
    "_ABUSH_REMOTE_ID", "_ABUSH_WORKSPACE_ID", "_ABUSH_WINDOW_GENERATION",
    "_ABUSH_CONTROL_STATE", "_ABUSH_DISPLAY_TITLE", "_ABUSH_PREMAP_TOKEN",
    "_ABUSH_PREMAP_BASELINE",
)


def qtile_window_by_xid(manager, xid):
    if not isinstance(xid, int) or isinstance(xid, bool) or xid <= 0:
        raise ValueError("invalid X11 window ID")
    window = manager.windows_map.get(xid)
    if not isinstance(window, QtileWindow) or window.wid != xid:
        raise ValueError("unknown X11 window ID")
    return window


def prepare_ordinary_window(manager, xid: int, expected_json: str = ""):
    window = qtile_window_by_xid(manager, xid)
    if not is_chrome_window(window) or has_legacy_browser_binding(window):
        raise ValueError("invalid ordinary Chrome window")
    if expected_json:
        expected = json.loads(expected_json)
        identity = ordinary_browser_incarnation(window)
        if (
            not isinstance(expected, dict)
            or set(expected) != {"version", "nonce", "identity"}
            or type(expected["version"]) is not int
            or expected["version"] != 1
            or not isinstance(expected["nonce"], str)
            or not ORDINARY_NONCE.fullmatch(expected["nonce"])
            or x11_text_property(window, "_ABUSH_ORDINARY_NONCE") != expected["nonce"]
            or identity is None
            or identity != expected["identity"]
            or not re.fullmatch(
                r"__ABUSH_ORDINARY__:" + expected["nonce"]
                + r"(?: - (?:Google Chrome|Chromium))?", window.name or ""
            )
        ):
            raise ValueError("ordinary prepare identity changed")
    window.can_steal_focus = False
    window.togroup("f", switch_group=False)
    return {"id": window.wid, "group": window.group.name if window.group else None}


def activate_ordinary_window(manager, xid: int, expected_json: str = ""):
    window = qtile_window_by_xid(manager, xid)
    if (
        not is_chrome_window(window)
        or has_legacy_browser_binding(window)
        or window.group is None
        or window.group.name != "f"
    ):
        raise ValueError("invalid ordinary Chrome window")
    if expected_json:
        expected = json.loads(expected_json)
        identity = ordinary_browser_incarnation(window)
        if (
            not isinstance(expected, dict)
            or set(expected) != {"version", "nonce", "identity"}
            or type(expected["version"]) is not int
            or expected["version"] != 1
            or not isinstance(expected["nonce"], str)
            or not ORDINARY_NONCE.fullmatch(expected["nonce"])
            or x11_text_property(window, "_ABUSH_ORDINARY_NONCE") != expected["nonce"]
            or identity is None
            or identity != expected["identity"]
        ):
            raise ValueError("ordinary activation identity changed")
    window.can_steal_focus = True
    manager.groups_map["f"].toscreen()
    window.focus(warp=False)
    return {"id": window.wid, "group": window.group.name if window.group else None}


def install_browser_commands():
    commands = {
        "prepare_ordinary_window": prepare_ordinary_window,
        "activate_ordinary_window": activate_ordinary_window,
    }
    registered = getattr(Qtile, "_commands", {}).copy()
    for name, function in commands.items():
        exposed = expose_command(name)(function)
        setattr(Qtile, name, exposed)
        registered[name] = exposed
    Qtile._commands = registered


def ordinary_browser_incarnation(window):
    try:
        pid = window.get_pid()
        if not isinstance(pid, int) or isinstance(pid, bool) or pid <= 0:
            return None
        boot = pathlib.Path("/proc/sys/kernel/random/boot_id").read_text().strip()
        fields = pathlib.Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()
        return [boot, pid, fields[19]]
    except (OSError, ValueError, IndexError, AttributeError):
        return None


def is_chrome_window(window):
    if not isinstance(window, QtileWindow):
        return False
    wm_class = [value.lower() for value in (window.get_wm_class() or [])]
    return any("chrom" in value for value in wm_class)


def x11_text_property(window, name, property_type="UTF8_STRING"):
    try:
        value = window.window.get_property(name, property_type, unpack=str)
    except (AttributeError, ValueError, TypeError):
        return ""
    if isinstance(value, bytes):
        return value.rstrip(b"\0").decode(errors="replace")
    if isinstance(value, str):
        return value.rstrip("\0")
    return ""


def has_legacy_browser_binding(window):
    return (
        window.get_wm_role() == "abush-browser"
        or any(
            "abushbrowser" in value.lower() or "abush-browser" in value.lower()
            for value in (window.get_wm_class() or [])
        )
        or any(x11_text_property(window, name) for name in LEGACY_BROWSER_PROPERTIES)
    )
