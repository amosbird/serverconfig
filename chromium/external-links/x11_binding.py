#!/usr/bin/env python3

import json
import os
import pathlib
import re
import struct
import subprocess
import time

NONCE_RE = re.compile(r"[0-9a-f]{32}\Z")
ORDINARY_TITLE_PREFIX = "__ABUSH_ORDINARY__:"
ORDINARY_NONCE_PROPERTY = "_ABUSH_ORDINARY_NONCE"
MANAGED_PROPERTIES = (
    "_ABUSH_BINDING_NONCE", "_ABUSH_REMOTE_ID", "_ABUSH_WORKSPACE_ID",
    "_ABUSH_WINDOW_GENERATION", "_ABUSH_CONTROL_STATE",
)
CHROME_TITLE_SUFFIXES = (" - Google Chrome", " - Chromium")
TEXT_FIELDS = {
    "_ABUSH_REMOTE_ID": "remote_key",
    "_ABUSH_WORKSPACE_ID": "workspace_id",
    "_ABUSH_WINDOW_GENERATION": "generation",
    "_ABUSH_CONTROL_STATE": "control_state",
    "_ABUSH_DISPLAY_TITLE": "display_title",
}
STABLE_FIELDS = {
    "_ABUSH_BINDING_NONCE": "nonce",
    "_ABUSH_REMOTE_ID": "remote_key",
}


class X11Unavailable(RuntimeError):
    pass


class XcffibAdapter:
    def __init__(self):
        try:
            import xcffib
            import xcffib.xproto
        except ImportError as error:
            raise X11Unavailable("xcffib is unavailable") from error
        self.xcffib = xcffib
        self.xproto = xcffib.xproto
        self.connection = xcffib.connect(display=os.environ.get("DISPLAY"))
        self.core = self.connection.core
        self.root = self.connection.get_setup().roots[0].root
        self.atoms = {}

    def atom(self, name):
        if name not in self.atoms:
            encoded = name.encode()
            self.atoms[name] = self.core.InternAtom(False, len(encoded), encoded).reply().atom
        return self.atoms[name]

    def property(self, window, name, property_type="UTF8_STRING"):
        type_atom = getattr(self.xproto.Atom, property_type, None)
        if type_atom is None:
            type_atom = self.atom(property_type)
        reply = self.core.GetProperty(False, window, self.atom(name), type_atom, 0, 4096).reply()
        missing = None if property_type in {"CARDINAL", "WINDOW"} else ""
        if reply.type == self.xproto.Atom.NONE or reply.type != type_atom or reply.format == 0:
            return missing
        raw = bytes(reply.value.buf())
        if property_type in {"CARDINAL", "WINDOW"}:
            if reply.format != 32 or len(raw) != 4:
                return None
            return struct.unpack("=I", raw)[0]
        if reply.format != 8:
            return ""
        return raw.rstrip(b"\0").decode(errors="replace")

    def windows(self):
        pending = list(self.core.QueryTree(self.root).reply().children)
        seen = set()
        while pending:
            window = pending.pop()
            if window in seen:
                continue
            seen.add(window)
            yield window
            try:
                pending.extend(self.core.QueryTree(window).reply().children)
            except self.xcffib.ConnectionException:
                pass

    def title(self, window):
        return self.property(window, "_NET_WM_NAME") or self.property(window, "WM_NAME", "STRING")

    def wm_class(self, window):
        return self.property(window, "WM_CLASS", "STRING").split("\0")

    def pid(self, window):
        return self.property(window, "_NET_WM_PID", "CARDINAL")

    def active_window(self):
        return self.property(self.root, "_NET_ACTIVE_WINDOW", "WINDOW")

    def set_text(self, window, name, value, property_type="UTF8_STRING"):
        encoded = str(value).encode()
        self.core.ChangeProperty(self.xproto.PropMode.Replace, window, self.atom(name),
                                 self.atom(property_type), 8, len(encoded), encoded)

    def finish(self):
        self.connection.flush()


def binding_title_matches(title, expected):
    return title == expected or any(title == expected + suffix for suffix in CHROME_TITLE_SUFFIXES)


def browser_process(pid, proc_root="/proc"):
    if pid is None:
        return False
    try:
        arguments = (pathlib.Path(proc_root) / str(pid) / "cmdline").read_bytes().split(b"\0")
    except OSError:
        return False
    executable = pathlib.PurePath(os.fsdecode(arguments[0])).name.lower() if arguments[0] else ""
    return executable in {
        "chrome", "chrome-wrapper", "chromium", "chromium-browser", "google-chrome",
        "google-chrome-beta", "google-chrome-stable", "google-chrome-unstable",
    }


def chrome_process_window(adapter, window):
    return browser_process(adapter.pid(window))


def qtile_command(name, window):
    return [
        "qtile",
        "cmd-obj",
        "--object",
        "root",
        "--function",
        name,
        "--args",
        str(window),
    ]


def qtile_move_command(window, group="c"):
    if group == "f":
        return qtile_command("prepare_ordinary_window", window)
    if group != "c":
        raise ValueError("unsupported Qtile destination")
    return qtile_command("place_abush_browser_window", window)


def qtile_activate_command(window):
    return qtile_command("activate_ordinary_window", window)


def qtile_transition(command, window, group, run=None):
    output = run_qtile(command, run)
    try:
        transition = json.loads(output)
    except (json.JSONDecodeError, TypeError) as error:
        raise X11Unavailable("invalid Qtile transition response") from error
    if not isinstance(transition, dict) or transition.get("id") != window or \
            transition.get("group") != group:
        raise X11Unavailable("Qtile transition did not reach the exact window")
    return transition


def notify_qtile(window, run=None):
    try:
        qtile_transition(qtile_move_command(window), window, "c", run)
    except X11Unavailable:
        return False
    return True


def run_qtile(command, run=None):
    run = run or subprocess.run
    try:
        result = run(command, capture_output=True, text=True, check=False, timeout=2)
    except (OSError, subprocess.TimeoutExpired) as error:
        raise X11Unavailable("Qtile command failed") from error
    if result.returncode != 0:
        raise X11Unavailable("Qtile command failed")
    return result.stdout.strip()


def qtile_window_info(window, run=None):
    output = run_qtile([
        "qtile", "cmd-obj", "--object", "window", str(window), "--function", "info",
    ], run)
    try:
        info = json.loads(output)
    except (json.JSONDecodeError, TypeError) as error:
        raise X11Unavailable("invalid Qtile window info") from error
    if not isinstance(info, dict) or not isinstance(info.get("group"), str):
        raise X11Unavailable("Qtile window info has no group")
    return info


def verify_ordinary_activation(adapter, window, nonce, run=None, attempts=20, interval=0.05):
    for attempt in range(attempts):
        try:
            info = qtile_window_info(window, run)
            active = getattr(adapter, "active_window", lambda: None)()
            if active == window and info.get("id") == window and info.get("group") == "f" and \
                    adapter.property(window, ORDINARY_NONCE_PROPERTY) == nonce and \
                    not managed_window(adapter, window):
                return True
        except X11Unavailable:
            if attempt + 1 == attempts:
                raise
        if attempt + 1 < attempts:
            time.sleep(interval)
    return False


def ordinary_title_matches(title, nonce):
    expected = f"{ORDINARY_TITLE_PREFIX}{nonce}"
    return binding_title_matches(title, expected)


def managed_window(adapter, window):
    if adapter.property(window, "WM_WINDOW_ROLE", "STRING") == "abush-browser":
        return True
    return any(adapter.property(window, name) for name in MANAGED_PROPERTIES)


def ordinary_candidate(adapter, window, nonce):
    return ordinary_title_matches(adapter.title(window), nonce) and \
        any("chrom" in value.lower() for value in adapter.wm_class(window)) and \
        chrome_process_window(adapter, window) and not managed_window(adapter, window)


def find_ordinary_window(adapter, nonce, attempts=100, interval=0.05):
    for attempt in range(attempts):
        matches = [window for window in adapter.windows()
                   if ordinary_candidate(adapter, window, nonce)]
        if len(matches) == 1:
            return matches[0]
        if len(matches) > 1:
            raise ValueError("ambiguous ordinary staging nonce")
        if attempt + 1 < attempts:
            time.sleep(interval)
    raise X11Unavailable("matching ordinary Chrome X11 window was not found")


def prepare_ordinary_window(payload, adapter=None, attempts=100, interval=0.05, run=None):
    if not isinstance(payload, dict) or set(payload) != {"nonce"} or \
            not isinstance(payload["nonce"], str) or not NONCE_RE.fullmatch(payload["nonce"]):
        raise ValueError("invalid ordinary prepare payload")
    adapter = adapter or XcffibAdapter()
    window = find_ordinary_window(adapter, payload["nonce"], attempts, interval)
    if managed_window(adapter, window):
        raise ValueError("ordinary staging window is managed")
    adapter.set_text(window, ORDINARY_NONCE_PROPERTY, payload["nonce"])
    adapter.finish()
    if adapter.property(window, ORDINARY_NONCE_PROPERTY) != payload["nonce"] or \
            managed_window(adapter, window):
        raise X11Unavailable("ordinary prepare verification failed")
    qtile_transition(qtile_move_command(window, "f"), window, "f", run)
    if adapter.property(window, ORDINARY_NONCE_PROPERTY) != payload["nonce"] or \
            managed_window(adapter, window):
        raise X11Unavailable("ordinary prepare changed window identity")
    return {"xid": window, "nonce": payload["nonce"]}


def prepared_ordinary_identity(payload):
    if not isinstance(payload, dict) or set(payload) != {"nonce", "xid"} or \
            not isinstance(payload["nonce"], str) or not NONCE_RE.fullmatch(payload["nonce"]) or \
            not isinstance(payload["xid"], int) or isinstance(payload["xid"], bool) or \
            payload["xid"] <= 0:
        raise ValueError("invalid prepared ordinary identity")
    return payload["xid"], payload["nonce"]


def verify_ordinary_chrome(adapter, window, nonce, require_staging_title=False):
    title_mismatch = require_staging_title and \
        not ordinary_title_matches(adapter.title(window), nonce)
    if adapter.property(window, ORDINARY_NONCE_PROPERTY) != nonce or title_mismatch or \
            not any("chrom" in value.lower() for value in adapter.wm_class(window)) or \
            not chrome_process_window(adapter, window) or managed_window(adapter, window):
        raise ValueError("ordinary prepared window identity mismatch")
    return True


def verify_prepared_ordinary_window(payload, adapter=None):
    window, nonce = prepared_ordinary_identity(payload)
    adapter = adapter or XcffibAdapter()
    return verify_ordinary_chrome(adapter, window, nonce, require_staging_title=True)


def activate_ordinary_window(payload, adapter=None, run=None, verify=None):
    window, nonce = prepared_ordinary_identity(payload)
    adapter = adapter or XcffibAdapter()
    verify_ordinary_chrome(adapter, window, nonce)
    qtile_transition(qtile_activate_command(window), window, "f", run)
    if adapter.property(window, ORDINARY_NONCE_PROPERTY) != nonce or \
            managed_window(adapter, window):
        raise X11Unavailable("ordinary activation changed window identity")
    if verify is None:
        verified = verify_ordinary_activation(adapter, window, nonce, run)
    else:
        verified = verify(window)
    if verified is not True:
        raise X11Unavailable("ordinary activation verification failed")
    return {"xid": window, "nonce": nonce}


def bind_workspace_window(
    payload,
    adapter=None,
    attempts=100,
    interval=0.05,
    qtile_notifier=None,
):
    required = {"nonce", "remote_key", "workspace_id", "generation", "control_state",
                "display_title"}
    if not isinstance(payload, dict) or set(payload) != required:
        raise ValueError("missing X11 binding metadata")
    nonce = payload["nonce"]
    if not isinstance(nonce, str) or not NONCE_RE.fullmatch(nonce):
        raise ValueError("invalid binding nonce")
    for key in required - {"nonce", "generation"}:
        if not isinstance(payload[key], str) or not payload[key] or len(payload[key]) > 256:
            raise ValueError(f"invalid {key}")
    if not isinstance(payload["generation"], int) or isinstance(payload["generation"], bool) or \
            payload["generation"] <= 0:
        raise ValueError("invalid generation")
    adapter = adapter or XcffibAdapter()
    expected = f"__ABUSH_BIND__:{nonce}:a"
    matches = []
    for attempt in range(attempts):
        existing = [window for window in adapter.windows()
                    if adapter.property(window, "WM_WINDOW_ROLE", "STRING") == "abush-browser" and
                    all(adapter.property(window, prop) == str(payload[field])
                        for prop, field in STABLE_FIELDS.items())]
        fresh = [window for window in adapter.windows()
                 if binding_title_matches(adapter.title(window), expected) and
                 any("chrom" in value.lower() for value in adapter.wm_class(window))]
        matches = existing or fresh
        if len(matches) == 1:
            window = matches[0]
            if not chrome_process_window(adapter, window):
                raise ValueError("X11 binding candidate is not a Chrome process")
            break
        if len(matches) > 1:
            raise ValueError("ambiguous X11 binding nonce")
        if attempt + 1 < attempts:
            time.sleep(interval)
    if len(matches) != 1:
        raise X11Unavailable("matching Chrome X11 window was not found")
    window = matches[0]
    adapter.set_text(window, "WM_WINDOW_ROLE", "abush-browser", "STRING")
    for prop, field in {**STABLE_FIELDS, **TEXT_FIELDS}.items():
        adapter.set_text(window, prop, payload[field])
    adapter.finish()
    observed = {prop: adapter.property(window, prop) for prop in TEXT_FIELDS}
    observed["_ABUSH_BINDING_NONCE"] = adapter.property(window, "_ABUSH_BINDING_NONCE")
    observed["WM_WINDOW_ROLE"] = adapter.property(window, "WM_WINDOW_ROLE", "STRING")
    expected_observed = {
        "WM_WINDOW_ROLE": "abush-browser",
        "_ABUSH_BINDING_NONCE": nonce,
        **{prop: str(payload[field]) for prop, field in TEXT_FIELDS.items()},
    }
    if observed != expected_observed:
        raise X11Unavailable(f"X11 binding verification failed: {observed}")
    qtile_moved = (qtile_notifier or notify_qtile)(window)
    try:
        adapter.set_text(window, "WM_CLASS", "abush-browser\0AbushBrowser\0", "STRING")
        adapter.set_text(window, "_NET_WM_NAME", payload["display_title"])
        adapter.set_text(window, "_NET_WM_VISIBLE_NAME", payload["display_title"])
        adapter.finish()
    except Exception:
        pass
    return {
        "available": True,
        "xid": window,
        "observed": observed,
        "qtile_moved": qtile_moved,
    }
