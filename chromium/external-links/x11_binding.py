#!/usr/bin/env python3

import ast
import os
import re
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
        reply = self.core.GetProperty(False, window, self.atom(name), self.atom(property_type),
                                      0, 4096).reply()
        return bytes(reply.value.buf()).rstrip(b"\0").decode(errors="replace")

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
        value = self.property(window, "_NET_WM_PID", "CARDINAL")
        try:
            return int(value)
        except ValueError:
            return None

    def active_window(self):
        reply = self.core.GetProperty(False, self.root, self.atom("_NET_ACTIVE_WINDOW"),
                                      self.xproto.Atom.WINDOW, 0, 1).reply()
        values = reply.value.to_atoms()
        return values[0] if values else None

    def set_text(self, window, name, value, property_type="UTF8_STRING"):
        encoded = str(value).encode()
        self.core.ChangeProperty(self.xproto.PropMode.Replace, window, self.atom(name),
                                 self.atom(property_type), 8, len(encoded), encoded)

    def finish(self):
        self.connection.flush()


def binding_title_matches(title, expected):
    return title == expected or any(title == expected + suffix for suffix in CHROME_TITLE_SUFFIXES)


def browser_process(pid):
    if pid is None:
        return False
    try:
        with open(f"/proc/{pid}/cmdline", "rb") as process:
            return b"chrom" in process.read().lower()
    except OSError:
        return False


def qtile_move_command(window, group="c"):
    return [
        "qtile",
        "cmd-obj",
        "--object",
        "window",
        str(window),
        "--function",
        "togroup",
        "--args",
        group,
    ]


def notify_qtile(window, run=None):
    run = run or subprocess.run
    try:
        result = run(
            qtile_move_command(window),
            capture_output=True,
            text=True,
            check=False,
            timeout=2,
        )
    except (OSError, subprocess.TimeoutExpired):
        return False
    return result.returncode == 0


def run_qtile(command, run=None):
    run = run or subprocess.run
    try:
        result = run(command, capture_output=True, text=True, check=False, timeout=2)
    except (OSError, subprocess.TimeoutExpired) as error:
        raise X11Unavailable("Qtile command failed") from error
    if result.returncode != 0:
        raise X11Unavailable("Qtile command failed")
    return result.stdout.strip()


def qtile_window_group(window, run=None):
    output = run_qtile([
        "qtile", "cmd-obj", "--object", "window", str(window), "--function", "info",
    ], run)
    try:
        info = ast.literal_eval(output)
    except (SyntaxError, ValueError) as error:
        raise X11Unavailable("invalid Qtile window info") from error
    if not isinstance(info, dict) or not isinstance(info.get("group"), str):
        raise X11Unavailable("Qtile window info has no group")
    return info["group"]


def verify_ordinary_activation(adapter, window, run=None):
    active = getattr(adapter, "active_window", lambda: None)()
    return active == window and qtile_window_group(window, run) == "f"


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
        not managed_window(adapter, window)


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
    run_qtile(qtile_move_command(window, "f"), run)
    if adapter.property(window, ORDINARY_NONCE_PROPERTY) != payload["nonce"] or \
            managed_window(adapter, window):
        raise X11Unavailable("ordinary prepare changed window identity")
    return {"xid": window, "nonce": payload["nonce"]}


def activate_ordinary_window(payload, adapter=None, run=None, verify=None):
    if not isinstance(payload, dict) or set(payload) != {"nonce", "xid"} or \
            not isinstance(payload["nonce"], str) or not NONCE_RE.fullmatch(payload["nonce"]) or \
            not isinstance(payload["xid"], int) or isinstance(payload["xid"], bool) or \
            payload["xid"] <= 0:
        raise ValueError("invalid ordinary activate payload")
    adapter = adapter or XcffibAdapter()
    window = payload["xid"]
    if adapter.property(window, ORDINARY_NONCE_PROPERTY) != payload["nonce"] or \
            managed_window(adapter, window):
        raise ValueError("ordinary activation identity mismatch")
    run_qtile(["qtile", "cmd-obj", "--object", "group", "f", "--function", "toscreen"], run)
    run_qtile(["qtile", "cmd-obj", "--object", "window", str(window),
               "--function", "focus"], run)
    if adapter.property(window, ORDINARY_NONCE_PROPERTY) != payload["nonce"] or \
            managed_window(adapter, window):
        raise X11Unavailable("ordinary activation changed window identity")
    if verify is None:
        verified = verify_ordinary_activation(adapter, window, run)
    else:
        verified = verify(window)
    if verified is not True:
        raise X11Unavailable("ordinary activation verification failed")
    return {"xid": window, "nonce": payload["nonce"]}


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
            pid = getattr(adapter, "pid", lambda _window: None)(window)
            if pid is not None and not browser_process(pid):
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
