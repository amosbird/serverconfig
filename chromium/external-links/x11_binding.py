#!/usr/bin/env python3

import json
import marshal
import os
import pathlib
import re
import shlex
import socket
import stat
import struct
import subprocess
import time

NONCE_RE = re.compile(r"[0-9a-f]{32}\Z")
ORDINARY_TITLE_PREFIX = "__ABUSH_ORDINARY__:"
ORDINARY_NONCE_PROPERTY = "_ABUSH_ORDINARY_NONCE"
LEGACY_BROWSER_PROPERTIES = (
    "_ABUSH_CANDIDATE_NONCE", "_ABUSH_CANDIDATE_CLAIM", "_ABUSH_BINDING_NONCE",
    "_ABUSH_REMOTE_ID", "_ABUSH_WORKSPACE_ID", "_ABUSH_WINDOW_GENERATION",
    "_ABUSH_CONTROL_STATE", "_ABUSH_DISPLAY_TITLE", "_ABUSH_PREMAP_TOKEN",
    "_ABUSH_PREMAP_BASELINE",
)
CHROME_TITLE_SUFFIXES = (" - Google Chrome", " - Chromium")
QTILE_IPC_MAX_REPLY = 1024 * 1024
QTILE_IPC_HEADER = struct.Struct("!L")
BROWSER_EXECUTABLES = {
    "chrome", "chrome-wrapper", "chromium", "chromium-browser", "google-chrome",
    "google-chrome-beta", "google-chrome-stable", "google-chrome-unstable",
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
        if reply.type == self.xproto.Atom.Any or reply.type != type_atom or reply.format == 0:
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


def command_executable(raw):
    fields = raw.split(b"\0")
    while fields and not fields[-1]:
        fields.pop()
    if not fields or not fields[0]:
        return ""
    if len(fields) == 1:
        try:
            tokens = shlex.split(os.fsdecode(fields[0]))
        except ValueError:
            return ""
        if not tokens:
            return ""
        executable = tokens[0]
    else:
        executable = os.fsdecode(fields[0])
    return pathlib.PurePath(executable).name.lower()


def browser_process(pid, proc_root="/proc"):
    if not isinstance(pid, int) or isinstance(pid, bool) or pid <= 0:
        return False
    try:
        raw = (pathlib.Path(proc_root) / str(pid) / "cmdline").read_bytes()
    except OSError:
        return False
    return command_executable(raw) in BROWSER_EXECUTABLES


def browser_incarnation(pid):
    if not isinstance(pid, int) or isinstance(pid, bool) or pid <= 0:
        return None
    try:
        boot = pathlib.Path("/proc/sys/kernel/random/boot_id").read_text().strip()
        fields = pathlib.Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()
        return [boot, pid, fields[19]]
    except (OSError, ValueError, IndexError):
        return None


def chrome_process_window(adapter, window):
    return browser_process(adapter.pid(window))


def qtile_command(name, window, *arguments):
    return [
        "qtile", "cmd-obj", "--object", "root", "--function", name, "--args",
        str(window), *(str(argument) for argument in arguments),
    ]


def qtile_activate_command(window, expected=None):
    arguments = () if expected is None else (expected,)
    return qtile_command("activate_ordinary_window", window, *arguments)


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


def qtile_socket_path():
    injected = os.environ.get("ABUSH_QTILE_SOCKET")
    if injected:
        path = pathlib.Path(injected)
        if not path.is_absolute():
            raise X11Unavailable("Qtile socket path is not absolute")
        return path
    cache = pathlib.Path(os.environ.get(
        "XDG_CACHE_HOME", pathlib.Path.home() / ".cache")) / "qtile"
    display = os.environ.get("WAYLAND_DISPLAY") or os.environ.get("DISPLAY")
    if display:
        return cache / f"qtilesocket.{display}"
    for fallback in ("wayland-0", ":0"):
        candidate = cache / f"qtilesocket.{fallback}"
        if candidate.exists():
            return candidate
    raise X11Unavailable("Qtile socket path is unavailable")


def qtile_ipc_call(selectors, function, arguments=(), path=None):
    path = pathlib.Path(path) if path is not None else qtile_socket_path()
    try:
        before = path.lstat()
    except OSError as error:
        raise X11Unavailable("Qtile socket is unavailable") from error
    if path.is_symlink() or not stat.S_ISSOCK(before.st_mode) or before.st_uid != os.getuid() or \
            stat.S_IMODE(before.st_mode) != 0o600:
        raise X11Unavailable("Qtile socket is unsafe")
    request = marshal.dumps((selectors, function, tuple(arguments), {}, True))
    frame = QTILE_IPC_HEADER.pack(len(request)) + request
    try:
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
            client.settimeout(2)
            client.connect(str(path))
            if hasattr(socket, "SO_PEERCRED"):
                credentials = client.getsockopt(
                    socket.SOL_SOCKET, socket.SO_PEERCRED, struct.calcsize("3i"))
                _, peer_uid, _ = struct.unpack("3i", credentials)
                if peer_uid != os.getuid():
                    raise X11Unavailable("Qtile socket peer is not owned by this user")
            after = path.lstat()
            if (after.st_dev, after.st_ino) != (before.st_dev, before.st_ino):
                raise X11Unavailable("Qtile socket changed during connection")
            client.sendall(frame)
            client.shutdown(socket.SHUT_WR)
            response = bytearray()
            while len(response) <= QTILE_IPC_MAX_REPLY + QTILE_IPC_HEADER.size:
                chunk = client.recv(65536)
                if not chunk:
                    break
                response.extend(chunk)
    except X11Unavailable:
        raise
    except (OSError, TimeoutError) as error:
        raise X11Unavailable("Qtile IPC command failed") from error
    if len(response) < QTILE_IPC_HEADER.size:
        raise X11Unavailable("Qtile IPC response is truncated")
    size = QTILE_IPC_HEADER.unpack(response[:QTILE_IPC_HEADER.size])[0]
    payload = response[QTILE_IPC_HEADER.size:]
    if size > QTILE_IPC_MAX_REPLY or len(payload) != size:
        raise X11Unavailable("Qtile IPC response has an invalid size")
    try:
        status, result = marshal.loads(payload)
    except (EOFError, TypeError, ValueError) as error:
        raise X11Unavailable("Qtile IPC response is invalid") from error
    if status != 0:
        raise X11Unavailable("Qtile command failed")
    return result


def qtile_ipc_command(command):
    if not isinstance(command, list) or not all(isinstance(part, str) for part in command) or \
            command[:3] != ["qtile", "cmd-obj", "--object"]:
        raise X11Unavailable("invalid Qtile command")
    try:
        function_index = command.index("--function", 3)
    except ValueError as error:
        raise X11Unavailable("invalid Qtile command") from error
    objects = command[3:function_index]
    tail = command[function_index + 1:]
    if not tail:
        raise X11Unavailable("invalid Qtile command")
    function = tail[0]
    arguments = ()
    if len(tail) > 1:
        if tail[1] != "--args":
            raise X11Unavailable("invalid Qtile command")
        arguments = tuple(tail[2:])
    if objects == ["root"]:
        selectors = []
    elif len(objects) == 2 and objects[0] == "window":
        try:
            window = int(objects[1])
        except ValueError as error:
            raise X11Unavailable("invalid Qtile window selector") from error
        if window <= 0:
            raise X11Unavailable("invalid Qtile window selector")
        selectors = [("window", window)]
    else:
        raise X11Unavailable("invalid Qtile object selector")
    return qtile_ipc_call(selectors, function, arguments)


def run_qtile(command, run=None):
    if run is None:
        return json.dumps(qtile_ipc_command(command), separators=(",", ":"))
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
                    not has_legacy_browser_binding(adapter, window):
                return True
        except X11Unavailable:
            if attempt + 1 == attempts:
                raise
        if attempt + 1 < attempts:
            time.sleep(interval)
    return False


def ordinary_title_matches(title, nonce):
    expected = f"{ORDINARY_TITLE_PREFIX}{nonce}"
    return title == expected or any(title == expected + suffix for suffix in CHROME_TITLE_SUFFIXES)


def has_legacy_browser_binding(adapter, window):
    return adapter.property(window, "WM_WINDOW_ROLE", "STRING") == "abush-browser" or \
        any("abushbrowser" in value.lower() or "abush-browser" in value.lower()
            for value in adapter.wm_class(window)) or \
        any(adapter.property(window, name) for name in LEGACY_BROWSER_PROPERTIES)


def ordinary_candidate(adapter, window, nonce):
    return ordinary_title_matches(adapter.title(window), nonce) and \
        any("chrom" in value.lower() for value in adapter.wm_class(window)) and \
        chrome_process_window(adapter, window) and not has_legacy_browser_binding(adapter, window)


def find_ordinary_window(adapter, nonce, attempts=100, interval=0.05, deadline=None):
    for attempt in range(attempts):
        if deadline is not None and time.monotonic() >= deadline:
            break
        matches = [window for window in adapter.windows()
                   if ordinary_candidate(adapter, window, nonce)]
        if len(matches) == 1:
            return matches[0]
        if len(matches) > 1:
            raise ValueError("ambiguous ordinary staging nonce")
        if attempt + 1 < attempts:
            delay = interval if deadline is None else min(interval, deadline - time.monotonic())
            if delay > 0:
                time.sleep(delay)
    raise X11Unavailable("matching ordinary Chrome X11 window was not found")


def prepare_ordinary_window(payload, adapter=None, attempts=100, interval=0.05, run=None,
                            expected_identity=None):
    if not isinstance(payload, dict) or set(payload) != {"nonce"} or \
            not isinstance(payload["nonce"], str) or not NONCE_RE.fullmatch(payload["nonce"]):
        raise ValueError("invalid ordinary prepare payload")
    adapter = adapter or XcffibAdapter()
    deadline = time.monotonic() + 5.0
    window = find_ordinary_window(adapter, payload["nonce"], attempts, interval, deadline)
    identity = browser_incarnation(adapter.pid(window))
    if identity is None or (expected_identity is not None and identity != expected_identity) or \
            not ordinary_candidate(adapter, window, payload["nonce"]):
        raise X11Unavailable("ordinary browser incarnation is unavailable")
    adapter.set_text(window, ORDINARY_NONCE_PROPERTY, payload["nonce"])
    adapter.finish()
    if adapter.property(window, ORDINARY_NONCE_PROPERTY) != payload["nonce"] or \
            has_legacy_browser_binding(adapter, window):
        raise X11Unavailable("ordinary prepare verification failed")
    for attempt in range(attempts):
        if time.monotonic() >= deadline:
            raise X11Unavailable("Qtile ordinary staging title readiness timed out")
        if browser_incarnation(adapter.pid(window)) != identity or \
                adapter.property(window, ORDINARY_NONCE_PROPERTY) != payload["nonce"] or \
                not ordinary_candidate(adapter, window, payload["nonce"]):
            raise X11Unavailable("ordinary browser identity changed during Qtile readiness")
        info = qtile_window_info(window, run)
        if info.get("id") != window:
            raise X11Unavailable("Qtile readiness did not reach the exact window")
        if ordinary_title_matches(info.get("name"), payload["nonce"]):
            break
        if attempt + 1 < attempts:
            delay = min(interval, deadline - time.monotonic())
            if delay > 0:
                time.sleep(delay)
    else:
        raise X11Unavailable("Qtile ordinary staging title readiness timed out")
    if time.monotonic() >= deadline:
        raise X11Unavailable("Qtile ordinary staging title readiness timed out")
    if browser_incarnation(adapter.pid(window)) != identity or \
            adapter.property(window, ORDINARY_NONCE_PROPERTY) != payload["nonce"] or \
            not ordinary_candidate(adapter, window, payload["nonce"]):
        raise X11Unavailable("ordinary browser incarnation changed before prepare")
    expected = json.dumps({"version": 1, "nonce": payload["nonce"], "identity": identity})
    qtile_transition(qtile_command("prepare_ordinary_window", window, expected), window, "f", run)
    if browser_incarnation(adapter.pid(window)) != identity or \
            adapter.property(window, ORDINARY_NONCE_PROPERTY) != payload["nonce"] or \
            has_legacy_browser_binding(adapter, window):
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
            not chrome_process_window(adapter, window) or has_legacy_browser_binding(adapter, window):
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
    identity = browser_incarnation(adapter.pid(window))
    if identity is None:
        raise X11Unavailable("ordinary browser incarnation is unavailable")
    expected = json.dumps({"version": 1, "nonce": nonce, "identity": identity})
    qtile_transition(qtile_activate_command(window, expected), window, "f", run)
    if adapter.property(window, ORDINARY_NONCE_PROPERTY) != nonce or \
            browser_incarnation(adapter.pid(window)) != identity or \
            has_legacy_browser_binding(adapter, window):
        raise X11Unavailable("ordinary activation changed window identity")
    if verify is None:
        verified = verify_ordinary_activation(adapter, window, nonce, run)
    else:
        verified = verify(window)
    if verified is not True:
        raise X11Unavailable("ordinary activation verification failed")
    return {"xid": window, "nonce": nonce}
