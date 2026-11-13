import json
import os
import pathlib
import re
import subprocess
import time
from threading import Lock

import xcffib.xproto
from libqtile import bar, hook, layout, qtile, widget
from libqtile.command.base import expose_command
from libqtile.config import (
    Click,
    Drag,
    DropDown,
    Group,
    Key,
    Match,
    MatchAny,
    Rule,
    ScratchPad,
    Screen,
)
from libqtile.backend.base import Window as QtileWindow
from libqtile.core.manager import Qtile
from libqtile.backend.x11 import core as x11_core
from libqtile.backend.x11 import xcbq
from libqtile.lazy import lazy
from libqtile.scratchpad import DropDownToggler
from libqtile.utils import send_notification
from libqtile.configurable import Configurable


super_r = "mod3"
super_l = "mod4"
mod5 = "mod5"
alt = "mod1"
ctrl = "control"
shift = "shift"
lock = "lock"


class Shell:
    def __init__(self, window):
        self.scratchpad_name = "scratchpad"
        self.window = window

    def visible(self):
        if self.window.group is None:
            return False
        return (
            self.window.group.name != self.scratchpad_name
            and self.window.group is self.window.qtile.current_group
            and self.window.has_focus
        )

    def show_left(self):
        self.show_float(0, 0)

    def show_right(self):
        screen = self.window.qtile.current_screen
        if screen is None or not self.window.qtile.screens:
            return
        self.show_float(int(screen.width / 2), 0)

    def toggle_left(self):
        if self.visible() and self.window.float_x == 0:
            self.hide()
        else:
            self.show_left()

    def toggle_right(self):
        screen = self.window.qtile.current_screen
        if screen is None or not self.window.qtile.screens:
            return
        if self.visible() and self.window.float_x == int(screen.width / 2):
            self.hide()
        else:
            self.show_right()

    def show_float(self, x, y):
        win = self.window
        screen = win.qtile.current_screen
        if screen is None or not win.qtile.screens:
            return
        win.togroup(win.qtile.current_group.name)
        win.opacity = 0.95
        win.enable_floating()
        win.place(
            int(screen.x + x),
            int(screen.y + y),
            int(screen.width / 2),
            int(screen.height),
            win.qtile.current_group.floating_layout.border_width,
            win.qtile.current_group.floating_layout.border_focus,
            above=True,
        )
        win.bring_to_front()
        win.focus()

    def show_tiled(self):
        win = self.window
        win.qtile.groups_map["h"].toscreen()
        win.togroup("h")
        win.disable_floating()
        win.opacity = 1
        win.focus()

    def hide(self):
        self.window.togroup(self.scratchpad_name)


class ShellHolder:
    def __init__(self):
        self.shell: Shell | None = None
        self._spawned: tuple[Match, int] | None = None
        self.spawn_lock = Lock()

    def _show(self, mode):
        # The shell was just spawned for this, so it is shown rather than
        # toggled: a window that has only now been managed is focused and may
        # be tiled at exactly the half of the screen it is asked for, which a
        # toggle reads as "already there" and hides again.
        if mode == 2:
            self.shell.show_left()
        elif mode == 1:
            self.shell.show_right()
        else:
            self.shell.show_tiled()

    def _spawn(self, mode: int):
        with self.spawn_lock:
            if self.shell is not None or self._spawned is not None:
                return
            pid = qtile.spawn(
                [
                    "kitty",
                    "-T",
                    "urxvt_scratchpad",
                    "-c",
                    "/home/amos/.config/kitty/kitty-mux.conf",
                    "--listen-on",
                    "unix:/tmp/kitty-mux-socket",
                ],
                shell=True,
            )
            self._spawned = (Match(net_wm_pid=pid), mode)

    def register(self, window):
        if self.shell is not None or window.name != "urxvt_scratchpad":
            return False
        if self._spawned is not None and not self._spawned[0].compare(window):
            return False
        mode = self._spawned[1] if self._spawned is not None else None
        self.shell = Shell(window)
        self._spawned = None
        if mode is not None:
            self._show(mode)
        return True

    def recover(self, windows):
        found = [window for window in windows if window.name == "urxvt_scratchpad"]
        if not found:
            return
        self.register(next((window for window in found if window.has_focus), found[0]))
        for window in found:
            if window is not self.shell.window:
                window.kill()

    def on_client_killed(self, client, *args, **kwargs):
        if self.shell is not None and self.shell.window is client:
            self.shell = None
            self._spawned = None

    def toggle_left(self):
        if self.shell:
            self.shell.toggle_left()
        else:
            self._spawn(2)

    def toggle_right(self):
        if self.shell:
            self.shell.toggle_right()
        else:
            self._spawn(1)

    def show_shell(self):
        if self.shell:
            self.shell.show_tiled()
        else:
            self._spawn(0)


shell = ShellHolder()


def qtile_window_by_xid(manager, xid):
    if not isinstance(xid, int) or isinstance(xid, bool) or xid <= 0:
        raise ValueError("invalid X11 window ID")
    window = manager.windows_map.get(xid)
    if not isinstance(window, QtileWindow) or window.wid != xid:
        raise ValueError("unknown X11 window ID")
    return window


def prepare_ordinary_window(manager, xid: int, expected_json: str = ""):
    if expected_json:
        admit_ordinary_premap(manager, xid, expected_json)
    window = qtile_window_by_xid(manager, xid)
    if not is_chrome_window(window) or is_abush_browser_verified(window) or \
            is_abush_browser_candidate(window):
        raise ValueError("invalid ordinary Chrome window")
    window.can_steal_focus = False
    window.togroup("f", switch_group=False)
    return {"id": window.wid, "group": window.group.name if window.group else None}


def admit_ordinary_premap(manager, xid, expected_json):
    expected = json.loads(expected_json)
    if not isinstance(expected, dict) or set(expected) != {"version", "nonce", "identity"} or \
            type(expected["version"]) is not int or expected["version"] != 1 or \
            not isinstance(expected["nonce"], str) or not ABUSH_NONCE.fullmatch(expected["nonce"]):
        raise ValueError("invalid exact ordinary admission")
    nonce = expected["nonce"]
    core = manager.core
    state = load_abush_premap(core)
    held = state["pending"].get(xid)
    if xid in manager.windows_map:
        window = qtile_window_by_xid(manager, xid)
        if x11_text_property(window, "_ABUSH_ORDINARY_NONCE") != nonce or \
                abush_browser_incarnation(window) != expected["identity"] or \
                is_abush_browser_verified(window) or is_abush_browser_candidate(window) or \
                x11_text_property(window, ABUSH_CANDIDATE_CLAIM_PROPERTY) or \
                window.get_wm_role() == "abush-browser" or \
                (held is not None and x11_text_property(window, "_ABUSH_PREMAP_TOKEN") != held["token"]):
            raise ValueError("ordinary admission identity changed")
        if held is not None:
            if window.group is None or window.group.name != "f" or window.can_steal_focus:
                raise ValueError("ordinary constructor recovery is ambiguous")
            del state["pending"][xid]
            save_abush_premap(core)
        return
    if held is None:
        raise ValueError("ordinary XID was not held")
    raw = x11_core.window.XWindow(core.conn, xid)
    pid_values = raw.get_property("_NET_WM_PID", "CARDINAL", unpack=int)
    identity = abush_premap_identity(pid_values[0]) if pid_values else None
    title = raw.get_name() or ""
    if identity is None or identity != held["identity"] or identity != expected["identity"] or \
            not re.fullmatch(r"__ABUSH_ORDINARY__:" + nonce +
                             r"(?: - (?:Google Chrome|Chromium))?", title) or \
            raw.get_property("_ABUSH_ORDINARY_NONCE", "UTF8_STRING", unpack=str) != nonce or \
            raw.get_property("_ABUSH_PREMAP_TOKEN", "UTF8_STRING", unpack=str) != held["token"] or \
            not any("chrom" in value.lower() for value in (raw.get_wm_class() or [])) or \
            any(raw.get_property(prop, "UTF8_STRING", unpack=str) for prop in
                (ABUSH_CANDIDATE_CLAIM_PROPERTY, ABUSH_CANDIDATE_NONCE_PROPERTY,
                 "_ABUSH_BINDING_NONCE", "_ABUSH_REMOTE_ID")) or \
            raw.get_property("WM_WINDOW_ROLE", "STRING", unpack=str) == "abush-browser" or \
            xid in manager.windows_map:
        raise ValueError("held ordinary admission identity or role changed")
    desktop = manager.groups.index(manager.groups_map["f"])
    raw.set_property("_NET_WM_DESKTOP", desktop)
    core.conn.flush()
    if raw.get_property("_NET_WM_DESKTOP", "CARDINAL", unpack=int) != [desktop]:
        raise ValueError("ordinary pre-map desktop was not confirmed")
    current_group = manager.current_screen.group.name
    active_xid = getattr(manager.current_window, "wid", None)
    core._abush_passive_map_xid = xid
    try:
        type(core)._abush_original_map_request(core, held["event"])
    finally:
        core._abush_passive_map_xid = None
    window = qtile_window_by_xid(manager, xid)
    if window.group is None or window.group.name != "f" or window.can_steal_focus or \
            manager.current_screen.group.name != current_group or \
            getattr(manager.current_window, "wid", None) != active_xid:
        raise ValueError("ordinary constructor changed group or focus")
    del state["pending"][xid]
    save_abush_premap(core)


def activate_ordinary_window(manager, xid: int):
    window = qtile_window_by_xid(manager, xid)
    if not is_chrome_window(window) or is_abush_browser_verified(window) or \
            is_abush_browser_candidate(window):
        raise ValueError("invalid ordinary Chrome window")
    window.can_steal_focus = True
    manager.groups_map["f"].toscreen()
    window.focus(warp=False)
    return {"id": window.wid, "group": window.group.name if window.group else None}


def passive_window_move(manager, window, destination):
    current_group = manager.current_screen.group.name
    active_xid = getattr(manager.current_window, "wid", None)
    if window.group is not None and window.group.name != destination and active_xid == window.wid:
        raise ValueError("cannot passively move the active X11 window")
    window.can_steal_focus = False
    if window.group is None or window.group.name != destination:
        window.togroup(destination, switch_group=False)
    if window.group is None or window.group.name != destination or \
            manager.current_screen.group.name != current_group or \
            getattr(manager.current_window, "wid", None) != active_xid:
        raise ValueError("passive window placement changed focus or current group")
    return {
        "id": window.wid,
        "group": window.group.name,
        "current_group_before": current_group,
        "current_group_after": manager.current_screen.group.name,
        "active_xid_before": active_xid,
        "active_xid_after": getattr(manager.current_window, "wid", None),
    }


def abush_premap_identity(pid):
    if type(pid) is not int or pid <= 0:
        return None
    try:
        proc = pathlib.Path(f"/proc/{pid}")
        if proc.stat().st_uid != os.getuid():
            return None
        boot = pathlib.Path("/proc/sys/kernel/random/boot_id").read_text().strip()
        fields = (proc / "stat").read_text().rsplit(")", 1)[1].split()
        return [boot, pid, fields[19]]
    except (OSError, ValueError, IndexError):
        return None


def abush_premap_epoch(core):
    return [abush_premap_identity(os.getpid()), type(core)._abush_gate_generation]


def abush_premap_journal_path():
    directory = pathlib.Path(os.environ.get("XDG_STATE_HOME", str(pathlib.Path.home() / ".local/state")))
    display = re.sub(r"[^a-zA-Z0-9_.-]", "_", os.environ.get("DISPLAY", "unknown"))
    return directory / "abush-browser" / f"premap-{display}.json"


def save_abush_premap(core):
    try:
        persist_abush_premap(core)
    except (OSError, ValueError, TypeError) as error:
        core._abush_premap["error"] = f"candidate journal persistence failed: {error}"
        raise


def persist_abush_premap(core):
    state = core._abush_premap
    durable = {"version": 1, "operations": state["operations"], "serial": state["serial"],
               "pending": {str(xid): {key: value for key, value in held.items() if key != "event"}
                           for xid, held in state["pending"].items()}}
    path = abush_premap_journal_path()
    path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    temporary = path.with_suffix(".new")
    with temporary.open("w") as stream:
        os.chmod(temporary, 0o600)
        json.dump(durable, stream, sort_keys=True)
        stream.flush()
        os.fsync(stream.fileno())
    os.replace(temporary, path)
    directory = os.open(path.parent, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(directory)
    finally:
        os.close(directory)
    if json.loads(path.read_text()) != durable:
        raise ValueError("candidate journal persistence was not confirmed")


def load_abush_premap(core):
    state = getattr(core, "_abush_premap", None)
    if isinstance(state, dict):
        if state.get("error"):
            raise ValueError(state["error"])
        return state
    state = {"operations": {}, "pending": {}, "serial": 0}
    core._abush_premap = state
    path = abush_premap_journal_path()
    try:
        if path.exists():
            saved = json.loads(path.read_text())
            if set(saved) != {"version", "operations", "pending", "serial"} or \
                    saved["version"] != 1 or type(saved["serial"]) is not int or \
                    not isinstance(saved["operations"], dict) or \
                    not isinstance(saved["pending"], dict):
                raise ValueError("invalid candidate journal schema")
            for nonce, operation in saved["operations"].items():
                terminal = operation.get("terminal") if isinstance(operation, dict) else None
                if terminal is not None and (not isinstance(terminal, dict) or set(terminal) != {
                        "version", "nonce", "operation_token", "outcome", "identity", "window_id"} or
                        type(terminal["version"]) is not int or terminal["version"] != 1 or
                        terminal["nonce"] != nonce or terminal["identity"] != operation.get("identity") or
                        terminal["operation_token"] != operation.get("operation_token") or
                        terminal["outcome"] not in {"not_invoked", "rejected", "fulfilled_cleaned"} or
                        operation.get("phase") != "aborted-resolved" or
                        (terminal["outcome"] == "fulfilled_cleaned" and
                         (type(terminal["window_id"]) is not int or terminal["window_id"] <= 0)) or
                        (terminal["outcome"] != "fulfilled_cleaned" and terminal["window_id"] is not None)):
                    raise ValueError("invalid candidate journal terminal")
                required = {"identity", "phase", "deadline", "epoch"}
                optional = {"operation_token", "candidate", "overflow", "proof", "xid", "token", "terminal",
                            "baseline"}
                if not isinstance(operation, dict) or not required <= set(operation) or \
                        set(operation) - required - optional or \
                        ("baseline" in operation and (not isinstance(operation["baseline"], dict) or
                         any(not xid.isdecimal() or int(xid) <= 0 or not isinstance(token, str)
                             for xid, token in operation["baseline"].items()))) or \
                        type(operation["deadline"]) not in {int, float} or \
                        (operation.get("operation_token") is not None and
                         (not isinstance(operation["operation_token"], str) or
                          not ABUSH_NONCE.fullmatch(operation["operation_token"]))) or \
                        not ABUSH_NONCE.fullmatch(nonce) or \
                        operation["phase"] not in {"reserved", "dispatched", "admitted", \
                                                   "aborted-resolved", "unknown"} or \
                        not isinstance(operation["identity"], list) or \
                        len(operation["identity"]) != 3 or \
                        not isinstance(operation["identity"][0], str) or \
                        type(operation["identity"][1]) is not int or \
                        operation["identity"][1] <= 0 or \
                        not isinstance(operation["identity"][2], str):
                    raise ValueError("invalid candidate journal operation")
            for operation in saved["operations"].values():
                if operation.get("terminal", {}).get("outcome") == "rejected":
                    operation["phase"] = "unknown"
                    del operation["terminal"]
            state["operations"] = saved["operations"]
            state["serial"] = saved["serial"]
            for xid, held in saved["pending"].items():
                if not isinstance(held, dict) or set(held) != {"nonce", "identity", "token"} or \
                        (held["nonce"] != "" and held["nonce"] not in state["operations"]) or \
                        not isinstance(held["token"], str) or \
                        not xid.isdecimal() or int(xid) <= 0:
                    raise ValueError("invalid candidate journal pending identity")
                held["event"] = type("PremapEvent", (), {"window": int(xid)})()
                state["pending"][int(xid)] = held
    except (OSError, ValueError, TypeError, KeyError) as error:
        state["error"] = f"candidate journal restoration failed: {error}"
        raise ValueError(state["error"]) from error
    return state


def reserve_abush_premap(manager, nonce, identity_json):
    core = manager.core
    if not getattr(type(core).handle_MapRequest, "_abush_gate", False) or \
            not hasattr(core, "conn") or not hasattr(x11_core.window, "XWindow"):
        raise ValueError("Qtile pre-map admission v2 is unavailable")
    identity = json.loads(identity_json)
    operation_token = None
    if isinstance(identity, dict) and set(identity) == {"identity", "operation_token"}:
        operation_token = identity["operation_token"]
        identity = identity["identity"]
        if not isinstance(operation_token, str) or not ABUSH_NONCE.fullmatch(operation_token):
            raise ValueError("invalid candidate operation token")
    if not isinstance(nonce, str) or not ABUSH_NONCE.fullmatch(nonce) or \
            not isinstance(identity, list) or len(identity) != 3 or \
            identity != abush_premap_identity(identity[1]):
        raise ValueError("invalid candidate browser incarnation")
    state = load_abush_premap(core)
    for key, operation in list(state["operations"].items()):
        current_identity = abush_premap_identity(operation["identity"][1])
        if current_identity is not None and operation["identity"] != current_identity:
            state["pending"] = {xid: held for xid, held in state["pending"].items()
                                if held["nonce"] != key}
            del state["operations"][key]
        elif operation["phase"] == "reserved" and (operation["deadline"] < time.monotonic() or
                operation.get("epoch") != abush_premap_epoch(core)):
            del state["operations"][key]
    if nonce in state["operations"] or nonce in ABUSH_CANDIDATE_USED or \
            any(op["identity"] == identity and op["phase"] != "aborted-resolved"
                for op in state["operations"].values()) or \
            sum(op["phase"] != "aborted-resolved" for op in state["operations"].values()) >= 16:
        raise ValueError("candidate browser already has an unresolved admission")
    baseline = {}
    for xid in core._root.query_tree():
        raw = x11_core.window.XWindow(core.conn, xid)
        pid_values = raw.get_property("_NET_WM_PID", "CARDINAL", unpack=int)
        if not pid_values or abush_premap_identity(pid_values[0]) != identity:
            continue
        token = f"{nonce}:baseline:{xid}"
        raw.set_property("_ABUSH_PREMAP_BASELINE", token, "UTF8_STRING", 8)
        if raw.get_property("_ABUSH_PREMAP_BASELINE", "UTF8_STRING", unpack=str) != token:
            raise ValueError("candidate baseline token was not confirmed")
        baseline[str(xid)] = token
    state["operations"][nonce] = {
        "identity": identity, "phase": "reserved", "deadline": time.monotonic() + 10,
        "epoch": abush_premap_epoch(core), "operation_token": operation_token,
        "baseline": baseline,
    }
    save_abush_premap(core)
    return {"accepted": True, "nonce": nonce, "admission_version": 2, "phase": "reserved"}


def arm_abush_browser_candidate(manager, nonce: str):
    state = load_abush_premap(manager.core)
    operation = state.get("operations", {}).get(nonce)
    if operation is None or operation["phase"] != "reserved" or \
            operation.get("epoch") != abush_premap_epoch(manager.core) or \
            operation["deadline"] < time.monotonic() or \
            operation["identity"] != abush_premap_identity(operation["identity"][1]):
        raise ValueError("candidate reservation expired before dispatch")
    operation["phase"] = "dispatched"
    save_abush_premap(manager.core)
    return {"armed": True, "nonce": nonce, "admission_version": 2, "phase": "dispatched"}


def handle_abush_map_request(core, event):
    original = type(core)._abush_original_map_request
    if event.window in core.qtile.windows_map:
        return original(core, event)
    state = load_abush_premap(core)
    if event.window not in state["pending"] and not any(
            op["phase"] in {"dispatched", "unknown"} for op in state["operations"].values()):
        return original(core, event)
    raw = x11_core.window.XWindow(core.conn, event.window)
    pid_values = raw.get_property("_NET_WM_PID", "CARDINAL", unpack=int)
    identity = abush_premap_identity(pid_values[0]) if pid_values else None
    match = ABUSH_CANDIDATE_TITLE.fullmatch(raw.get_name() or "")
    operations = [(nonce, op) for nonce, op in state["operations"].items()
                  if op["phase"] in {"dispatched", "unknown"}]
    exact = [(nonce, op) for nonce, op in operations
             if match is not None and match.group(1) == nonce]
    matches = exact or [(nonce, op) for nonce, op in operations
                        if identity is not None and op["identity"] == identity]
    held = state["pending"].get(event.window)
    if not matches and identity is not None and held is None:
        return original(core, event)
    # Empty nonce is an explicitly unresolved owner, never the first operation.
    nonce, operation = matches[0] if len(matches) == 1 else ("", None)
    if held is not None and raw.get_property(
            "_ABUSH_PREMAP_TOKEN", "UTF8_STRING", unpack=str) != held["token"]:
        del state["pending"][event.window]
        held = None
    if held is None:
        if len(state["pending"]) >= 128:
            for _, possible in operations:
                possible["overflow"] = True
            save_abush_premap(core)
            raise ValueError("candidate pre-map pending limit reached; admission remains denied")
        state["serial"] += 1
        token = f"{nonce}:{state['serial']}"
        raw.set_property("_ABUSH_PREMAP_TOKEN", token, "UTF8_STRING", 8)
        state["pending"][event.window] = {
            "nonce": nonce, "identity": identity, "event": event, "token": token,
        }
        save_abush_premap(core)
    elif match is not None and operation is not None and held["identity"] is None:
        held["nonce"] = nonce
        save_abush_premap(core)
    if operation is not None and match is not None and match.group(1) == nonce and \
            identity == operation["identity"]:
        if operation.get("candidate") not in (None, {"xid": event.window,
                "token": state["pending"][event.window]["token"]}):
            operation["overflow"] = True
        operation["candidate"] = {"xid": event.window,
                                  "token": state["pending"][event.window]["token"]}
        save_abush_premap(core)
    return None


def abush_premap_destroyed(core, xid):
    try:
        core.conn.conn.core.GetWindowAttributes(xid).reply()
    except xcffib.xproto.WindowError:
        return True
    return False


def handle_abush_destroy_notify(core, event):
    state = load_abush_premap(core)
    if not abush_premap_destroyed(core, event.window):
        return None
    if event.window in state["pending"]:
        held = state["pending"].pop(event.window)
        operation = state["operations"].get(held["nonce"])
        if operation is not None and not operation.get("overflow") and operation.get("candidate") == \
                {"xid": event.window, "token": held["token"]}:
            operation["proof"] = {"kind": "destroyed", **operation["candidate"]}
        save_abush_premap(core)
        if event.window in core.qtile.windows_map:
            return type(core)._abush_original_destroy_notify(core, event)
        return None
    return type(core)._abush_original_destroy_notify(core, event)


def admit_abush_browser_candidate(manager, xid: int, nonce: str):
    core = manager.core
    state = load_abush_premap(core)
    operation = state.get("operations", {}).get(nonce)
    held = state.get("pending", {}).get(xid)
    if operation is not None and operation["phase"] == "admitted" and \
            operation.get("xid") == xid:
        window = qtile_window_by_xid(manager, xid)
        claim = abush_candidate_claim(window)
        if x11_text_property(window, "_ABUSH_PREMAP_TOKEN") != operation.get("token") or \
                claim is None or claim["nonce"] != nonce:
            raise ValueError("admitted candidate token changed")
        if not replay_abush_ordinary_pending(manager, nonce):
            raise ValueError("ordinary replay remains denied")
        return {"admitted": True, "nonce": nonce, "xid": xid,
                "admission_version": 2, "first_group": "__abush_quarantine"}
    if operation is None or operation["phase"] not in {"dispatched", "unknown"} or \
            operation.get("overflow") or \
            operation.get("epoch") != abush_premap_epoch(core) or \
            held is None or \
            (held["nonce"] not in {"", nonce} and held["identity"] is not None) or \
            xid in manager.windows_map:
        raise ValueError("candidate was not held by this admission")
    raw = x11_core.window.XWindow(core.conn, xid)
    pid_values = raw.get_property("_NET_WM_PID", "CARDINAL", unpack=int)
    identity = abush_premap_identity(pid_values[0]) if pid_values else None
    title = raw.get_name() or ""
    match = ABUSH_CANDIDATE_TITLE.fullmatch(title)
    if identity is None or identity != abush_premap_identity(operation["identity"][1]) or \
            (held["identity"] is not None and identity != held["identity"]) or \
            identity != operation["identity"] or \
            not any("chrom" in value.lower() for value in (raw.get_wm_class() or [])) or \
            raw.get_property("_ABUSH_PREMAP_TOKEN", "UTF8_STRING", unpack=str) != held["token"] or \
            match is None or match.group(1) != nonce:
        raise ValueError("held candidate identity or exact title changed")
    held.update(nonce=nonce, identity=identity)
    operation["candidate"] = {"xid": xid, "token": held["token"]}
    save_abush_premap(core)
    claim = {"nonce": nonce, "xid": xid, "incarnation": identity,
             "released": False, "placed": False}
    raw.set_property(ABUSH_CANDIDATE_CLAIM_PROPERTY, json.dumps(claim), "UTF8_STRING", 8)
    raw.set_property(ABUSH_CANDIDATE_NONCE_PROPERTY, nonce, "UTF8_STRING", 8)
    desktop = manager.groups.index(manager.groups_map["__abush_quarantine"])
    raw.set_property("_NET_WM_DESKTOP", desktop)
    core.conn.flush()
    if raw.get_property(ABUSH_CANDIDATE_CLAIM_PROPERTY, "UTF8_STRING", unpack=str) != \
            json.dumps(claim) or raw.get_property(
                "_NET_WM_DESKTOP", "CARDINAL", unpack=int)[0] != desktop:
        raise ValueError("candidate pre-map property persistence was not confirmed")
    # The original constructor reads _NET_WM_DESKTOP before client_new.
    type(core)._abush_original_map_request(core, held["event"])
    window = qtile_window_by_xid(manager, xid)
    if window.group is None or window.group.name != "__abush_quarantine" or \
            window.can_steal_focus:
        raise ValueError("candidate constructor did not preserve quarantine")
    del state["pending"][xid]
    operation.update(phase="admitted", xid=xid, token=held["token"])
    save_abush_premap(core)
    if not replay_abush_ordinary_pending(manager, nonce):
        raise ValueError("ordinary replay remains denied")
    return {"admitted": True, "nonce": nonce, "xid": xid,
            "admission_version": 2, "first_group": "__abush_quarantine"}


def replay_abush_ordinary_pending(manager, nonce):
    core = manager.core
    state = load_abush_premap(core)
    operation = state["operations"][nonce]
    for xid, held in list(state["pending"].items()):
        if held["nonce"] not in {"", nonce}:
            continue
        if abush_premap_destroyed(core, xid):
            del state["pending"][xid]
            save_abush_premap(core)
            continue
        raw = x11_core.window.XWindow(core.conn, xid)
        if raw.get_property("_ABUSH_PREMAP_TOKEN", "UTF8_STRING", unpack=str) != held["token"]:
            raise ValueError("ordinary replay token changed")
        pid_values = raw.get_property("_NET_WM_PID", "CARDINAL", unpack=int)
        identity = abush_premap_identity(pid_values[0]) if pid_values else None
        match = ABUSH_CANDIDATE_TITLE.fullmatch(raw.get_name() or "")
        candidate_nonce = raw.get_property(ABUSH_CANDIDATE_NONCE_PROPERTY, "UTF8_STRING", unpack=str)
        target = next((key for key, op in state["operations"].items()
                       if op.get("candidate") == {"xid": xid, "token": held["token"]}), None)
        if target is None:
            target = match.group(1) if match is not None else candidate_nonce
        if target in state["operations"]:
            held["nonce"] = target
            save_abush_premap(core)
            continue
        if identity is None or identity != operation["identity"] or \
                (held["identity"] is not None and held["identity"] != identity):
            continue
        if match is not None or any(raw.get_property(prop, "UTF8_STRING", unpack=str) for prop in
                (ABUSH_CANDIDATE_NONCE_PROPERTY, ABUSH_CANDIDATE_CLAIM_PROPERTY,
                 "_ABUSH_BINDING_NONCE", "_ABUSH_REMOTE_ID")) or \
                raw.get_property("WM_WINDOW_ROLE", "STRING", unpack=str) == "abush-browser":
            continue
        if any(other["phase"] in {"dispatched", "unknown"} and other["identity"] == identity
               for other in state["operations"].values()):
            continue
        current_group = manager.current_screen.group.name
        active_xid = getattr(manager.current_window, "wid", None)
        if xid not in manager.windows_map:
            raw.set_property("_NET_WM_DESKTOP", manager.groups.index(manager.groups_map["f"]))
            core.conn.flush()
            if raw.get_property("_NET_WM_DESKTOP", "CARDINAL", unpack=int) != \
                    [manager.groups.index(manager.groups_map["f"])]:
                raise ValueError("ordinary replay desktop was not confirmed")
            core._abush_passive_map_xid = xid
            try:
                type(core)._abush_original_map_request(core, held["event"])
            finally:
                core._abush_passive_map_xid = None
        window = qtile_window_by_xid(manager, xid)
        if window.group is None or window.group.name != "f" or window.can_steal_focus or \
                manager.current_screen.group.name != current_group or \
                getattr(manager.current_window, "wid", None) != active_xid:
            raise ValueError("ordinary replay constructor changed group or focus")
        del state["pending"][xid]
        save_abush_premap(core)
    return not any(held["nonce"] == nonce for held in state["pending"].values())


def handle_abush_config_load(core, initial):
    if initial:
        state = load_abush_premap(core)
        operations = []
        for nonce, operation in state["operations"].items():
            if operation["phase"] not in {"dispatched", "unknown"}:
                continue
            if abush_premap_browser_terminated(operation["identity"]):
                continue
            if not isinstance(operation.get("baseline"), dict):
                raise ValueError("initial scan admission has no pre-create baseline")
            if operation["identity"] != abush_premap_identity(operation["identity"][1]):
                raise ValueError("initial scan browser incarnation is unavailable")
            operations.append((nonce, operation))
        for xid in core._root.query_tree() if operations else []:
            if xid in core.qtile.windows_map:
                continue
            raw = x11_core.window.XWindow(core.conn, xid)
            attrs = raw.get_attributes()
            if attrs.override_redirect or attrs.map_state == xcffib.xproto.MapState.Unmapped:
                continue
            pid_values = raw.get_property("_NET_WM_PID", "CARDINAL", unpack=int)
            identity = abush_premap_identity(pid_values[0]) if pid_values else None
            match = ABUSH_CANDIDATE_TITLE.fullmatch(raw.get_name() or "")
            same_browser = [(nonce, op) for nonce, op in operations if op["identity"] == identity]
            exact = [(nonce, op) for nonce, op in operations
                     if match is not None and match.group(1) == nonce]
            if any(op.get("baseline", {}).get(str(xid)) == raw.get_property(
                    "_ABUSH_PREMAP_BASELINE", "UTF8_STRING", unpack=str) and
                    str(xid) in op.get("baseline", {}) for _, op in same_browser) and not exact:
                continue
            if not same_browser and not exact:
                if identity is None and (not raw.get_name() or any(
                        "chrom" in value.lower() for value in (raw.get_wm_class() or []))):
                    raise ValueError("initial scan has an unidentified window during admission")
                continue
            # A mapped client may have been visible while the WM was absent. Only stop
            # constructor admission here; this is not a first-visibility guarantee.
            handle_abush_map_request(core, type("PremapEvent", (), {"window": xid})())
            if xid not in state["pending"]:
                raise ValueError("initial candidate scan did not retain admission denial")
            raw.unmap()
            core.conn.flush()
            if raw.get_attributes().map_state != xcffib.xproto.MapState.Unmapped:
                raise ValueError("initial candidate unmap was not confirmed")
        for _, operation in operations:
            if operation.get("epoch") != abush_premap_epoch(core):
                operation["phase"] = "unknown"
                operation["epoch"] = abush_premap_epoch(core)
        if operations:
            save_abush_premap(core)
    return type(core)._abush_original_config_load(core, initial)


def install_abush_premap_gate():
    cls = x11_core.Core
    if not hasattr(cls, "_abush_original_map_request"):
        cls._abush_original_map_request = cls.handle_MapRequest
        cls._abush_original_destroy_notify = cls.handle_DestroyNotify
        if hasattr(cls, "on_config_load"):
            cls._abush_original_config_load = cls.on_config_load
    cls._abush_gate_generation = getattr(cls, "_abush_gate_generation", 0) + 1
    handle_abush_map_request._abush_gate = True
    cls.handle_MapRequest = handle_abush_map_request
    cls.handle_DestroyNotify = handle_abush_destroy_notify
    if hasattr(cls, "_abush_original_config_load"):
        cls.on_config_load = handle_abush_config_load


def register_abush_browser_candidate(manager, nonce: str, identity_json: str = ""):
    if identity_json:
        return reserve_abush_premap(manager, nonce, identity_json)
    raise ValueError("candidate admission v2 browser identity is required")


def finish_abush_browser_candidate(manager, terminal_json: str):
    terminal = json.loads(terminal_json)
    fields = {"version", "nonce", "operation_token", "outcome", "identity", "window_id"}
    if not isinstance(terminal, dict) or set(terminal) != fields or \
            type(terminal["version"]) is not int or terminal["version"] != 1 or \
            terminal["outcome"] not in {"not_invoked", "rejected", "fulfilled_cleaned"} or \
            not isinstance(terminal["nonce"], str) or not ABUSH_NONCE.fullmatch(terminal["nonce"]) or \
            not isinstance(terminal["operation_token"], str) or \
            not ABUSH_NONCE.fullmatch(terminal["operation_token"]) or \
            (terminal["outcome"] == "fulfilled_cleaned" and
             (type(terminal["window_id"]) is not int or terminal["window_id"] <= 0)) or \
            (terminal["outcome"] != "fulfilled_cleaned" and terminal["window_id"] is not None):
        raise ValueError("invalid candidate terminal schema")
    nonce = terminal["nonce"]
    core = manager.core
    state = load_abush_premap(core)
    operation = state["operations"].get(nonce)
    if operation is None or operation.get("operation_token") != terminal["operation_token"] or \
            operation["identity"] != terminal["identity"] or operation.get("overflow"):
        raise ValueError("candidate terminal ownership changed or is ambiguous")
    previous = operation.get("terminal")
    if previous is not None and previous != terminal:
        raise ValueError("conflicting candidate terminal")
    if terminal["outcome"] == "rejected":
        operation["phase"] = "unknown"
        save_abush_premap(core)
        raise ValueError("candidate rejection has no physical absence proof")
    candidate = operation.get("candidate")
    if candidate is not None and not abush_premap_destroyed(core, candidate["xid"]):
        raise ValueError("candidate terminal has a live or reused XID")
    if candidate is not None and terminal["outcome"] != "fulfilled_cleaned":
        raise ValueError("candidate terminal contradicts physical evidence")
    # The extension terminal is a sealed API result, never a timeout or worker-abort guess.
    operation.update(phase="aborted-resolved", terminal=terminal)
    for xid, held in list(state["pending"].items()):
        if held["nonce"] == nonce and candidate is not None and xid == candidate["xid"] and \
                abush_premap_destroyed(core, xid):
            del state["pending"][xid]
    save_abush_premap(core)
    return {"finished": True, "version": 1, "nonce": nonce, "outcome": terminal["outcome"]}


def abush_premap_browser_terminated(identity):
    try:
        boot = pathlib.Path("/proc/sys/kernel/random/boot_id").read_text().strip()
        if identity[0] != boot:
            return True
        try:
            fields = pathlib.Path(f"/proc/{identity[1]}/stat").read_text().rsplit(")", 1)[1].split()
        except FileNotFoundError:
            return True
        return fields[19] != identity[2]
    except (OSError, ValueError, IndexError):
        return False


def release_abush_browser_candidate(manager, nonce: str):
    if not isinstance(nonce, str) or ABUSH_NONCE.fullmatch(nonce) is None:
        raise ValueError("invalid Abush browser candidate nonce")
    state = load_abush_premap(manager.core)
    if nonce in state["operations"]:
        operation = state["operations"][nonce]
        if operation["phase"] in {"dispatched", "unknown"} and \
                abush_premap_browser_terminated(operation["identity"]):
            operation.update(phase="aborted-resolved", proof={"kind": "browser-terminated"})
            save_abush_premap(manager.core)
        if operation["phase"] in {"dispatched", "unknown"}:
            return {"released": False, "nonce": nonce, "admission_version": 2,
                    "phase": operation["phase"], "denial_retained": True}
        if operation["phase"] in {"aborted-resolved", "admitted"}:
            if not replay_abush_ordinary_pending(manager, nonce):
                return {"released": False, "nonce": nonce, "admission_version": 2,
                        "phase": operation["phase"], "denial_retained": True}
        if not operation.get("terminal"):
            del state["operations"][nonce]
        save_abush_premap(manager.core)
    ABUSH_CANDIDATE_LEASES.pop(nonce, None)
    for window in manager.windows_map.values():
        if is_chrome_window(window):
            abush_candidate_claim(window)
    for claim in ABUSH_CANDIDATE_WINDOWS.values():
        if claim["nonce"] == nonce and not claim["placed"] and \
                manager.windows_map.get(claim["xid"]) is claim["window"]:
            claim["released"] = True
            save_abush_candidate_claim(claim["window"], claim)
    return {"released": True, "nonce": nonce, "admission_version": 2,
            "phase": "aborted-resolved", "denial_retained": False}


def quarantine_abush_browser_window(manager, xid: int, nonce: str):
    window = qtile_window_by_xid(manager, xid)
    expected_titles = {
        f"__ABUSH_BIND__:{nonce}:a",
        f"__ABUSH_BIND__:{nonce}:a - Google Chrome",
        f"__ABUSH_BIND__:{nonce}:a - Chromium",
    }
    claim = abush_candidate_claim(window)
    if not isinstance(nonce, str) or ABUSH_NONCE.fullmatch(nonce) is None or \
            not is_chrome_window(window) or is_abush_browser_verified(window) or \
            claim is None or claim["nonce"] != nonce or claim["released"] or \
            window.group is None or window.group.name != "__abush_quarantine" or \
            x11_text_property(window, ABUSH_CANDIDATE_NONCE_PROPERTY) != nonce or \
            (window.name or "") not in expected_titles:
        raise ValueError("unverified Abush browser candidate")
    return passive_window_move(manager, window, "__abush_quarantine")


def place_abush_browser_window(manager, xid: int, expected_json: str):
    window = qtile_window_by_xid(manager, xid)
    try:
        expected = json.loads(expected_json)
    except (TypeError, json.JSONDecodeError) as error:
        raise ValueError("invalid expected Abush metadata") from error
    if not isinstance(expected, dict) or set(expected) != set(ABUSH_METADATA_PROPERTIES) or \
            not is_abush_browser_verified(window, expected):
        raise ValueError("unverified Abush browser window")
    claim = abush_candidate_claim(window)
    if x11_text_property(window, ABUSH_CANDIDATE_CLAIM_PROPERTY) and claim is None:
        raise ValueError("stale Abush browser incarnation")
    if claim is not None and (claim["released"] or claim["nonce"] != expected["nonce"]):
        raise ValueError("stale Abush browser candidate")
    result = passive_window_move(manager, window, "c")
    if claim is not None:
        claim["placed"] = True
        save_abush_candidate_claim(window, claim)
    return result


def reclassify_ordinary_window(manager, xid: int):
    window = qtile_window_by_xid(manager, xid)
    claim = abush_candidate_claim(window)
    if not is_chrome_window(window) or is_abush_browser_verified(window) or \
            (claim is not None and not claim["placed"]):
        raise ValueError("managed metadata was not cleared")
    result = passive_window_move(manager, window, "f")
    if claim is not None:
        ABUSH_CANDIDATE_WINDOWS.pop(window.wid, None)
        window.window.set_property(ABUSH_CANDIDATE_CLAIM_PROPERTY, "", "UTF8_STRING", 8)
    return result


def restore_abush_browser_window(manager, xid: int, group: str):
    window = qtile_window_by_xid(manager, xid)
    if not isinstance(group, str) or group not in manager.groups_map:
        raise ValueError("unknown rollback group")
    if is_abush_browser_candidate(window) and group != "__abush_quarantine":
        raise ValueError("cannot release an unverified candidate during rollback")
    result = passive_window_move(manager, window, group)
    if group == "__abush_quarantine":
        claim = abush_candidate_claim(window)
        if claim is not None:
            claim["placed"] = False
            save_abush_candidate_claim(window, claim)
    return result


def install_browser_commands():
    commands = {
        "prepare_ordinary_window": prepare_ordinary_window,
        "activate_ordinary_window": activate_ordinary_window,
        "finish_abush_browser_candidate": finish_abush_browser_candidate,
        "arm_abush_browser_candidate": arm_abush_browser_candidate,
        "admit_abush_browser_candidate": admit_abush_browser_candidate,
        "register_abush_browser_candidate": register_abush_browser_candidate,
        "release_abush_browser_candidate": release_abush_browser_candidate,
        "quarantine_abush_browser_window": quarantine_abush_browser_window,
        "place_abush_browser_window": place_abush_browser_window,
        "reclassify_ordinary_window": reclassify_ordinary_window,
        "restore_abush_browser_window": restore_abush_browser_window,
    }
    registered = getattr(Qtile, "_commands", {}).copy()
    for name, function in commands.items():
        exposed = expose_command(name)(function)
        setattr(Qtile, name, exposed)
        registered[name] = exposed
    Qtile._commands = registered


@hook.subscribe.startup
def recover_shell():
    shell.recover(qtile.windows_map.values())


@lazy.function
def toggle_shell_left(qtile: Qtile):
    global shell
    shell.toggle_left()


@lazy.function
def toggle_shell_right(qtile: Qtile):
    global shell
    shell.toggle_right()


@lazy.function
def show_shell(qtile: Qtile):
    global shell
    shell.show_shell()


def _scratchpad_window_alive(qtile: Qtile, window) -> bool:
    return window is not None and window.wid in qtile.windows_map


def _drop_scratchpad_dropdown(scratchpad, name):
    dropdown = scratchpad.dropdowns.pop(name, None)
    if dropdown is None:
        return
    try:
        dropdown.unsubscribe()
    except Exception:
        pass
    scratchpad._spawned.pop(name, None)
    if not scratchpad._spawned:
        try:
            hook.unsubscribe.client_new(scratchpad.on_client_new)
        except Exception:
            pass


def _adopt_scratchpad_window(qtile: Qtile, name):
    """Bind an already-mapped window to a dropdown instead of re-spawning.

    ioagui (and similar) is owned by systemd: `systemctl --user start` is a
    no-op when the unit is already active, so ScratchPad's spawn waiter would
    hang forever on Match(title=...) for a window that already exists.
    """
    scratchpad = qtile.groups_map["scratchpad"]
    match = scratchpad_matches[name]
    existing = next(
        (
            window
            for window in qtile.windows_map.values()
            if isinstance(window, QtileWindow) and match.compare(window)
        ),
        None,
    )
    if existing is None:
        return False
    scratchpad._spawned.pop(name, None)
    if not scratchpad._spawned:
        try:
            hook.unsubscribe.client_new(scratchpad.on_client_new)
        except Exception:
            pass
    register_scratchpad_window(existing, hide=True)
    return name in scratchpad.dropdowns


# These dropdowns share the same screen real estate, so showing one hides the
# others instead of stacking them on top of each other.
exclusive_scratchpads = ("chatgpt", "webchat", "tdesktop")


def _hide_exclusive_scratchpads(scratchpad, name):
    if name not in exclusive_scratchpads:
        return
    for other in exclusive_scratchpads:
        if other == name:
            continue
        dropdown = scratchpad.dropdowns.get(other)
        if dropdown is not None:
            dropdown.hide()


def toggle_scratchpad(name):
    @lazy.function
    def toggle(qtile: Qtile):
        scratchpad = qtile.groups_map["scratchpad"]
        if name in scratchpad.dropdowns and not _scratchpad_window_alive(
            qtile, scratchpad.dropdowns[name].window
        ):
            _drop_scratchpad_dropdown(scratchpad, name)
        if name not in scratchpad.dropdowns:
            if not _adopt_scratchpad_window(qtile, name):
                _hide_exclusive_scratchpads(scratchpad, name)
                scratchpad.dropdown_toggle(name)
                return
        dropdown = scratchpad.dropdowns[name]
        if dropdown.window.has_focus:
            dropdown.hide()
            return
        _hide_exclusive_scratchpads(scratchpad, name)
        current_group = qtile.current_group
        dropdown.window.togroup(current_group.name)
        dropdown.show()
        if qtile.current_group is not current_group:
            current_group.toscreen()
        dropdown.window.bring_to_front()
        dropdown.window.focus(warp=True)

    return toggle()


pending_inputstr: tuple[list[int], str | list[str], bool] | None = None
super_r_keycodes: list[int] = []


def key_is_down(keymap, keycode):
    return bool(keymap[keycode // 8] & (1 << (keycode % 8)))


def flush_pending_inputstr(qtile: Qtile):
    global pending_inputstr
    if pending_inputstr is None:
        return

    keymap = qtile.core.conn.conn.core.QueryKeymap().reply().keys
    if any(key_is_down(keymap, keycode) for keycode in super_r_keycodes):
        qtile.call_later(0.01, flush_pending_inputstr, qtile)
        return

    _, command, shell = pending_inputstr
    pending_inputstr = None
    qtile.spawn(command, shell=shell)


def handle_key_release(self, event):
    if pending_inputstr is not None and event.detail in pending_inputstr[0]:
        self.qtile.call_later(0.01, flush_pending_inputstr, self.qtile)


def defer_inputstr(key, value, shell=False):
    command = f'inputstr "{value}"' if shell else ["inputstr", value]

    @lazy.function
    def defer(qtile: Qtile):
        global pending_inputstr, super_r_keycodes
        super_r_keycodes = qtile.core.conn.keysym_to_keycode(xcbq.keysyms["super_r"])
        trigger_keycodes = qtile.core.conn.keysym_to_keycode(xcbq.keysyms[key])
        pending_inputstr = (trigger_keycodes, command, shell)

    return defer()


# shortcut: Qtile has no public release binding; its X11 grab already receives the release event.
x11_core._IGNORED_EVENTS.discard(xcffib.xproto.KeyReleaseEvent)
x11_core.EVENT_TO_HANDLER[xcffib.xproto.KeyReleaseEvent] = "handle_KeyRelease"
x11_core.Core.handle_KeyRelease = handle_key_release


@hook.subscribe.startup
@hook.subscribe.startup_complete
def enable_key_release_events():
    if hasattr(qtile.core, "eventmask"):
        qtile.core.eventmask |= xcffib.xproto.EventMask.KeyRelease
        qtile.core._root.set_attribute(eventmask=qtile.core.eventmask)


keys = [
    Key([super_r], "e", lazy.spawn("rofi -show emoji -modi emoji")),
    Key(
        [super_r],
        "a",
        lazy.spawn("kitty -T float /home/amos/git/work/scripts/insert-cluster.sh"),
    ),
    Key([super_r], "c", lazy.spawn("roficalc")),
    Key([super_r, shift], "c", lazy.spawn("colorinsert")),
    Key([super_r], "w", lazy.spawn("rofiurl")),
    Key([super_r], "f", lazy.spawn("copyq toggle")),
    Key([super_r], "0", defer_inputstr("0", "0.0.0.0")),
    Key([super_r], "1", defer_inputstr("1", "127.0.0.1")),
    Key(
        [super_r],
        "2",
        lazy.spawn('joinwemeet "$(xclip -selection clipboard -out)"', shell=True),
    ),
    Key([super_r], "3", lazy.spawn("rofipass")),
    Key([super_r], "4", defer_inputstr("4", "amosbird@gmail.com")),
    Key([super_r], "r", lazy.spawn("rofidbtbl")),
    Key([super_r], "h", lazy.spawn("rofihosts")),
    Key([super_r], "d", lazy.spawn("dshot | copyq copyImage -", shell=True)),
    Key([super_r, shift], "d", lazy.spawn("dshot | uploadimg -", shell=True)),
    Key([super_r], "t", lazy.spawn("dtinput")),
    Key([super_r], "y", lazy.spawn("ocr")),
    Key(
        [super_r],
        "p",
        lazy.spawn("flameshot gui -r | pngquant - | copyq copyImage -", shell=True),
    ),
    Key(
        [super_r, shift],
        "p",
        lazy.spawn("flameshot gui -r | pngquant - | uploadimg -", shell=True),
    ),
    Key(
        [super_r],
        "u",
        lazy.spawn("xclip -selection clipboard -out | upload -", shell=True),
    ),
    Key([super_r], "o", lazy.spawn("openclipboard")),
    Key([super_r, shift], "r", lazy.spawn("teiler")),
    Key(
        [super_r], "g", lazy.spawn('url "www.google.com/search?pws=0&gl=us&gws_rd=cr&q="')
    ),
    Key(
        [super_r],
        "k",
        defer_inputstr("k", "$(pass show scripts/otp | bash)", shell=True),
    ),
    Key([super_r], "s", lazy.spawn("/home/amos/git/work/scripts/rofitsearch")),
    Key([super_r], "v", lazy.spawn("pavucontrol")),
    Key([ctrl], "F8", lazy.spawn("iwmenu --launcher rofi")),
    Key([ctrl], "F10", lazy.spawn("bzmenu --launcher rofi --interactive")),
    Key([ctrl], "F12", lazy.spawn("reloadtouchpad")),
    Key([super_l, shift], "f", lazy.window.toggle_fullscreen()),
    Key([super_l], "f", lazy.window.toggle_floating()),
    Key([super_l], "z", lazy.spawn("lockscreen")),
    Key([ctrl, alt], "Eisu_toggle", lazy.spawn("toggleaudio")),
    Key([ctrl, alt], "1", toggle_scratchpad("ioa")),
    Key([ctrl, alt], "2", lazy.spawn("togglewemeet")),
    Key([ctrl, alt], "3", lazy.spawn("echo p | nc -U /tmp/scrcpy.socket", shell=True)),
    Key([ctrl, alt], "4", toggle_scratchpad("stalonetray")),
    Key([ctrl, alt], "8", toggle_scratchpad("chatgpt")),
    Key([ctrl, alt], "9", toggle_scratchpad("webchat")),
    Key([ctrl, alt], "0", toggle_scratchpad("tdesktop")),
    Key([ctrl, alt], "minus", toggle_scratchpad("stardict")),
    Key([ctrl, alt], "t", lazy.spawn("rofi-hister")),
    Key([ctrl, alt], "b", toggle_scratchpad("bookmarks")),
    Key(
        [ctrl, alt],
        "a",
        lazy.spawn(
            "google-chrome-stable "
            "--user-data-dir=/home/amos/.config/google-chrome-downloads "
            "--class=download-manager "
            "--app=chrome-extension://jpgfhlaplofoaempbhliigmjbpofeghk/download.html",
            shell=True,
        ),
    ),
    Key([ctrl, alt], "g", lazy.spawn("colorpick")),
    Key([ctrl, alt], "s", toggle_shell_left()),
    Key([ctrl, alt], "l", toggle_shell_right()),
    Key([ctrl, alt], "h", show_shell()),
    Key([super_l], "s", lazy.spawn("kitty fish")),
    Key([super_l], "Home", lazy.spawn("movehome")),
    Key([super_l], "End", lazy.spawn("moveend")),
    Key([super_l], "Insert", lazy.spawn("moveinsert")),
    Key([super_l], "Delete", lazy.spawn("lxrandr")),
    Key([ctrl, alt], "r", lazy.spawn("rofi-runner")),
    Key([super_l], "0", lazy.reload_config()),
    Key([ctrl], "Escape", lazy.spawn("dunstctl close-all")),
    Key([ctrl], "Eisu_Toggle", lazy.spawn("dunstctl history-pop")),
    Key([ctrl], "F1", lazy.spawn("volume mute")),
    Key([ctrl], "F2", lazy.spawn("volume down")),
    Key([ctrl], "F3", lazy.spawn("volume up")),
    Key([ctrl], "F4", lazy.spawn("microphone-mute")),
    Key([ctrl, shift], "F4", lazy.spawn("bluetooth-profile")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("volume down")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("volume up")),
    Key([], "XF86AudioMute", lazy.spawn("volume mute")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -5")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight +5")),
    Key([ctrl, alt], "q", lazy.window.kill()),
    Key([ctrl, alt], "j", lazy.layout.next()),
    Key([ctrl, alt], "k", lazy.layout.previous()),
    # Key([ctrl, alt], "Tab", focus_previous_window()),
    Key([super_l], "w", lazy.next_layout()),
    Key([super_l], "t", lazy.spawn("theme toggle")),
]

mouse = [
    Drag(
        [lock],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [lock], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([lock], "Button2", lazy.spawn("ungrab-keyboard")),
]

scratchpad_matches = {
    "ioa": Match(title="iOA"),
    "tdesktop": Match(wm_class="TelegramDesktop"),
    "webchat": Match(wm_class="webchat"),
    "chatgpt": Match(wm_class="chatgpt"),
    "stardict": Match(title="stardict"),
    "stalonetray": Match(title="stalonetray"),
    "bookmarks": Match(wm_class="bookmark-manager"),
}


ABUSH_CANDIDATE_TITLE = re.compile(
    r"^__ABUSH_BIND__:([0-9a-f]{32}):a(?: - (?:Google Chrome|Chromium))?$"
)
ABUSH_NONCE = re.compile(r"^[0-9a-f]{32}$")
ABUSH_CANDIDATE_NONCE_PROPERTY = "_ABUSH_CANDIDATE_NONCE"
ABUSH_CANDIDATE_LEASE_SECONDS = 10.0
ABUSH_CANDIDATE_LEASES = {}
ABUSH_CANDIDATE_WINDOWS = {}
ABUSH_CANDIDATE_USED = set()
ABUSH_CANDIDATE_CLAIM_PROPERTY = "_ABUSH_CANDIDATE_CLAIM"
ABUSH_CONTROL_STATES = {"remote", "human"}
ABUSH_METADATA_PROPERTIES = {
    "nonce": ("_ABUSH_BINDING_NONCE", "UTF8_STRING"),
    "remote_key": ("_ABUSH_REMOTE_ID", "UTF8_STRING"),
    "workspace_id": ("_ABUSH_WORKSPACE_ID", "UTF8_STRING"),
    "generation": ("_ABUSH_WINDOW_GENERATION", "UTF8_STRING"),
    "control_state": ("_ABUSH_CONTROL_STATE", "UTF8_STRING"),
    "display_title": ("_ABUSH_DISPLAY_TITLE", "UTF8_STRING"),
}


groups = [
    ScratchPad(
        "scratchpad",
        [
            DropDown(
                "ioa",
                # Start via the user unit so the ngnclient daemon's recovery
                # path (`systemctl --user restart ioagui.service`) owns the
                # same process instead of spawning a second GUI.
                "systemctl --user start ioagui.service",
                match=scratchpad_matches["ioa"],
                x=0.26,
                y=0.224,
                opacity=1,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "tdesktop",
                "/opt/telegram/Telegram",
                match=scratchpad_matches["tdesktop"],
                x=0.1,
                y=0.1,
                width=0.8,
                height=0.85,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "webchat",
                "runchat",
                match=scratchpad_matches["webchat"],
                x=0.1,
                y=0.1,
                width=0.8,
                height=0.85,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "chatgpt",
                "runai",
                match=scratchpad_matches["chatgpt"],
                x=0.1,
                y=0.1,
                width=0.8,
                height=0.85,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "bookmarks",
                "bookmark-manager",
                match=scratchpad_matches["bookmarks"],
                x=0.1,
                y=0.05,
                width=0.8,
                height=0.9,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "stardict",
                "kitty -T stardict -e dict.sh",
                match=scratchpad_matches["stardict"],
                x=0.25,
                y=0.1,
                width=0.5,
                height=0.8,
                opacity=0.75,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "stalonetray",
                "tray",
                match=scratchpad_matches["stalonetray"],
                # No position here: the row is as wide as the icons docked in
                # it, so center_tray() places it on every move or resize.
                on_focus_lost_hide=True,
            ),
        ],
    ),
    Group("h"),
    Group("2", layout="max"),
    Group("c", layouts=[layout.Max()]),
    Group("__abush_quarantine"),
]

keys.append(Key([ctrl, alt, shift], "2", lazy.window.togroup("2")))

for i in ["w", "e", "d", "f", "v", "n", "i", "o"]:
    groups.append(Group(i))
    keys.append(Key([ctrl, alt], i, lazy.group[i].toscreen()))
    keys.append(Key([ctrl, alt, shift], i, lazy.window.togroup(i)))

keys.append(Key([ctrl, alt], "c", lazy.group["c"].toscreen()))

def expire_abush_browser_candidate_leases(now=None):
    now = time.monotonic() if now is None else now
    for nonce, deadline in list(ABUSH_CANDIDATE_LEASES.items()):
        if deadline < now:
            del ABUSH_CANDIDATE_LEASES[nonce]


def abush_browser_incarnation(window):
    try:
        pid = window.get_pid()
        if not isinstance(pid, int) or isinstance(pid, bool) or pid <= 0:
            return None
        boot = pathlib.Path("/proc/sys/kernel/random/boot_id").read_text().strip()
        fields = pathlib.Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()
        return [boot, pid, fields[19]]
    except (OSError, ValueError, IndexError, AttributeError):
        return None


def save_abush_candidate_claim(window, claim):
    durable = {key: value for key, value in claim.items() if key != "window"}
    window.window.set_property(
        ABUSH_CANDIDATE_CLAIM_PROPERTY, json.dumps(durable), "UTF8_STRING", 8)


def abush_candidate_claim(window):
    claim = ABUSH_CANDIDATE_WINDOWS.get(window.wid)
    if claim is not None:
        if claim["window"] is not window or claim["incarnation"] != abush_browser_incarnation(window):
            return None
        return claim
    try:
        claim = json.loads(x11_text_property(window, ABUSH_CANDIDATE_CLAIM_PROPERTY))
    except (ValueError, TypeError):
        return None
    if not isinstance(claim, dict) or set(claim) != {
        "nonce", "xid", "incarnation", "released", "placed",
    } or not isinstance(claim["nonce"], str) or not ABUSH_NONCE.fullmatch(claim["nonce"]) or \
            claim["xid"] != window.wid or claim["incarnation"] is None or \
            claim["incarnation"] != abush_browser_incarnation(window) or \
            type(claim["released"]) is not bool or type(claim["placed"]) is not bool:
        return None
    claim["window"] = window
    ABUSH_CANDIDATE_WINDOWS[window.wid] = claim
    ABUSH_CANDIDATE_USED.add(claim["nonce"])
    return claim


def claim_abush_browser_candidate(window):
    if is_chrome_window(window):
        claim = abush_candidate_claim(window)
        if claim is not None and not claim["placed"]:
            window.can_steal_focus = False
            return False
    if not is_chrome_window(window) or window.group is not None:
        return False
    match = ABUSH_CANDIDATE_TITLE.fullmatch(window.name or "")
    nonce = x11_text_property(window, ABUSH_CANDIDATE_NONCE_PROPERTY)
    if not nonce and match is not None:
        nonce = match.group(1)
    now = time.monotonic()
    expire_abush_browser_candidate_leases(now)
    if nonce in ABUSH_CANDIDATE_USED or abush_candidate_claim(window) is not None:
        return False
    deadline = ABUSH_CANDIDATE_LEASES.pop(nonce, None)
    if deadline is None or deadline < now:
        return False
    incarnation = abush_browser_incarnation(window)
    if incarnation is None:
        raise ValueError("cannot establish candidate browser incarnation")
    claim = {"nonce": nonce, "window": window, "xid": window.wid,
             "incarnation": incarnation, "released": False, "placed": False}
    ABUSH_CANDIDATE_WINDOWS[window.wid] = claim
    ABUSH_CANDIDATE_USED.add(nonce)
    save_abush_candidate_claim(window, claim)
    window.window.set_property(ABUSH_CANDIDATE_NONCE_PROPERTY, nonce, "UTF8_STRING", 8)
    window.can_steal_focus = False
    return True


dgroups_app_rules = [
    Rule(Match(func=claim_abush_browser_candidate), group="__abush_quarantine"),
    Rule(Match(wm_class="kitty", title="local"), group="e"),
    Rule(Match(wm_class="kitty", title="work"), group="v"),
    Rule(Match(wm_class="kitty", title="remote"), group="i"),
    Rule(Match(wm_class="wemeetapp"), group="2"),
    Rule(Match(wm_class="xfreerdp"), group="w"),
    Rule(Match(wm_class="Google-chrome"), group="f"),
]

border = dict(border_width=0)

layouts = [
    # layout.Bsp(**border),
    # layout.Stack(num_stacks=2),
    # layout.Matrix(),
    layout.MonadTall(**border),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
    layout.Max(),
]

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.TaskList(
                    borderwidth=0,
                    font="Ubuntu Mono",
                    fontsize=32,
                    highlight_method="block",
                    padding_x=8,
                    padding_y=2,
                    rounded=False,
                ),
            ],
            44,
        )
    )
]


@hook.subscribe.setgroup
def show_browser_tabs():
    for screen in qtile.screens:
        if screen.top is not None:
            screen.top.show(screen.group.name == "c")


@hook.subscribe.startup
def initialize_browser_tabs():
    show_browser_tabs()


def is_chrome_window(window):
    if not isinstance(window, QtileWindow):
        return False
    wm_class = [value.lower() for value in (window.get_wm_class() or [])]
    return any("chrom" in value for value in wm_class) or "abushbrowser" in wm_class


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


def abush_browser_metadata(window):
    metadata = {
        field: x11_text_property(window, property_name, property_type)
        for field, (property_name, property_type) in ABUSH_METADATA_PROPERTIES.items()
    }
    generation = metadata["generation"]
    if not ABUSH_NONCE.fullmatch(metadata["nonce"]) or not metadata["remote_key"] or \
            len(metadata["remote_key"]) > 256 or not metadata["workspace_id"] or \
            len(metadata["workspace_id"]) > 256 or not generation.isascii() or \
            not generation.isdecimal() or int(generation) <= 0 or \
            metadata["control_state"] not in ABUSH_CONTROL_STATES or \
            not metadata["display_title"] or len(metadata["display_title"]) > 256:
        return None
    metadata["generation"] = int(generation)
    return metadata


def is_abush_browser_verified(window, expected=None):
    metadata = abush_browser_metadata(window)
    if not is_chrome_window(window) or window.get_wm_role() != "abush-browser" or \
            metadata is None:
        return False
    if x11_text_property(window, ABUSH_CANDIDATE_CLAIM_PROPERTY) and \
            abush_candidate_claim(window) is None:
        return False
    return expected is None or metadata == expected


def has_abush_candidate_title(window):
    return is_chrome_window(window) and bool(
        ABUSH_CANDIDATE_TITLE.fullmatch(window.name or ""))


def is_abush_browser_candidate(window):
    if not is_chrome_window(window):
        return False
    claim = abush_candidate_claim(window)
    return claim is not None and not (
        claim["placed"] and is_abush_browser_verified(window) and
        abush_browser_metadata(window)["nonce"] == claim["nonce"])


@hook.subscribe.group_window_add
def prevent_abush_browser_focus_steal(group, window):
    if getattr(window.qtile.core, "_abush_passive_map_xid", None) == window.wid or \
            is_abush_browser_verified(window) or is_abush_browser_candidate(window) or \
            has_abush_candidate_title(window):
        window.can_steal_focus = False


def place_abush_browser(window):
    if is_abush_browser_candidate(window):
        destination = "__abush_quarantine"
        window.can_steal_focus = False
    elif is_abush_browser_verified(window):
        destination = "c"
        window.can_steal_focus = False
    elif is_chrome_window(window) and window.group is not None and window.group.name in {
        "c",
        "__abush_quarantine",
    }:
        destination = "f"
    else:
        return False
    if window.group is not None and window.group.name == destination:
        return False
    passive_window_move(window.qtile, window, destination)
    return True


install_abush_premap_gate()
install_browser_commands()


@hook.subscribe.startup
def recover_abush_browsers():
    for window in qtile.windows_map.values():
        place_abush_browser(window)


@hook.subscribe.client_name_updated
def reclassify_abush_browser(window):
    if is_abush_browser_verified(window) or is_abush_browser_candidate(window):
        return False
    if is_chrome_window(window) and window.group is not None and window.group.name in {
        "c",
        "__abush_quarantine",
    }:
        passive_window_move(window.qtile, window, "f")
        return True
    return False


@hook.subscribe.startup
def recover_scratchpad_dropdowns():
    for window in qtile.windows_map.values():
        register_scratchpad_window(window)


def register_scratchpad_window(window, hide=True):
    if not isinstance(window, QtileWindow):
        return
    scratchpad = qtile.groups_map["scratchpad"]
    if any(dropdown.window is window for dropdown in scratchpad.dropdowns.values()):
        return
    for name, match in scratchpad_matches.items():
        if not match.compare(window):
            continue
        if name in scratchpad.dropdowns:
            if _scratchpad_window_alive(qtile, scratchpad.dropdowns[name].window):
                continue
            _drop_scratchpad_dropdown(scratchpad, name)
        config = next(config for config in groups[0].dropdowns if config.name == name)
        first = not scratchpad.dropdowns
        scratchpad.dropdowns[name] = DropDownToggler(window, scratchpad.name, config)
        # Manual registration bypasses ScratchPad.on_client_new, so subscribe the
        # same cleanup hooks it would have installed for the first dropdown.
        if first:
            hook.subscribe.client_killed(scratchpad.on_client_killed)
            hook.subscribe.float_change(scratchpad.on_float_change)
        if hide:
            scratchpad.dropdowns[name].hide()
        else:
            scratchpad.dropdowns[name].show()
        return


def show_scratchpad(name):
    scratchpad = qtile.groups_map["scratchpad"]
    if name in scratchpad.dropdowns:
        dropdown = scratchpad.dropdowns[name]
        _hide_exclusive_scratchpads(scratchpad, name)
        dropdown.show()
        dropdown.window.bring_to_front()
        dropdown.window.focus(warp=True)


qtile.show_scratchpad = show_scratchpad


@hook.subscribe.client_new
def before_window_created(client):
    claim_abush_browser_candidate(client)
    if place_abush_browser(client):
        return
    if "copyq" in client.get_wm_class():
        client.enable_floating()
        client.set_size_floating(2000, 1200)
        client.center()
    elif "kitty" in client.get_wm_class() and client.window.get_name() == "float":
        client.enable_floating()
        client.set_size_floating(2000, 1200)
        client.center()
    elif "kitty" in client.get_wm_class() and client.window.get_name() == "dtpick":
        client.enable_floating()
        client.set_size_floating(400, 120)
        client.center()
    elif "xfreerdp" in client.get_wm_class():
        client.focus()
        client.togroup("w", switch_group=True)
    elif "urxvt_scratchpad" == client.window.get_name():
        # client.togroup("scratchpad", switch_group=False)
        with open("/tmp/urxvt_scratchpad", "w") as file:
            file.write(str(client.wid))
    # elif "stalonetray" in client.get_wm_class():
    #     client.set_size_floating(500, 200)
    #     client.center()
    # doesn't work


def release_abush_candidate_window(client):
    claim = ABUSH_CANDIDATE_WINDOWS.get(client.wid)
    if claim is not None and claim["window"] is client:
        ABUSH_CANDIDATE_WINDOWS.pop(client.wid)
        if client.qtile.windows_map.get(client.wid) is client:
            client.window.set_property(ABUSH_CANDIDATE_CLAIM_PROPERTY, "", "UTF8_STRING", 8)


@hook.subscribe.client_killed
def window_killed(client):
    release_abush_candidate_window(client)
    shell.on_client_killed(client)
    scratchpad = qtile.groups_map.get("scratchpad")
    if scratchpad is None:
        return
    for name, dropdown in list(scratchpad.dropdowns.items()):
        if dropdown.window is client:
            _drop_scratchpad_dropdown(scratchpad, name)
            break


@hook.subscribe.client_managed
def after_window_created(client):
    if shell.register(client):
        return
    if scratchpad_matches["bookmarks"].compare(client):
        register_scratchpad_window(client, hide=False)
    else:
        register_scratchpad_window(client)
    if scratchpad_matches["bookmarks"].compare(client):
        return
    if "Google-chrome" in client.get_wm_class() and client.get_wm_role() == "pop-up":
        screen = client.qtile.current_screen
        client.enable_floating()
        client.set_size_floating(int(screen.width * 0.7), int(screen.height * 0.8))
        client.set_position_floating(
            int(screen.x + screen.width * 0.15),
            int(screen.y + screen.height * 0.1),
        )
    elif "chatgpt" in client.get_wm_class():
        client.keep_above()


# @hook.subscribe.layout_change
# def layout_change(layout, group):
#     send_notification("qtile", f"{layout.name} is now on group {group.name}")


class ConditionalBorderColor(str):
    def __new__(cls, default, matches):
        color = super().__new__(cls, default)
        color.default = default
        color.matches = matches
        return color

    def get_border_for_window(self, win):
        for rule, value in self.matches:
            if rule.compare(win):
                return value
        return self.default


class ConditionalBorderWidth(int):
    def __new__(cls, default, matches):
        border_width = super().__new__(cls, default)
        border_width.default = default
        border_width.matches = matches
        return border_width

    def get_border_for_window(self, win):
        for rule, value in self.matches:
            if rule.compare(win):
                return value
        return self.default


def center_tray(win):
    """Keep the tray row centered on screen.

    stalonetray resizes its window as icons dock and undock, and the dropdown
    can only be placed at a fixed fraction of the screen, so the window is
    re-centered once it has been placed and its real size is known.
    """
    if not scratchpad_matches["stalonetray"].compare(win):
        return
    screen = (win.group.screen if win.group else None) or win.qtile.current_screen
    if screen is None:
        return
    x = int(screen.x + (screen.width - win.width) / 2)
    y = int(screen.y + (screen.height - win.height) / 2)
    if (win.x, win.y) == (x, y):
        return
    win._place(x, y, win.width, win.height, win.borderwidth, win.bordercolor, above=True)


def new_place(
    self,
    x,
    y,
    width,
    height,
    borderwidth,
    bordercolor,
    above=False,
    margin=None,
    respect_hints=False,
):
    if hasattr(borderwidth, "get_border_for_window"):
        old = getattr(self, "_old_bw", borderwidth.default)
        if not isinstance(old, int):
            old = borderwidth.default
        newborder = borderwidth.get_border_for_window(self)
        if newborder != old:
            width += old * 2
            width -= newborder * 2
            height += old * 2
            height -= newborder * 2
    else:
        newborder = borderwidth

    self._old_bw = newborder
    if hasattr(bordercolor, "get_border_for_window"):
        bordercolor = bordercolor.get_border_for_window(self)

    self._place(
        x,
        y,
        width,
        height,
        newborder,
        bordercolor,
        above=above,
        margin=margin,
        respect_hints=respect_hints,
    )
    center_tray(self)


@hook.subscribe.startup
def patch_window_place():
    from libqtile.backend.x11.window import _Window

    if not hasattr(_Window, "_place"):
        _Window._place = _Window.place
    _Window.place = new_place


def process_screens(self, reloading: bool = False):
    """Drop the screens qtile invented while it could see no output.

    A monitor that is still waking up at login leaves qtile without outputs
    for a moment, and it keeps itself a screen it never assigns a group to.
    Every later screen change and every config reload then dies reading that
    group, for as long as the session lasts. Nothing is ever shown on such a
    screen, so it is thrown away and the real outputs are laid out afresh.
    """
    live = [screen for screen in self.screens if hasattr(screen, "group")]
    if len(live) != len(self.screens):
        self.screens = live
        if not hasattr(getattr(self, "current_screen", None), "group"):
            del self.current_screen
    self._process_screens_unpatched(reloading)


@hook.subscribe.startup
def patch_process_screens():
    if not hasattr(Qtile, "_process_screens_unpatched"):
        Qtile._process_screens_unpatched = Qtile._process_screens
    Qtile._process_screens = process_screens


@hook.subscribe.startup_once
def startup():
    subprocess.Popen("startup")


previous_focused = []


@hook.subscribe.client_focus
def client_focused(window):
    if "urxvt_scratchpad" == window.name:
        window.border_width = 0

    global previous_focused
    if len(previous_focused) < 2:
        previous_focused.append(window)
    elif previous_focused[1] != window:
        previous_focused[0] = previous_focused[1]
        previous_focused[1] = window
    # logger.info(f"FOCUSED {window}, {previous_focused}")


@lazy.function
def focus_previous_window(qtile: Qtile):
    global previous_focused
    if len(previous_focused) == 2:
        group = previous_focused[0].group
        qtile.current_screen.set_group(group)
        # logger.info(f"FOCUS PREVIOUS {previous_focused[0]}")
        group.focus(previous_focused[0])


# @hook.subscribe.focus_change
# def focus_changed():
#     window = qtile.current_window
#     # send_notification("qtile", f"Focus changed.")
#     if "urxvt_scratchpad" == window.name:
#         window.border_width = 0
#         send_notification("qtile", "Focus changed.")


# @hook.subscribe.group_window_add
# def group_window_add(group, window):
#     if "urxvt_scratchpad" == window.name:
#         window.border_width = 0
#         send_notification("qtile", f"Window {window.name} added to {group.name}")


follow_mouse_focus = False
bring_front_click = "floating_only"
floats_kept_above = True
cursor_warp = False
floating_border_colors = ConditionalBorderColor(
    default="#FFB300",
    matches=[
        (scratchpad_matches["ioa"], "#F94144"),
        (scratchpad_matches["tdesktop"], "#70A288"),
        (scratchpad_matches["webchat"], "#2A9D8F"),
        (scratchpad_matches["chatgpt"], "#84A98C"),
        (scratchpad_matches["bookmarks"], "#4F6D7A"),
        (scratchpad_matches["stardict"], "#E83E8C"),
        (scratchpad_matches["stalonetray"], "#F2CC8F"),
        (Match(wm_class="Google-chrome", role="pop-up"), "#6A994E"),
        (Match(wm_class="copyq"), "#E9C46A"),
    ],
)

floating_layout = layout.Floating(
    border_width=ConditionalBorderWidth(
        default=8,
        matches=[
            (Match(title="urxvt_scratchpad"), 0),
            (Match(wm_class="flameshot"), 0),
            (Match(wm_class="kitty", title="float"), 0),
            (Match(wm_class="kitty", title="dtpick"), 0),
        ],
    ),
    border_focus=floating_border_colors,
    border_normal=floating_border_colors,
    float_rules=[
        MatchAny(*layout.Floating.default_float_rules)
        & ~Match(wm_class="xfreerdp")
        & ~Match(wm_class="mpv"),
        Match(wm_class="Google-chrome", role="pop-up"),
        Match(wm_class="copyq"),
        Match(wm_class="TelegramDesktop"),
        Match(wm_class="kitty", title="dtpick"),
        # Match(wm_class="wemeetapp"),
    ],
)
auto_fullscreen = False
focus_on_window_activation = "urgent"


reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = False

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
