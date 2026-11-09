#!/usr/bin/env python3

import os
import re
import time

NONCE_RE = re.compile(r"[0-9a-f]{32}\Z")
TEXT_FIELDS = {
    "_ABUSH_REMOTE_ID": "remote_key",
    "_ABUSH_WORKSPACE_ID": "workspace_id",
    "_ABUSH_WINDOW_GENERATION": "generation",
    "_ABUSH_CONTROL_STATE": "control_state",
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
        return list(self.core.QueryTree(self.root).reply().children)

    def title(self, window):
        return self.property(window, "_NET_WM_NAME") or self.property(window, "WM_NAME", "STRING")

    def wm_class(self, window):
        return self.property(window, "WM_CLASS", "STRING").split("\0")

    def set_text(self, window, name, value, property_type="UTF8_STRING"):
        encoded = str(value).encode()
        self.core.ChangeProperty(self.xproto.PropMode.Replace, window, self.atom(name),
                                 self.atom(property_type), 8, len(encoded), encoded)

    def finish(self):
        self.connection.flush()


def bind_workspace_window(payload, adapter=None, attempts=20, interval=0.05):
    required = {"nonce", "remote_key", "workspace_id", "generation", "control_state"}
    if not isinstance(payload, dict) or not required.issubset(payload):
        raise ValueError("missing X11 binding metadata")
    nonce = payload["nonce"]
    if not isinstance(nonce, str) or not NONCE_RE.fullmatch(nonce):
        raise ValueError("invalid binding nonce")
    for key in required - {"nonce", "generation"}:
        if not isinstance(payload[key], str) or not payload[key] or len(payload[key]) > 256:
            raise ValueError(f"invalid {key}")
    if not isinstance(payload["generation"], int) or isinstance(payload["generation"], bool):
        raise ValueError("invalid generation")
    adapter = adapter or XcffibAdapter()
    expected = f"__ABUSH_BIND__:{nonce}"
    matches = []
    for attempt in range(attempts):
        matches = [window for window in adapter.windows()
                   if adapter.title(window) == expected and
                   any("chrom" in value.lower() for value in adapter.wm_class(window))]
        if len(matches) == 1:
            break
        if len(matches) > 1:
            raise ValueError("ambiguous X11 binding nonce")
        if attempt + 1 < attempts:
            time.sleep(interval)
    if len(matches) != 1:
        raise X11Unavailable("matching Chrome X11 window was not found")
    window = matches[0]
    adapter.set_text(window, "WM_WINDOW_ROLE", "abush-browser", "STRING")
    for prop, field in TEXT_FIELDS.items():
        adapter.set_text(window, prop, payload[field])
    observed = {prop: adapter.property(window, prop) for prop in TEXT_FIELDS}
    observed["WM_WINDOW_ROLE"] = adapter.property(window, "WM_WINDOW_ROLE", "STRING")
    try:
        adapter.set_text(window, "WM_CLASS", "abush-browser\0AbushBrowser\0", "STRING")
        adapter.set_text(window, "_NET_WM_NAME", f"Abush Browser — {payload['workspace_id']}")
    except Exception:
        pass
    adapter.finish()
    return {"available": True, "xid": window, "observed": observed}
