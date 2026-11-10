#!/usr/bin/env python3

import hashlib
import json
import logging
import os
import pathlib
import queue
import re
import socket
import stat
import struct
import subprocess
import sys
import threading
import time
import urllib.request
import uuid
from urllib.parse import urlsplit

try:
    from websockets.exceptions import WebSocketException
    from websockets.sync.client import connect
except ImportError:
    class WebSocketException(Exception):
        pass

    def connect(*_args, **_kwargs):
        raise WebSocketException("websockets package is unavailable")

MAX_MESSAGE_SIZE = 1024 * 1024
NATIVE_ENVELOPE_VERSION = 1
PEER_PROTOCOL_VERSION = 2
REMOTE_KEY_RE = re.compile(r"r_[A-Za-z0-9]{8,64}\Z")
CONNECTION_ID_RE = re.compile(r"c_[A-Za-z0-9]{8,64}\Z")
REMOTE_SOCKET_SUFFIX = "/abush/browser-peer.sock"
SCRIPTS = pathlib.Path.home() / "scripts"
ROUTES_PATH = pathlib.Path(__file__).with_name("routes.json")
ROUTES = json.loads(ROUTES_PATH.read_text())
DEBUG_PORTS = {
    9222: "google-chrome-main",
    9223: "chrome-chatgpt",
    9224: "chrome-chat",
}
LOG = pathlib.Path.home() / ".cache/browser-router.log"
logging.basicConfig(
    filename=LOG,
    level=logging.INFO,
    format="%(asctime)s %(message)s",
)


def group_for_hostname(hostname):
    if not hostname:
        return None
    for name, group in ROUTES["groups"].items():
        if hostname in group["sites"] or any(
            hostname.endswith(suffix) for suffix in group["suffixes"]
        ):
            return name
    return None


def reuse_tab(group, hostname, url):
    try:
        pages = json.load(
            urllib.request.urlopen(
                f"http://127.0.0.1:{ROUTES['groups'][group]['debug_port']}/json",
                timeout=1,
            )
        )
        page = next(
            page
            for page in pages
            if page.get("type") == "page" and urlsplit(page.get("url", "")).hostname == hostname
        )
        with connect(page["webSocketDebuggerUrl"]) as websocket:
            websocket.send(
                json.dumps(
                    {
                        "id": 1,
                        "method": "Page.navigate",
                        "params": {"url": url},
                    }
                )
            )
            websocket.recv()
        urllib.request.urlopen(
            f"http://127.0.0.1:{ROUTES['groups'][group]['debug_port']}/json/activate/{page['id']}",
            timeout=1,
        ).read()
        subprocess.run(
            [
                "qtile",
                "cmd-obj",
                "-o",
                "root",
                "-f",
                "eval",
                "-a",
                f'self.show_scratchpad("{ROUTES["groups"][group]["scratchpad"]}")',
            ],
            check=False,
        )
        return True
    except (OSError, StopIteration, KeyError, ValueError) as error:
        logging.info("CDP fallback group=%s host=%s error=%s", group, hostname, error)
        return False


def evaluate(websocket, expression, timeout=1):
    websocket.send(json.dumps({"id": 1, "method": "Runtime.evaluate", "params": {
        "expression": expression,
        "returnByValue": True,
    }}))
    return json.loads(websocket.recv(timeout=timeout))


def receive_response(websocket, response_id):
    while True:
        response = json.loads(websocket.recv(timeout=1))
        if response.get("id") == response_id:
            return response


def execute(websocket, expression, response_id=4):
    websocket.send(json.dumps({
        "id": response_id,
        "method": "Runtime.evaluate",
        "params": {"expression": expression, "returnByValue": True},
    }))
    return receive_response(websocket, response_id)


def scroll_fallback(websocket, direction, response_id=6):
    expression = """(() => {
        const direction = __DIRECTION__;
        const root = document.scrollingElement;
        let active = document.activeElement;
        while (active?.shadowRoot?.activeElement) active = active.shadowRoot.activeElement;
        const hovered = [...document.querySelectorAll(':hover')].at(-1);

        function parent(element) {
            return element?.assignedSlot || element?.parentElement ||
                element?.getRootNode?.().host;
        }

        function editable(element) {
            if (document.designMode === 'on') return true;
            for (; element; element = parent(element)) {
                if (element.isContentEditable ||
                        /^(INPUT|TEXTAREA|SELECT)$/.test(element.tagName)) return true;
            }
            return false;
        }

        function maxScroll(element) {
            return Math.max(0, element.scrollHeight - element.clientHeight);
        }

        function scrollable(element) {
            if (!element || maxScroll(element) <= 1) return false;
            if (element === root) {
                const rootOverflow = getComputedStyle(document.documentElement).overflowY;
                const bodyOverflow = getComputedStyle(document.body).overflowY;
                return !/(hidden|clip)/.test(rootOverflow) &&
                    !/(hidden|clip)/.test(bodyOverflow);
            }
            const style = getComputedStyle(element);
            return /(auto|scroll|overlay)/.test(style.overflowY);
        }

        function atBoundary(element) {
            return direction === 'scrollToTop' ? element.scrollTop <= 1 :
                element.scrollTop >= maxScroll(element) - 1;
        }

        function nearest(element, source) {
            for (; element; element = parent(element)) {
                if (scrollable(element) && !atBoundary(element)) return {element, source};
            }
            return null;
        }

        function descendants(scope) {
            const result = [];
            for (const element of scope.querySelectorAll('*')) {
                result.push(element);
                if (element.shadowRoot) result.push(...descendants(element.shadowRoot));
            }
            return result;
        }

        function visibleArea(element) {
            if (element === root) return innerWidth * innerHeight;
            const style = getComputedStyle(element);
            if (style.display === 'none' || style.visibility === 'hidden') return 0;
            let rect = element.getBoundingClientRect();
            let left = Math.max(0, rect.left);
            let top = Math.max(0, rect.top);
            let right = Math.min(innerWidth, rect.right);
            let bottom = Math.min(innerHeight, rect.bottom);
            for (let ancestor = parent(element); ancestor; ancestor = parent(ancestor)) {
                const overflow = getComputedStyle(ancestor);
                if (/(hidden|clip|auto|scroll|overlay)/.test(overflow.overflowY) ||
                        /(hidden|clip|auto|scroll|overlay)/.test(overflow.overflowX)) {
                    rect = ancestor.getBoundingClientRect();
                    left = Math.max(left, rect.left);
                    top = Math.max(top, rect.top);
                    right = Math.min(right, rect.right);
                    bottom = Math.min(bottom, rect.bottom);
                }
            }
            return Math.max(0, right - left) * Math.max(0, bottom - top);
        }

        const activeFrame = active?.tagName === 'IFRAME' || active?.tagName === 'FRAME';
        if (activeFrame || hovered?.closest?.('iframe,frame')) {
            return {handled: false, moved: false, atBoundary: false, source: 'frame'};
        }
        if (editable(active)) {
            return {
                handled: true,
                moved: false,
                atBoundary: false,
                source: 'editable',
            };
        }

        let selected = nearest(hovered, 'hover');
        if (selected && visibleArea(selected.element) <= 1) selected = null;
        selected ||= nearest(active, 'active');
        if (selected && visibleArea(selected.element) <= 1) selected = null;
        if (!selected) {
            const candidates = [...new Set([root, ...descendants(document)])]
                .filter(scrollable)
                .filter(element => !atBoundary(element))
                .filter(element => visibleArea(element) > 1)
                .sort((left, right) => visibleArea(right) - visibleArea(left) ||
                    maxScroll(right) - maxScroll(left));
            if (candidates.length) selected = {element: candidates[0], source: 'visible'};
        }
        if (!selected) {
            return {handled: false, moved: false, atBoundary: false, source: 'none'};
        }

        const target = selected.element;
        const before = target.scrollTop;
        const max = maxScroll(target);
        const requested = direction === 'scrollToTop' ? 0 : max;
        if (target === root) {
            window.scrollTo({left: target.scrollLeft, top: requested, behavior: 'instant'});
        } else {
            target.scrollTo({left: target.scrollLeft, top: requested, behavior: 'instant'});
        }
        const after = target.scrollTop;
        const rect = target === root ? {left: 0, top: 0, width: innerWidth,
            height: innerHeight} : target.getBoundingClientRect();
        return {
            handled: true,
            moved: Math.abs(after - before) > 1,
            atBoundary: direction === 'scrollToTop' ? after <= 1 : after >= max - 1,
            source: selected.source,
            tag: target.tagName,
            id: target.id,
            class: String(target.className).slice(0, 120),
            before,
            after,
            max,
            rect: [rect.left, rect.top, rect.width, rect.height],
        };
    })()""".replace("__DIRECTION__", json.dumps(direction))
    response = execute(websocket, expression, response_id)
    result = response.get("result", {}).get("result", {})
    value = result.get("value")
    if isinstance(value, dict):
        return value
    return {
        "handled": False,
        "moved": False,
        "atBoundary": False,
        "error": result.get("description") or response.get("error") or response,
    }


def scroll_page(direction, url=None):
    started = time.monotonic()
    command = {
        "scrollToTop": "ScrollToBeginningOfDocument",
        "scrollToBottom": "ScrollToEndOfDocument",
    }.get(direction)
    if not command:
        logging.info("scroll invalid direction=%s", direction)
        return False

    candidates = []
    for port in DEBUG_PORTS:
        try:
            pages = json.load(
                urllib.request.urlopen(f"http://127.0.0.1:{port}/json", timeout=1)
            )
        except (OSError, ValueError):
            continue
        for page in pages:
            websocket_url = page.get("webSocketDebuggerUrl", "")
            if page.get("type") == "page" and page.get("url") == url and \
                    websocket_url.startswith(f"ws://127.0.0.1:{port}/"):
                candidates.append(websocket_url)
    if len(candidates) > 1:
        focused = []
        for websocket_url in candidates:
            try:
                with connect(websocket_url, open_timeout=0.2, close_timeout=0.2) as websocket:
                    response = evaluate(websocket, "document.hasFocus()", timeout=0.2)
                if response.get("result", {}).get("result", {}).get("value") is True:
                    focused.append(websocket_url)
            except (OSError, KeyError, ValueError, WebSocketException):
                continue
        candidates = focused
    logging.info(
        "scroll request direction=%s url=%s candidates=%d scan_ms=%d",
        direction,
        url,
        len(candidates),
        (time.monotonic() - started) * 1000,
    )
    if len(candidates) != 1:
        return False
    websocket_url = candidates[0]
    try:
        with connect(websocket_url, open_timeout=1, close_timeout=1) as websocket:
            key = {
                "key": "F20",
                "code": "F20",
                "windowsVirtualKeyCode": 131,
            }
            fallback = scroll_fallback(websocket, direction, response_id=4)
            if fallback.get("source") == "editable":
                logging.info("scroll ignored direction=%s target=%s", direction, fallback)
                return True
            if fallback.get("handled"):
                result = fallback.get("moved") or fallback.get("atBoundary")
                logging.info(
                    "scroll result direction=%s ok=%s total_ms=%d fallback=%s",
                    direction,
                    result,
                    (time.monotonic() - started) * 1000,
                    fallback,
                )
                return result
            key_down = None
            try:
                websocket.send(json.dumps({
                    "id": 2,
                    "method": "Input.dispatchKeyEvent",
                    "params": {
                        **key,
                        "type": "rawKeyDown",
                        "commands": [command],
                    },
                }))
                key_down = receive_response(websocket, 2)
            finally:
                websocket.send(json.dumps({
                    "id": 3,
                    "method": "Input.dispatchKeyEvent",
                    "params": {**key, "type": "keyUp"},
                }))
                key_up = receive_response(websocket, 3)
            native_ok = (
                key_down is not None and "error" not in key_down and "error" not in key_up
            )
            result = native_ok
            logging.info(
                "scroll result direction=%s ok=%s total_ms=%d "
                "key_down=%s key_up=%s fallback=%s",
                direction,
                result,
                (time.monotonic() - started) * 1000,
                key_down,
                key_up,
                fallback,
            )
            return result
    except (OSError, KeyError, ValueError, WebSocketException) as error:
        logging.info("scroll CDP failure direction=%s url=%s error=%s", direction, url, error)
        return False


def read_message(stream=None):
    stream = stream or sys.stdin.buffer
    header = stream.read(4)
    if not header:
        return None
    if len(header) != 4:
        raise EOFError
    size = struct.unpack("<I", header)[0]
    if size > MAX_MESSAGE_SIZE:
        raise ValueError("native message is too large")
    payload = stream.read(size)
    if len(payload) != size:
        raise EOFError
    return json.loads(payload)


def send_message(message, stream=None):
    stream = stream or sys.stdout.buffer
    payload = json.dumps(message, separators=(",", ":")).encode()
    if len(payload) > MAX_MESSAGE_SIZE:
        raise ValueError("native message is too large")
    stream.write(struct.pack("<I", len(payload)) + payload)
    stream.flush()


def handle_message(message):
    if message.get("command") == "scroll":
        try:
            return {"ok": scroll_page(
                message.get("direction"),
                url=message.get("url"),
            )}
        except (OSError, KeyError, ValueError, WebSocketException) as error:
            logging.info("scroll failed error=%s", error)
            return {"ok": False}

    url = message.get("url", "")
    parsed = urlsplit(url)
    if parsed.scheme not in {"http", "https"}:
        return {"ok": False}
    hostname = parsed.hostname
    source_hostname = urlsplit(message.get("sourceUrl", "")).hostname
    if hostname in ROUTES["auth_sites"] or source_hostname in ROUTES["auth_sites"]:
        return {"ok": True}
    group = group_for_hostname(hostname)
    launcher = ROUTES["groups"].get(group, {}).get("launcher", "chromium")
    if group and reuse_tab(group, hostname, url):
        logging.info("reused group=%s url=%s", group, url)
        return {"ok": True}
    logging.info("launching launcher=%s url=%s", launcher, url)
    subprocess.Popen([str(SCRIPTS / launcher), url])
    return {"ok": True}


class EnvelopeError(ValueError):
    pass


def validate_request(message):
    if not isinstance(message, dict):
        raise EnvelopeError("request must be an object")
    required = {"version", "id", "service", "operation", "payload"}
    if set(message) != required:
        raise EnvelopeError("request has unexpected fields")
    if message["version"] != NATIVE_ENVELOPE_VERSION:
        raise EnvelopeError("unsupported envelope version")
    if not isinstance(message["id"], str) or not message["id"] or len(message["id"]) > 128:
        raise EnvelopeError("invalid request id")
    if not isinstance(message["service"], str) or not message["service"]:
        raise EnvelopeError("invalid service")
    if not isinstance(message["operation"], str) or not message["operation"]:
        raise EnvelopeError("invalid operation")
    if not isinstance(message["payload"], dict):
        raise EnvelopeError("payload must be an object")
    return message


def response(reply_to, payload=None, error=None):
    message = {
        "version": NATIVE_ENVELOPE_VERSION,
        "reply_to": reply_to,
        "ok": error is None,
    }
    if error is None:
        message["payload"] = payload if payload is not None else {}
    else:
        message["error"] = str(error)
    return message


def event(name, service, payload):
    return {
        "version": NATIVE_ENVELOPE_VERSION,
        "event": name,
        "service": service,
        "payload": payload,
    }


def _secure_stat(path, kind, mode, uid):
    info = os.lstat(path)
    expected = stat.S_ISDIR if kind == "directory" else stat.S_ISREG
    if stat.S_ISLNK(info.st_mode) or not expected(info.st_mode):
        raise ValueError(f"not a real {kind}")
    if info.st_uid != uid or stat.S_IMODE(info.st_mode) != mode:
        raise ValueError(f"unsafe {kind} ownership or mode")
    return info


def validate_remote_metadata(path, root=None, uid=None):
    path = pathlib.Path(path)
    root = pathlib.Path(root) if root else path.parents[2]
    uid = os.getuid() if uid is None else uid
    root = root.absolute()
    if path.name != "connection.json" or len(path.parents) < 3:
        raise ValueError("unexpected metadata path")
    connection_dir = path.parent
    remote_dir = connection_dir.parent
    if remote_dir.parent != root:
        raise ValueError("metadata is outside browser remotes root")
    remote_key = remote_dir.name
    connection_id = connection_dir.name
    if not REMOTE_KEY_RE.fullmatch(remote_key) or not CONNECTION_ID_RE.fullmatch(connection_id):
        raise ValueError("unsafe remote path component")
    _secure_stat(root, "directory", 0o700, uid)
    _secure_stat(remote_dir, "directory", 0o700, uid)
    _secure_stat(connection_dir, "directory", 0o700, uid)
    info = _secure_stat(path, "file", 0o600, uid)
    with path.open(encoding="utf-8") as source:
        metadata = json.load(source)
    if metadata.get("schema_version") != 1:
        raise ValueError("unsupported metadata schema")
    if metadata.get("remote_key") != remote_key or metadata.get("connection_id") != connection_id:
        raise ValueError("metadata path identity mismatch")
    browser_socket = pathlib.Path(metadata.get("browser_socket", ""))
    if not browser_socket.is_absolute() or browser_socket.parent != connection_dir.absolute():
        raise ValueError("browser socket escapes connection directory")
    if browser_socket.name != "browser-peer.sock":
        raise ValueError("unexpected browser socket name")
    remote_socket = metadata.get("remote_browser_socket")
    if not isinstance(remote_socket, str) or not remote_socket.endswith(REMOTE_SOCKET_SUFFIX):
        raise ValueError("unexpected remote browser socket")
    display_name = metadata.get("display_name")
    if not isinstance(display_name, str) or len(display_name) > 256:
        raise ValueError("invalid display name")
    return {
        "remote_key": remote_key,
        "connection_id": connection_id,
        "display_name": display_name,
        "browser_socket": str(browser_socket),
        "metadata_mtime_ns": info.st_mtime_ns,
    }


def discover_remotes(root=None):
    if root is None:
        runtime = os.environ.get("XDG_RUNTIME_DIR")
        if not runtime:
            return []
        root = pathlib.Path(runtime) / "abush" / "browser-remotes"
    root = pathlib.Path(root)
    try:
        entries = []
        for remote_dir in os.scandir(root):
            if remote_dir.is_symlink() or not remote_dir.is_dir(follow_symlinks=False):
                continue
            for connection_dir in os.scandir(remote_dir.path):
                if connection_dir.is_symlink() or not connection_dir.is_dir(follow_symlinks=False):
                    continue
                path = pathlib.Path(connection_dir.path) / "connection.json"
                try:
                    entries.append(validate_remote_metadata(path, root))
                except (OSError, ValueError, json.JSONDecodeError) as error:
                    logging.info("ignored browser remote metadata path=%s error=%s", path, error)
        return entries
    except (FileNotFoundError, NotADirectoryError, PermissionError, ValueError) as error:
        logging.info("browser remote discovery unavailable error=%s", error)
        return []


def encode_peer_message(message):
    payload = json.dumps(message, separators=(",", ":")).encode()
    if len(payload) > MAX_MESSAGE_SIZE:
        raise ValueError("peer message is too large")
    return struct.pack(">I", len(payload)) + payload


def read_exact(sock, size):
    chunks = []
    remaining = size
    while remaining:
        chunk = sock.recv(remaining)
        if not chunk:
            raise EOFError
        chunks.append(chunk)
        remaining -= len(chunk)
    return b"".join(chunks)


def read_peer_message(sock):
    size = struct.unpack(">I", read_exact(sock, 4))[0]
    if size > MAX_MESSAGE_SIZE:
        raise ValueError("peer message is too large")
    message = json.loads(read_exact(sock, size))
    if not isinstance(message, dict):
        raise ValueError("peer message must be an object")
    return message


class RemotePeer:
    def __init__(self, broker, metadata):
        self.broker = broker
        self.metadata = metadata
        self.socket = None
        self.write_lock = threading.Lock()
        self.stop_event = threading.Event()
        self.connected = False
        self.principal_id = None
        self.executor_fence = None
        self.default_window = None
        self.capabilities = {}
        self.pending = {}
        self.topology_pending = {}
        self.reader = None

    @property
    def key(self):
        return (self.metadata["remote_key"], self.metadata["connection_id"])

    def logical_state(self):
        state = {
            "remote_key": self.metadata["remote_key"],
            "connection_id": self.metadata["connection_id"],
            "display_name": self.metadata["display_name"],
            "connected": self.connected,
        }
        if self.principal_id is not None:
            state["principal_id"] = self.principal_id
        if self.default_window is not None:
            state["default_window"] = self.default_window
        return state

    def connect(self, timeout=1.0):
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        try:
            socket_info = os.lstat(self.metadata["browser_socket"])
            if stat.S_ISLNK(socket_info.st_mode) or not stat.S_ISSOCK(socket_info.st_mode) or \
                    socket_info.st_uid != os.getuid():
                raise ValueError("unsafe browser socket")
            sock.connect(self.metadata["browser_socket"])
            nonce = uuid.uuid4().hex
            self.socket = sock
            self.send({
                "type": "hello",
                "version": PEER_PROTOCOL_VERSION,
                "executor_id": self.broker.executor_id(self.metadata),
                "nonce": nonce,
                "capabilities": {"cancellation": True},
            })
            accepted = read_peer_message(sock)
            self._accept_hello(accepted)
            sock.settimeout(0.5)
            self.connected = True
            return True
        except (OSError, EOFError, ValueError, json.JSONDecodeError) as error:
            logging.info("browser peer connect failed remote=%s connection=%s error=%s",
                         self.metadata["remote_key"], self.metadata["connection_id"], error)
            self.close(emit=False)
            return False

    def activate(self):
        if not self.connected or self.reader is not None:
            return
        self.reader = threading.Thread(target=self._read_loop, daemon=True)
        self.reader.start()

    @staticmethod
    def _exact_fields(value, fields):
        return isinstance(value, dict) and set(value) == set(fields)

    def _accept_hello(self, message):
        if not self._exact_fields(message, {
            "type", "request_id", "principal_id", "executor_fence", "default_window"
        }) or message.get("type") != "hello_accepted":
            raise ValueError("peer rejected hello")
        request_id = message.get("request_id")
        principal_id = message.get("principal_id")
        fence = message.get("executor_fence")
        default_window = message.get("default_window")
        if not isinstance(request_id, str) or not request_id:
            raise ValueError("invalid hello request id")
        if not isinstance(principal_id, str) or not principal_id:
            raise ValueError("invalid principal id")
        if not self._valid_executor_fence(fence):
            raise ValueError("invalid executor fence")
        if not self._valid_window_ref(default_window):
            raise ValueError("invalid default window")
        self.principal_id = principal_id
        self.executor_fence = fence
        self.default_window = default_window

    @staticmethod
    def _valid_executor_fence(value):
        return isinstance(value, dict) and set(value) == {"executor_id", "epoch"} and \
            isinstance(value["executor_id"], str) and value["executor_id"] and \
            isinstance(value["epoch"], int) and not isinstance(value["epoch"], bool) and \
            value["epoch"] > 0

    @staticmethod
    def _valid_window_ref(value):
        return isinstance(value, dict) and set(value) == {
            "workspace_id", "window_id", "generation"
        } and all(isinstance(value[key], str) and value[key]
                  for key in ("workspace_id", "window_id")) and \
            isinstance(value["generation"], int) and not isinstance(value["generation"], bool)

    @classmethod
    def _valid_page_ref(cls, value):
        return isinstance(value, dict) and set(value) == {"window", "page_id", "generation"} and \
            cls._valid_window_ref(value["window"]) and \
            isinstance(value["page_id"], str) and value["page_id"] and \
            isinstance(value["generation"], int) and not isinstance(value["generation"], bool)

    def _handle_topology_response(self, message):
        if not self._exact_fields(message, {"type", "request_id", "result"}):
            raise ValueError("invalid topology response")
        request_id = message.get("request_id")
        pending = self.topology_pending.get(request_id)
        if pending is None:
            raise ValueError("uncorrelated topology response")
        result = message.get("result")
        if not isinstance(result, dict):
            raise ValueError("invalid topology response")
        outcome = result.get("outcome")
        expected = {
            "page_created": {"outcome", "page"},
            "page_moved_outside": {"outcome"},
            "rejected": {"outcome", "code"},
        }.get(outcome)
        if expected is None or set(result) != expected or \
                outcome == "page_created" and not self._valid_page_ref(result.get("page")) or \
                outcome == "rejected" and not isinstance(result.get("code"), str):
            raise ValueError("invalid topology response")
        pending["result"] = result
        pending["event"].set()

    def topology_request(self, message, timeout=3.0):
        request_id = message["request_id"]
        if request_id in self.topology_pending:
            raise ValueError("duplicate topology request")
        pending = {"event": threading.Event()}
        self.topology_pending[request_id] = pending
        try:
            self.send(message)
            if not pending["event"].wait(timeout):
                raise TimeoutError("topology response timed out")
            return pending["result"]
        finally:
            self.topology_pending.pop(request_id, None)

    def register_page(self, window, page_id):
        if window != self.default_window or not isinstance(page_id, str) or not page_id:
            raise ValueError("invalid managed page registration")
        result = self.topology_request({
            "type": "managed_page_created",
            "request_id": uuid.uuid4().hex,
            "executor_fence": self.executor_fence,
            "window": window,
            "page_id": page_id,
        })
        if result.get("outcome") != "page_created" or not self._valid_page_ref(result.get("page")):
            raise ValueError(f"page registration rejected: {result.get('outcome')}")
        return result["page"]

    def moved_outside(self, page):
        if not self._valid_page_ref(page):
            raise ValueError("invalid page ref")
        result = self.topology_request({
            "type": "managed_page_moved_outside",
            "request_id": uuid.uuid4().hex,
            "executor_fence": self.executor_fence,
            "page": page,
        })
        if result.get("outcome") != "page_moved_outside":
            raise ValueError(f"move rejected: {result.get('outcome')}")

    def complete(self, request_id, effect_id, fence, completion):
        if fence != self.executor_fence or completion not in {"applied", "not_applied", "unknown"}:
            raise ValueError("invalid executor completion")
        pending = self.pending.pop(effect_id, None)
        if pending is None or pending["request_id"] != request_id:
            raise ValueError("uncorrelated executor completion")
        self.send({
            "type": "completion",
            "request_id": request_id,
            "effect_id": effect_id,
            "executor_fence": fence,
            "completion": completion,
        })

    def send(self, message):
        data = encode_peer_message(message)
        with self.write_lock:
            if self.socket is None:
                raise OSError("peer is closed")
            self.socket.sendall(data)

    def _read_loop(self):
        try:
            while not self.stop_event.is_set():
                try:
                    message = read_peer_message(self.socket)
                except socket.timeout:
                    continue
                self.handle(message)
        except (OSError, EOFError, ValueError, json.JSONDecodeError) as error:
            if not self.stop_event.is_set():
                logging.info("browser peer closed remote=%s connection=%s error=%s",
                             self.metadata["remote_key"], self.metadata["connection_id"], error)
        finally:
            self.close(emit=True, from_reader=True)

    def handle(self, message):
        message_type = message.get("type")
        normalized_type = message_type.lower() if isinstance(message_type, str) else None
        if normalized_type == "ping":
            if not self._exact_fields(message, {"type", "request_id", "ping_id"}):
                raise ValueError("invalid ping")
            request_id = message.get("request_id")
            if not isinstance(request_id, str) or not request_id:
                raise ValueError("invalid ping")
            pong = {"type": "pong", "request_id": request_id}
            ping_id = message.get("ping_id")
            if not isinstance(ping_id, str) or not ping_id:
                raise ValueError("invalid ping id")
            pong["ping_id"] = ping_id
            self.send(pong)
            return
        if normalized_type == "execute":
            self._handle_execute(message)
            return
        if normalized_type == "cancel":
            self._handle_cancel(message)
            return
        if normalized_type == "topology_response":
            self._handle_topology_response(message)
            return
        raise ValueError("unknown peer message")

    @classmethod
    def _valid_command(cls, command):
        if not isinstance(command, dict):
            return False
        kind = command.get("command")
        if kind == "navigate":
            return set(command) == {"command", "page", "url"} and \
                cls._valid_page_ref(command["page"]) and \
                isinstance(command["url"], str) and command["url"]
        if kind == "close_page":
            return set(command) == {"command", "page"} and cls._valid_page_ref(command["page"])
        if kind == "close_window":
            return set(command) == {"command", "window"} and \
                cls._valid_window_ref(command["window"])
        return False

    def _correlation(self, message, expected_fields):
        if not self._exact_fields(message, expected_fields):
            raise ValueError("invalid peer correlation")
        request_id = message.get("request_id")
        effect_id = message.get("effect_id")
        fence = message.get("executor_fence")
        if not isinstance(request_id, str) or not request_id or \
                not isinstance(effect_id, str) or not effect_id or \
                not self._valid_executor_fence(fence):
            raise ValueError("invalid peer correlation")
        if fence != self.executor_fence:
            raise ValueError("stale executor fence")
        return request_id, effect_id, fence

    def _handle_execute(self, message):
        request_id, effect_id, fence = self._correlation(message, {
            "type", "request_id", "effect_id", "executor_fence", "command"
        })
        command = message.get("command")
        if not self._valid_command(command):
            raise ValueError("invalid browser command")
        if effect_id in self.pending:
            raise ValueError("duplicate execute effect")
        self.pending[effect_id] = {"request_id": request_id, "fence": fence}
        self.broker.outbound.put(event("abush_browser.execute", "abush_browser", {
            "remote_key": self.metadata["remote_key"],
            "connection_id": self.metadata["connection_id"],
            "request_id": request_id,
            "effect_id": effect_id,
            "executor_fence": fence,
            "command": command,
        }))

    def _handle_cancel(self, message):
        request_id, effect_id, fence = self._correlation(message, {
            "type", "request_id", "effect_id", "executor_fence"
        })
        if effect_id not in self.pending:
            self.send({
                "type": "completion",
                "request_id": request_id,
                "effect_id": effect_id,
                "executor_fence": fence,
                "completion": "unknown",
            })
            return
        self.broker.outbound.put(event("abush_browser.cancel", "abush_browser", {
            "remote_key": self.metadata["remote_key"],
            "connection_id": self.metadata["connection_id"],
            "request_id": request_id,
            "effect_id": effect_id,
            "executor_fence": fence,
        }))

    def close(self, emit=False, from_reader=False):
        was_connected = self.connected
        self.connected = False
        self.stop_event.set()
        sock, self.socket = self.socket, None
        if sock is not None:
            try:
                sock.shutdown(socket.SHUT_RDWR)
            except OSError:
                pass
            sock.close()
        if self.reader and not from_reader and self.reader is not threading.current_thread():
            self.reader.join(timeout=1)
        if emit and was_connected:
            self.broker.remote_disconnected(self)


class BrowserBroker:
    def __init__(self, remote_root=None, outbound=None):
        self.remote_root = pathlib.Path(remote_root) if remote_root else None
        self.outbound = outbound or queue.Queue()
        self.peers = {}
        self.candidates = {}
        self.lock = threading.RLock()
        install = f"{pathlib.Path(__file__).resolve()}:{os.getuid()}".encode()
        self.install_id = hashlib.sha256(install).hexdigest()[:24]

    def executor_id(self, metadata):
        return f"browser-router-{self.install_id}-{metadata['remote_key']}"

    def remote_changed(self, peer):
        self.outbound.put(event(
            "abush_browser.remote_state", "abush_browser", peer.logical_state()
        ))

    def remote_disconnected(self, peer):
        with self.lock:
            if self.peers.get(peer.metadata["remote_key"]) is not peer:
                return
            remote_key = peer.metadata["remote_key"]
            self.peers.pop(remote_key, None)
            replacement = self._select(remote_key, exclude={peer.key}, publish=False)
            self.remote_changed(replacement or peer)

    def _select_candidate(self, remote_key, exclude=frozenset()):
        for metadata in self.candidates.get(remote_key, []):
            key = (remote_key, metadata["connection_id"])
            if key in exclude:
                continue
            peer = RemotePeer(self, metadata)
            logging.info("browser peer candidate selected remote=%s connection=%s",
                         remote_key, metadata["connection_id"])
            if peer.connect():
                return peer
            logging.info("browser peer candidate failed remote=%s connection=%s",
                         remote_key, metadata["connection_id"])
        return None

    def _select(self, remote_key, exclude=frozenset(), publish=True):
        current = self.peers.get(remote_key)
        if current and current.connected:
            return current
        peer = self._select_candidate(remote_key, exclude)
        if peer:
            self.peers[remote_key] = peer
            peer.activate()
            if publish:
                self.remote_changed(peer)
        return peer

    def refresh(self):
        grouped = {}
        for item in discover_remotes(self.remote_root):
            grouped.setdefault(item["remote_key"], []).append(item)
        for values in grouped.values():
            values.sort(key=lambda item: (-item["metadata_mtime_ns"], item["connection_id"]))
        with self.lock:
            self.candidates = grouped
            for remote_key, peer in list(self.peers.items()):
                keys = {(item["remote_key"], item["connection_id"])
                        for item in grouped.get(remote_key, [])}
                if peer.key not in keys:
                    logging.info("browser peer active candidate disappeared remote=%s connection=%s",
                                 remote_key, peer.metadata["connection_id"])
                    replacement = self._select_candidate(remote_key, exclude={peer.key})
                    if replacement:
                        self.peers[remote_key] = replacement
                        replacement.activate()
                        peer.close(emit=False)
                        self.remote_changed(replacement)
                    else:
                        self.peers.pop(remote_key, None)
                        peer.close(emit=False)
                        self.remote_changed(peer)
            for remote_key in sorted(grouped):
                self._select(remote_key)
        return self.list_remotes()

    def list_remotes(self):
        with self.lock:
            states = []
            for remote_key in sorted(self.candidates):
                peer = self.peers.get(remote_key)
                if peer:
                    states.append(peer.logical_state())
                else:
                    newest = self.candidates[remote_key][0]
                    states.append({
                        "remote_key": remote_key,
                        "connection_id": newest["connection_id"],
                        "display_name": newest["display_name"],
                        "connected": False,
                    })
            return states

    def close(self):
        with self.lock:
            peers = list(self.peers.values())
            self.peers.clear()
            self.candidates.clear()
        for peer in peers:
            peer.close()

    def peer(self, payload):
        remote_key = payload.get("remote_key")
        connection_id = payload.get("connection_id")
        with self.lock:
            peer = self.peers.get(remote_key)
        if peer is None or not peer.connected or peer.metadata["connection_id"] != connection_id:
            raise ValueError("remote is unavailable")
        return peer

    def handle_request(self, message):
        request = validate_request(message)
        service = request["service"]
        operation = request["operation"]
        payload = request["payload"]
        if service == "external_links" and operation == "open":
            result = handle_message(payload)
            if not result.get("ok"):
                raise ValueError("failed to open external link")
            return {}
        if service == "external_links" and operation == "scroll":
            result = handle_message({"command": "scroll", **payload})
            if not result.get("ok"):
                raise ValueError("failed to scroll page")
            return {}
        if service == "abush_browser" and operation == "list_remotes":
            return {"remotes": self.list_remotes()}
        if service == "abush_browser" and operation == "refresh_remotes":
            return {"remotes": self.refresh()}
        if service == "abush_browser" and operation in {"open_workspace", "get_workspace_bootstrap"}:
            peer = self.peer(payload)
            return {
                **peer.logical_state(),
                "executor_fence": peer.executor_fence,
                "default_window": peer.default_window,
            }
        if service == "abush_browser" and operation == "register_managed_page":
            peer = self.peer(payload)
            return {"page": peer.register_page(payload.get("window"), payload.get("page_id"))}
        if service == "abush_browser" and operation == "moved_outside":
            self.peer(payload).moved_outside(payload.get("page"))
            return {}
        if service == "abush_browser" and operation == "executor_completion":
            self.peer(payload).complete(
                payload.get("request_id"), payload.get("effect_id"),
                payload.get("executor_fence"), payload.get("completion"),
            )
            return {}
        if service == "abush_browser" and operation == "bind_x11_window":
            from x11_binding import bind_workspace_window
            try:
                return bind_workspace_window(payload)
            except Exception as error:
                logging.info("browser X11 bind failed remote=%s workspace=%s error=%s",
                             payload.get("remote_key"), payload.get("workspace_id"), error)
                raise
        raise ValueError("unsupported native operation")


class NativeBrokerHost:
    def __init__(self, input_stream=None, output_stream=None, remote_root=None):
        self.input = input_stream or sys.stdin.buffer
        self.output = output_stream or sys.stdout.buffer
        self.outbound = queue.Queue()
        self.broker = BrowserBroker(remote_root=remote_root, outbound=self.outbound)
        self.writer_error = None
        self.writer = threading.Thread(target=self._write_loop, daemon=True)

    def _write_loop(self):
        try:
            while True:
                message = self.outbound.get()
                if message is None:
                    return
                send_message(message, self.output)
        except (OSError, ValueError) as error:
            self.writer_error = error

    def run(self):
        self.writer.start()
        self.broker.refresh()
        try:
            while self.writer_error is None:
                message = read_message(self.input)
                if message is None:
                    return
                request_id = message.get("id") if isinstance(message, dict) else "invalid"
                try:
                    payload = self.broker.handle_request(message)
                    self.outbound.put(response(request_id, payload=payload))
                except (EnvelopeError, OSError, ValueError, WebSocketException) as error:
                    self.outbound.put(response(request_id, error=error))
        finally:
            self.broker.close()
            self.outbound.put(None)
            self.writer.join(timeout=2)


def main():
    NativeBrokerHost().run()


if __name__ == "__main__":
    main()
