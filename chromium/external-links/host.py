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
PEER_PROTOCOL_VERSION = 1
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
    _secure_stat(path, "file", 0o600, uid)
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
        self.capabilities = {}
        self.pending = {}
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
            self.reader = threading.Thread(target=self._read_loop, daemon=True)
            self.reader.start()
            self.broker.remote_changed(self)
            return True
        except (OSError, EOFError, ValueError, json.JSONDecodeError) as error:
            logging.info("browser peer connect failed remote=%s connection=%s error=%s",
                         self.metadata["remote_key"], self.metadata["connection_id"], error)
            self.close(emit=False)
            return False

    def _accept_hello(self, message):
        if message.get("type") != "hello_accepted":
            raise ValueError("peer rejected hello")
        request_id = message.get("request_id")
        principal_id = message.get("principal_id")
        fence = message.get("executor_fence")
        if not isinstance(request_id, str) or not request_id:
            raise ValueError("invalid hello request id")
        if not isinstance(principal_id, str) or not principal_id:
            raise ValueError("invalid principal id")
        if not isinstance(fence, (str, int)) or isinstance(fence, bool):
            raise ValueError("invalid executor fence")
        capabilities = message.get("capabilities", {})
        if not isinstance(capabilities, dict):
            raise ValueError("invalid peer capabilities")
        self.principal_id = principal_id
        self.executor_fence = fence
        self.capabilities = capabilities

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
            request_id = message.get("request_id")
            if not isinstance(request_id, str):
                raise ValueError("invalid ping")
            pong = {"type": self._wire_type(message, "pong"), "request_id": request_id}
            if "timestamp" in message:
                pong["timestamp"] = message["timestamp"]
            self.send(pong)
            return
        if normalized_type == "execute":
            self._handle_execute(message)
            return
        if normalized_type == "cancel":
            self._handle_cancel(message)
            return
        raise ValueError("unknown peer message")

    def _correlation(self, message):
        request_id = message.get("request_id")
        effect = message.get("effect")
        fence = message.get("executor_fence", message.get("fence"))
        if not isinstance(request_id, str) or not request_id or effect is None or fence is None:
            raise ValueError("invalid peer correlation")
        if fence != self.executor_fence:
            raise ValueError("stale executor fence")
        return request_id, effect, fence

    @staticmethod
    def _wire_type(message, default):
        value = message.get("type", "")
        return default[:1].upper() + default[1:] if value[:1].isupper() else default

    @staticmethod
    def _fence_field(message):
        return "fence" if "fence" in message and "executor_fence" not in message else \
            "executor_fence"

    def _completion(self, request_id, effect, fence, message):
        completion = {
            "type": self._wire_type(message, "completion"),
            "request_id": request_id,
            "effect": effect,
            self._fence_field(message): fence,
            "outcome": "Unknown" if message.get("type", "")[:1].isupper() else "unknown",
        }
        self.send(completion)

    def _handle_execute(self, message):
        request_id, effect, fence = self._correlation(message)
        if request_id in self.pending:
            raise ValueError("duplicate execute request")
        self.pending[request_id] = (effect, fence)
        self._completion(request_id, effect, fence, message)
        self.pending.pop(request_id, None)

    def _handle_cancel(self, message):
        request_id, effect, fence = self._correlation(message)
        pending = self.pending.pop(request_id, None)
        if pending is not None:
            self._completion(request_id, pending[0], pending[1], message)

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
            self.broker.remote_changed(self)


class BrowserBroker:
    def __init__(self, remote_root=None, outbound=None):
        self.remote_root = pathlib.Path(remote_root) if remote_root else None
        self.outbound = outbound or queue.Queue()
        self.peers = {}
        self.lock = threading.RLock()
        install = f"{pathlib.Path(__file__).resolve()}:{os.getuid()}".encode()
        self.install_id = hashlib.sha256(install).hexdigest()[:24]

    def executor_id(self, metadata):
        return f"browser-router-{self.install_id}-{metadata['connection_id']}"

    def remote_changed(self, peer):
        self.outbound.put(event(
            "abush_browser.remote_state", "abush_browser", peer.logical_state()
        ))

    def refresh(self):
        discovered = {(
            item["remote_key"], item["connection_id"]
        ): item for item in discover_remotes(self.remote_root)}
        with self.lock:
            for key in list(self.peers):
                if key not in discovered:
                    self.peers.pop(key).close(emit=True)
            for key, metadata in discovered.items():
                peer = self.peers.get(key)
                if peer and peer.metadata["browser_socket"] != metadata["browser_socket"]:
                    peer.close(emit=True)
                    peer = None
                if peer is None:
                    peer = RemotePeer(self, metadata)
                    self.peers[key] = peer
                else:
                    peer.metadata = metadata
                if not peer.connected:
                    peer.stop_event.clear()
                    peer.connect()
        return self.list_remotes()

    def list_remotes(self):
        with self.lock:
            return [self.peers[key].logical_state() for key in sorted(self.peers)]

    def close(self):
        with self.lock:
            peers = list(self.peers.values())
            self.peers.clear()
        for peer in peers:
            peer.close()

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
