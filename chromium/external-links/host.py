#!/usr/bin/env python3

import hashlib
import json
import logging
import os
import pathlib
import queue
import re
import select
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

# host.py is also loaded by path by the daemon's offline fixtures.
import importlib.util
_engine_spec = importlib.util.spec_from_file_location(
    "external_links_browser_engine", pathlib.Path(__file__).with_name("browser_engine.py"))
_browser_engine = importlib.util.module_from_spec(_engine_spec)
_engine_spec.loader.exec_module(_browser_engine)
BrowserEngine = _browser_engine.BrowserEngine
valid_action = _browser_engine.valid_action
valid_result = _browser_engine.valid_result
BoundedQueue = _browser_engine.BoundedQueue
SHUTDOWN_TIMEOUT = 2.0
NATIVE_IO_TIMEOUT = 2.0
MAX_REQUEST_WORKERS = 32
RESERVED_REQUEST_WORKERS = 8

try:
    from websockets.exceptions import WebSocketException
    from websockets.sync.client import connect
except ImportError:
    class WebSocketException(Exception):
        pass

    def connect(*_args, **_kwargs):
        raise WebSocketException("websockets package is unavailable")

MAX_MESSAGE_SIZE = 1024 * 1024
ORDINARY_OPEN_MAX_MESSAGE_SIZE = 64 * 1024
ORDINARY_OPEN_VERSION = 1
ORDINARY_OPEN_COMPLETIONS = {"applied", "not_applied", "unknown"}
ORDINARY_OPEN_DEADLINE_MS = 30_000
ORDINARY_LAUNCH_LEASE_VERSION = 1
ORDINARY_LAUNCH_LEASE_MAX_SIZE = 64 * 1024
ORDINARY_LAUNCH_LEASE_MAX_LIFETIME_MS = 60_000
ORDINARY_LAUNCH_LEASE_DIR = "ordinary-launch"
NATIVE_ENVELOPE_VERSION = 1
PEER_PROTOCOL_VERSION = 10
COMPLETION_RECEIPTS = {
    "completion": "completion_accepted",
    "cancellation_completed": "cancellation_completion_accepted",
}
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
ORDINARY_STAGING_ORIGIN = "chrome-extension://ahonjbfnmpjgjgaabbppofphidfomgcj"
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
    if group == "main" or ROUTES["groups"][group]["debug_port"] == 9222:
        return False
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


def ordinary_debug_port(value=None):
    value = value if value is not None else os.environ.get("ABUSH_CHROME_DEBUG_PORT", "9222")
    try:
        port = int(value)
    except (TypeError, ValueError) as error:
        raise ValueError("invalid ordinary Chrome debug port") from error
    if not 0 < port < 65536:
        raise ValueError("invalid ordinary Chrome debug port")
    return port


def ordinary_staging_url(nonce):
    return f"{ORDINARY_STAGING_ORIGIN}/ordinary-staging.html#{nonce}"


def _load_cdp_targets(port, opener):
    response = opener(f"http://127.0.0.1:{port}/json", timeout=1)
    try:
        targets = json.load(response)
    finally:
        close = getattr(response, "close", None)
        if close is not None:
            close()
    if not isinstance(targets, list) or not all(isinstance(target, dict) for target in targets):
        raise ValueError("invalid Chrome target list")
    return targets


def _valid_target_websocket(url, port):
    if not isinstance(url, str):
        return False
    try:
        parsed = urlsplit(url)
        return parsed.scheme == "ws" and parsed.hostname == "127.0.0.1" and \
            parsed.port == port and bool(parsed.path) and not parsed.username and \
            not parsed.password and not parsed.query and not parsed.fragment
    except ValueError:
        return False


def ordinary_staging_target(nonce, port, opener=urllib.request.urlopen):
    expected_url = ordinary_staging_url(nonce)
    expected_title = f"__ABUSH_ORDINARY__:{nonce}"
    matches = [
        target for target in _load_cdp_targets(port, opener)
        if target.get("type") == "page" and target.get("url") == expected_url and
        target.get("title") == expected_title
    ]
    if len(matches) != 1:
        raise ValueError("ordinary staging CDP target is not unique")
    target = matches[0]
    if not isinstance(target.get("id"), str) or not target["id"] or \
            not _valid_target_websocket(target.get("webSocketDebuggerUrl"), port):
        raise ValueError("ordinary staging CDP target is invalid")
    return target


def _strict_file_url(url):
    if not isinstance(url, str) or not url.startswith("file:///"):
        return False
    parsed = urlsplit(url)
    return parsed.scheme == "file" and not parsed.netloc and parsed.path.startswith("/") and \
        not parsed.query


def _receive_cdp_response(websocket, response_id, limit=64):
    for _ in range(limit):
        response = json.loads(websocket.recv(timeout=1))
        if not isinstance(response, dict):
            raise ValueError("invalid CDP response")
        if response.get("id") == response_id:
            return response
    raise TimeoutError("CDP response was not received")


def _file_target_applied(target_id, url, port, opener):
    matches = [
        target for target in _load_cdp_targets(port, opener)
        if target.get("type") == "page" and target.get("id") == target_id and
        target.get("url") == url
    ]
    return len(matches) == 1


def navigate_ordinary_file(
    payload,
    prepared_verifier,
    debug_port=None,
    opener=urllib.request.urlopen,
    websocket_connect=connect,
    verify_attempts=20,
    verify_interval=0.025,
):
    required = {"nonce", "xid", "url"}
    if not isinstance(payload, dict) or set(payload) != required:
        raise ValueError("invalid ordinary file navigation payload")
    nonce = payload.get("nonce")
    xid = payload.get("xid")
    url = payload.get("url")
    if not isinstance(nonce, str) or not re.fullmatch(r"[0-9a-f]{32}", nonce) or \
            not isinstance(xid, int) or isinstance(xid, bool) or xid <= 0 or \
            not isinstance(url, str):
        raise ValueError("invalid ordinary file navigation payload")
    result = {"nonce": nonce, "xid": xid, "url": url, "completion": "not_applied"}
    if not _strict_file_url(url):
        return result
    try:
        if prepared_verifier({"nonce": nonce, "xid": xid}) is not True:
            return result
        port = ordinary_debug_port(debug_port)
        target = ordinary_staging_target(nonce, port, opener)
    except Exception as error:
        logging.info("ordinary file navigation rejected before mutation nonce=%s error=%s",
                     nonce, error)
        return result

    request_sent = False
    try:
        request = {
            "id": 1,
            "method": "Page.navigate",
            "params": {"url": url},
        }
        with websocket_connect(
            target["webSocketDebuggerUrl"], open_timeout=1, close_timeout=1,
        ) as websocket:
            request_sent = True
            websocket.send(json.dumps(request))
            response = _receive_cdp_response(websocket, request["id"])
        navigation = response.get("result")
        if response.get("error") is not None or not isinstance(navigation, dict) or \
                not isinstance(navigation.get("frameId"), str) or not navigation["frameId"] or \
                navigation.get("errorText"):
            result["completion"] = "unknown"
            return result
        for attempt in range(verify_attempts):
            if _file_target_applied(target["id"], url, port, opener):
                result["completion"] = "applied"
                return result
            if attempt + 1 < verify_attempts:
                time.sleep(verify_interval)
        result["completion"] = "unknown"
        return result
    except Exception as error:
        result["completion"] = "unknown" if request_sent else "not_applied"
        logging.info("ordinary file navigation failed nonce=%s sent=%s error=%s",
                     nonce, request_sent, error)
        return result


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


def read_message(stream=None, on_frame=None):
    stream = stream or sys.stdin.buffer
    header = stream.read(4)
    if not header:
        return None
    if len(header) != 4:
        raise EOFError
    if on_frame is not None:
        on_frame()
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


def handle_message(message, ordinary_submit=None):
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
    if group and launcher != "chromium" and reuse_tab(group, hostname, url):
        logging.info("reused group=%s url=%s", group, url)
        return {"ok": True}
    logging.info("launching launcher=%s url=%s", launcher, url)
    if launcher == "chromium":
        try:
            completion = ordinary_submit(url) if ordinary_submit is not None else \
                submit_ordinary_open(url)
            if ordinary_submit is None:
                return {"ok": completion == "applied"}
            return {"ok": completion == "applied", "completion": completion}
        except (EOFError, OSError, TimeoutError, ValueError, json.JSONDecodeError) as error:
            logging.info("ordinary-open failed url=%s error=%s", url, error)
            failure = {"ok": False}
            if ordinary_submit is not None:
                failure["completion"] = "not_applied"
            return failure
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


def command_line(pid, proc_root="/proc"):
    try:
        return (pathlib.Path(proc_root) / str(pid) / "cmdline").read_bytes().split(b"\0")
    except OSError:
        return []


def parent_pid(pid, proc_root="/proc"):
    try:
        stat_line = (pathlib.Path(proc_root) / str(pid) / "stat").read_text()
        fields = stat_line[stat_line.rfind(")") + 1:].split()
        return int(fields[1])
    except (OSError, ValueError, IndexError):
        return 0


def packed_command_value(arguments, name):
    fields = [argument for argument in arguments if argument]
    if len(fields) != 1 or b" " not in fields[0]:
        return None
    pattern = re.compile(
        rb"(?:^|\s)" + re.escape(name) + rb"(?:=([^\s]+)|\s+([^\s]+))(?=\s|$)"
    )
    matches = pattern.findall(fields[0])
    if len(matches) > 1:
        raise ValueError("process command line has conflicting values")
    if not matches:
        return None
    return matches[0][0] or matches[0][1]


def command_profile(arguments):
    prefix = b"--user-data-dir="
    for index, argument in enumerate(arguments):
        if argument.startswith(prefix):
            return os.fsdecode(argument[len(prefix):])
        if argument == b"--user-data-dir" and index + 1 < len(arguments):
            return os.fsdecode(arguments[index + 1])
    packed = packed_command_value(arguments, b"--user-data-dir")
    return os.fsdecode(packed) if packed is not None else None


def process_starttime(pid, proc_root="/proc"):
    try:
        stat_line = (pathlib.Path(proc_root) / str(pid) / "stat").read_text()
        fields = stat_line[stat_line.rfind(")") + 1:].split()
        starttime = fields[19]
        if not starttime.isdigit():
            return None
        return starttime
    except (OSError, IndexError):
        return None


def browser_process_arguments(arguments):
    if command_profile(arguments) is None:
        return False
    if any(argument.startswith(b"--type=") for argument in arguments):
        return False
    fields = [argument for argument in arguments if argument]
    return len(fields) != 1 or re.search(rb"(?:^|\s)--type=[^\s]+(?=\s|$)", fields[0]) is None


def native_host_browser(pid=None, proc_root="/proc", max_depth=32):
    current = os.getppid() if pid is None else pid
    seen = set()
    for _ in range(max_depth):
        if current <= 1 or current in seen:
            break
        seen.add(current)
        arguments = command_line(current, proc_root)
        profile = command_profile(arguments)
        if profile is not None and browser_process_arguments(arguments):
            starttime = process_starttime(current, proc_root)
            if starttime is None:
                raise ValueError("Chrome browser start time is unavailable")
            resolved = pathlib.Path(profile).expanduser().resolve()
            identity = hashlib.sha256(
                f"{current}:{starttime}:{resolved}".encode()
            ).hexdigest()
            return {
                "pid": current,
                "starttime": starttime,
                "profile": resolved,
                "browser_boot_id": f"chrome-{identity}",
            }
        current = parent_pid(current, proc_root)
    return None


def native_host_profile(pid=None, proc_root="/proc", max_depth=32):
    current = os.getppid() if pid is None else pid
    seen = set()
    for _ in range(max_depth):
        if current <= 1 or current in seen:
            break
        seen.add(current)
        profile = command_profile(command_line(current, proc_root))
        if profile is not None:
            return pathlib.Path(profile).expanduser().resolve()
        current = parent_pid(current, proc_root)
    return None


def is_main_profile_host(profile=None, pid=None, proc_root="/proc"):
    expected = pathlib.Path(profile or os.environ.get(
        "ABUSH_CHROME_PROFILE", pathlib.Path.home() / ".config/google-chrome-main"
    )).expanduser().resolve()
    return native_host_profile(pid, proc_root) == expected


def ordinary_open_socket_path():
    injected = os.environ.get("ABUSH_ORDINARY_OPEN_SOCKET")
    if injected:
        return pathlib.Path(injected)
    runtime = os.environ.get("XDG_RUNTIME_DIR")
    if not runtime:
        raise ValueError("XDG_RUNTIME_DIR is unavailable")
    return pathlib.Path(runtime) / "abush" / "ordinary-open.sock"


def ordinary_launch_lease_root(runtime=None):
    runtime = runtime or os.environ.get("XDG_RUNTIME_DIR")
    if not runtime:
        raise ValueError("XDG_RUNTIME_DIR is unavailable")
    return pathlib.Path(runtime) / "abush" / ORDINARY_LAUNCH_LEASE_DIR


def _secure_runtime_directory(path, uid):
    path.mkdir(mode=0o700, parents=True, exist_ok=True)
    info = path.lstat()
    if path.is_symlink() or not stat.S_ISDIR(info.st_mode) or info.st_uid != uid:
        raise ValueError("unsafe ordinary launch lease directory")
    if stat.S_IMODE(info.st_mode) != 0o700:
        os.chmod(path, 0o700)
    return info


def _lease_profile_hash(profile):
    return hashlib.sha256(os.fsencode(pathlib.Path(profile).expanduser().resolve())).hexdigest()


def validate_ordinary_launch_lease(lease, *, profile, now_ms=None):
    fields = {
        "version", "request_id", "url", "url_hash", "deadline_ms", "profile_hash",
        "boot_target",
    }
    if not isinstance(lease, dict) or set(lease) != fields or \
            lease.get("version") != ORDINARY_LAUNCH_LEASE_VERSION or \
            lease.get("boot_target") != "next":
        raise ValueError("invalid ordinary launch lease")
    request_id = lease.get("request_id")
    url = lease.get("url")
    url_hash = lease.get("url_hash")
    deadline_ms = lease.get("deadline_ms")
    profile_hash = lease.get("profile_hash")
    if not isinstance(request_id, str) or not re.fullmatch(r"[0-9a-f]{32}", request_id) or \
            not isinstance(url_hash, str) or not re.fullmatch(r"[0-9a-f]{64}", url_hash) or \
            not isinstance(profile_hash, str) or not re.fullmatch(r"[0-9a-f]{64}", profile_hash) or \
            profile_hash != _lease_profile_hash(profile) or \
            not isinstance(deadline_ms, int) or isinstance(deadline_ms, bool):
        raise ValueError("invalid ordinary launch lease")
    validate_ordinary_open_request({
        "version": ORDINARY_OPEN_VERSION,
        "request_id": request_id,
        "operation": "ordinary_open",
        "url": url,
        "focus": True,
        "deadline_ms": deadline_ms,
    })
    if hashlib.sha256(url.encode()).hexdigest() != url_hash:
        raise ValueError("ordinary launch lease URL hash mismatch")
    now_ms = int(time.time() * 1000) if now_ms is None else now_ms
    if deadline_ms <= now_ms or deadline_ms - now_ms > ORDINARY_LAUNCH_LEASE_MAX_LIFETIME_MS:
        raise TimeoutError("ordinary launch lease expired or exceeds its lifetime")
    return lease


def claim_ordinary_launch_leases(profile, *, root=None, uid=None, now_ms=None):
    uid = os.getuid() if uid is None else uid
    root = pathlib.Path(root) if root else ordinary_launch_lease_root()
    _secure_runtime_directory(root.parent, uid)
    _secure_runtime_directory(root, uid)
    claimed = []
    seen_request_ids = set()
    entries = sorted(os.scandir(root), key=lambda entry: entry.name)
    for entry in entries:
        match = re.fullmatch(r"([0-9a-f]{32})\.(json|claimed)", entry.name)
        path = pathlib.Path(entry.path)
        claim = root / f"{match.group(1)}.claimed" if match else None
        if match is None or entry.is_symlink() or not entry.is_file(follow_symlinks=False):
            continue
        try:
            info = path.lstat()
            if info.st_uid != uid or stat.S_IMODE(info.st_mode) != 0o600 or \
                    info.st_size > ORDINARY_LAUNCH_LEASE_MAX_SIZE:
                raise ValueError("unsafe ordinary launch lease file")
            if match.group(2) == "json":
                try:
                    os.link(path, claim, follow_symlinks=False)
                except FileExistsError:
                    path.unlink()
                    continue
                path.unlink()
            claim_info = claim.lstat()
            if claim.is_symlink() or not stat.S_ISREG(claim_info.st_mode) or \
                    claim_info.st_uid != uid or stat.S_IMODE(claim_info.st_mode) != 0o600 or \
                    claim_info.st_size > ORDINARY_LAUNCH_LEASE_MAX_SIZE:
                raise ValueError("unsafe claimed ordinary launch lease")
            with claim.open("r", encoding="utf-8") as source:
                lease = validate_ordinary_launch_lease(
                    json.load(source), profile=profile, now_ms=now_ms)
            if lease["request_id"] != match.group(1):
                raise ValueError("ordinary launch lease filename identity mismatch")
            if lease["request_id"] in seen_request_ids:
                continue
            seen_request_ids.add(lease["request_id"])
            claimed.append({"lease": lease, "claim_path": claim})
        except (OSError, ValueError, TimeoutError, json.JSONDecodeError) as error:
            logging.info("ordinary launch lease rejected path=%s error=%s", path, error)
            try:
                candidate = claim if claim and claim.exists() else path
                info = candidate.lstat()
                if stat.S_ISREG(info.st_mode) and not candidate.is_symlink() and \
                        info.st_uid == uid:
                    candidate.unlink()
            except FileNotFoundError:
                pass
    return claimed


def ordinary_open_response(request_id, completion):
    return {
        "version": ORDINARY_OPEN_VERSION,
        "request_id": request_id,
        "operation": "ordinary_open_completed",
        "completion": completion,
    }


def ordinary_open_request(url, request_id=None, deadline_ms=None):
    if deadline_ms is None:
        deadline_ms = int(time.time() * 1000) + ORDINARY_OPEN_DEADLINE_MS
    return validate_ordinary_open_request({
        "version": ORDINARY_OPEN_VERSION,
        "request_id": request_id or uuid.uuid4().hex,
        "operation": "ordinary_open",
        "url": url,
        "focus": True,
        "deadline_ms": deadline_ms,
    })


def _ordinary_deadline(payload, required):
    fields = set(required) | {"deadline_ms"}
    if not isinstance(payload, dict) or set(payload) != fields:
        raise ValueError("invalid ordinary-open payload")
    deadline_ms = payload.get("deadline_ms")
    if not isinstance(deadline_ms, int) or isinstance(deadline_ms, bool) or deadline_ms <= 0:
        raise ValueError("invalid ordinary-open deadline")
    if time.time() * 1000 >= deadline_ms:
        raise TimeoutError("ordinary-open deadline expired")
    return deadline_ms


def _without_deadline(payload):
    return {key: value for key, value in payload.items() if key != "deadline_ms"}


def validate_ordinary_open_request(message):
    fields = {"version", "request_id", "operation", "url", "focus", "deadline_ms"}
    if not isinstance(message, dict) or set(message) != fields or \
            message.get("version") != ORDINARY_OPEN_VERSION or \
            message.get("operation") != "ordinary_open" or message.get("focus") is not True:
        raise ValueError("invalid ordinary-open request")
    request_id = message.get("request_id")
    url = message.get("url")
    deadline_ms = message.get("deadline_ms")
    if not isinstance(request_id, str) or not request_id or len(request_id) > 128 or \
            not isinstance(url, str) or not url or len(url.encode()) > 32768 or \
            not isinstance(deadline_ms, int) or isinstance(deadline_ms, bool) or deadline_ms <= 0:
        raise ValueError("invalid ordinary-open request")
    parsed = urlsplit(url)
    if parsed.scheme not in {"http", "https", "file"} or \
            (parsed.scheme in {"http", "https"} and not parsed.hostname) or \
            (parsed.scheme == "file" and (not url.startswith("file:///") or parsed.netloc)):
        raise ValueError("invalid ordinary-open URL")
    return message


def encode_ordinary_open_message(message):
    payload = json.dumps(message, separators=(",", ":")).encode()
    if len(payload) > ORDINARY_OPEN_MAX_MESSAGE_SIZE:
        raise ValueError("ordinary-open message is too large")
    return struct.pack(">I", len(payload)) + payload


def read_ordinary_open_message(sock):
    size = struct.unpack(">I", read_exact(sock, 4))[0]
    if size > ORDINARY_OPEN_MAX_MESSAGE_SIZE:
        raise ValueError("ordinary-open message is too large")
    message = json.loads(read_exact(sock, size))
    if not isinstance(message, dict):
        raise ValueError("ordinary-open message must be an object")
    return message


class OrdinaryOpenServer:
    def __init__(self, outbound, path=None, uid=None, timeout=30, bootstrap_requests=None,
                 bootstrap_claims=None):
        self.outbound = outbound
        self.path = pathlib.Path(path) if path else ordinary_open_socket_path()
        self.uid = os.getuid() if uid is None else uid
        self.timeout = timeout
        self.bootstrap_requests = list(bootstrap_requests or [])
        self.bootstrap_claims = dict(bootstrap_claims or {})
        self.server = None
        self.socket_identity = None
        self.thread = None
        self.workers = set()
        self.lock = threading.RLock()
        self.requests = {}
        self.stopping = threading.Event()

    def start(self):
        directory = self.path.parent
        directory.mkdir(mode=0o700, parents=True, exist_ok=True)
        info = directory.lstat()
        if directory.is_symlink() or not stat.S_ISDIR(info.st_mode) or info.st_uid != self.uid:
            raise ValueError("unsafe ordinary-open runtime directory")
        os.chmod(directory, 0o700)
        if self.path.exists() or self.path.is_symlink():
            info = self.path.lstat()
            if self.path.is_symlink() or not stat.S_ISSOCK(info.st_mode) or \
                    info.st_uid != self.uid:
                raise ValueError("unsafe ordinary-open socket path")
            probe = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            probe.settimeout(0.1)
            try:
                probe.connect(str(self.path))
            except (ConnectionRefusedError, FileNotFoundError):
                current = self.path.lstat()
                if current.st_dev != info.st_dev or current.st_ino != info.st_ino:
                    raise ValueError("ordinary-open socket path changed during verification")
                self.path.unlink()
            except OSError as error:
                raise ValueError("cannot verify ordinary-open socket path") from error
            else:
                raise ValueError("ordinary-open socket is already active")
            finally:
                probe.close()
        self.server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.server.bind(str(self.path))
        os.chmod(self.path, 0o600)
        bound = self.path.stat()
        self.socket_identity = (bound.st_dev, bound.st_ino)
        self.server.listen(16)
        self.server.settimeout(0.2)
        self.thread = threading.Thread(target=self._serve, daemon=True)
        self.thread.start()
        for request in self.bootstrap_requests:
            self._register(request, source="bootstrap", publish=False)

    def _serve(self):
        while not self.stopping.is_set():
            try:
                client, _ = self.server.accept()
            except socket.timeout:
                continue
            except OSError:
                return
            worker = threading.Thread(target=self._handle, args=(client,), daemon=True)
            with self.lock:
                if len(self.workers) >= 32:
                    client.close()
                    logging.warning("ordinary-open worker admission exhausted")
                    continue
                self.workers.add(worker)
            worker.start()

    def _cancel_locked(self, state):
        if state["response"] is not None or state["cancelled"]:
            return False
        state["cancelled"] = True
        self.outbound.put(event("ordinary_open_cancel", "ordinary_open", {
            "version": ORDINARY_OPEN_VERSION,
            "request_id": state["request"]["request_id"],
            "operation": "ordinary_open_cancel",
        }))
        return True

    def _cancel(self, state):
        with self.lock:
            return self._cancel_locked(state)

    def _attach_waiter(self, state):
        waiter = object()
        with self.lock:
            state["active_waiters"].add(waiter)
        return waiter

    def _release_waiter(self, state, waiter, abandoned=False):
        with self.lock:
            state["active_waiters"].discard(waiter)
            if abandoned and not state["active_waiters"] and not state["bootstrap_owner"]:
                self._cancel_locked(state)

    def _wait(self, state, client=None):
        while not state["done"].wait(0.05):
            if self.stopping.is_set():
                raise TimeoutError("ordinary-open broker stopped")
            if time.time() * 1000 >= state["request"]["deadline_ms"]:
                self._cancel(state)
                raise TimeoutError("ordinary-open completion timed out")
            if client is not None:
                try:
                    readable, _, _ = select.select([client], [], [], 0)
                    if readable and client.recv(1, socket.MSG_PEEK) == b"":
                        raise EOFError("ordinary-open client disconnected")
                except BlockingIOError:
                    pass
        if state["response"] is None:
            raise TimeoutError("ordinary-open broker stopped")
        return state["response"]

    def _register(self, request, source="socket", publish=True):
        with self.lock:
            state = self.requests.get(request["request_id"])
            if state is None:
                if len(self.requests) >= 256:
                    evict = next((key for key, value in self.requests.items()
                                  if value["done"].is_set()), None)
                    if evict is None:
                        raise ValueError("ordinary-open request admission exhausted")
                    self.requests.pop(evict)
                state = {"request": request, "done": threading.Event(), "response": None,
                         "cancelled": False, "source": source, "active_waiters": set(),
                         "bootstrap_owner": source == "bootstrap"}
                self.requests[request["request_id"]] = state
                if publish:
                    self.outbound.put(event(
                        "ordinary_open_bootstrap" if source == "bootstrap" else "ordinary_open",
                        "ordinary_open", request))
            elif state["request"] != request:
                raise ValueError("conflicting duplicate ordinary-open request")
            elif source == "bootstrap":
                state["bootstrap_owner"] = True
            return state

    def _handle(self, client):
        state = None
        waiter = None
        abandoned = False
        try:
            client.settimeout(self.timeout)
            credentials = client.getsockopt(socket.SOL_SOCKET, socket.SO_PEERCRED,
                                            struct.calcsize("3i"))
            _, uid, _ = struct.unpack("3i", credentials)
            if uid != self.uid:
                return
            request = validate_ordinary_open_request(read_ordinary_open_message(client))
            state = self._register(request)
            waiter = self._attach_waiter(state)
            response = self._wait(state, client)
            client.sendall(encode_ordinary_open_message(response))
        except (EOFError, OSError, TimeoutError, ValueError, json.JSONDecodeError):
            abandoned = state is not None and not state["done"].is_set()
        finally:
            if state is not None and waiter is not None:
                self._release_waiter(state, waiter, abandoned=abandoned)
            client.close()
            with self.lock:
                self.workers.discard(threading.current_thread())

    def submit(self, url, request_id=None, timeout=None):
        timeout = self.timeout if timeout is None else timeout
        request = ordinary_open_request(
            url, request_id=request_id,
            deadline_ms=int(time.time() * 1000 + timeout * 1000),
        )
        state = self._register(request)
        waiter = self._attach_waiter(state)
        try:
            return self._wait(state)["completion"]
        finally:
            self._release_waiter(state, waiter, abandoned=not state["done"].is_set())

    def complete(self, payload):
        fields = {"version", "request_id", "operation", "completion"}
        if not isinstance(payload, dict) or set(payload) != fields or \
                payload.get("version") != ORDINARY_OPEN_VERSION or \
                payload.get("operation") != "ordinary_open_completed" or \
                payload.get("completion") not in ORDINARY_OPEN_COMPLETIONS:
            raise ValueError("invalid ordinary-open completion")
        with self.lock:
            state = self.requests.get(payload.get("request_id"))
            if state is None:
                raise ValueError("uncorrelated ordinary-open completion")
            if state["response"] is not None:
                if state["response"] != payload:
                    raise ValueError("conflicting ordinary-open completion")
                return
            state["response"] = payload
            state["bootstrap_owner"] = False
            state["done"].set()
            self._remove_bootstrap_claim(payload["request_id"])
            while len(self.requests) > 256:
                request_id = next(iter(self.requests))
                if not self.requests[request_id]["done"].is_set():
                    break
                self.requests.pop(request_id)

    def _remove_bootstrap_claim(self, request_id):
        claim = self.bootstrap_claims.pop(request_id, None)
        if claim is None:
            return
        try:
            info = claim.lstat()
            if stat.S_ISREG(info.st_mode) and not claim.is_symlink() and info.st_uid == self.uid:
                claim.unlink()
        except FileNotFoundError:
            pass

    def close(self, deadline=None):
        deadline = deadline or time.monotonic() + SHUTDOWN_TIMEOUT
        self.stopping.set()
        if self.server is not None:
            self.server.close()
        if self.thread is not None:
            self.thread.join(timeout=max(0, deadline - time.monotonic()))
        with self.lock:
            states = list(self.requests.values())
            workers = list(self.workers)
        for state in states:
            state["done"].set()
        for worker in workers:
            worker.join(timeout=max(0, deadline - time.monotonic()))
        try:
            info = self.path.lstat()
            identity = (info.st_dev, info.st_ino)
            if stat.S_ISSOCK(info.st_mode) and info.st_uid == self.uid and \
                    identity == self.socket_identity:
                self.path.unlink()
        except FileNotFoundError:
            pass


def submit_ordinary_open(url, socket_path=None, timeout=30, request_id=None):
    request = ordinary_open_request(
        url, request_id=request_id,
        deadline_ms=int(time.time() * 1000 + timeout * 1000),
    )
    path = pathlib.Path(socket_path) if socket_path else ordinary_open_socket_path()
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
        client.settimeout(timeout)
        client.connect(str(path))
        client.sendall(encode_ordinary_open_message(request))
        completion = read_ordinary_open_message(client)
    expected = ordinary_open_response(request["request_id"], completion.get("completion"))
    if completion != expected or completion.get("completion") not in ORDINARY_OPEN_COMPLETIONS:
        raise ValueError("invalid ordinary-open completion")
    return completion["completion"]


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
        self.owner_display = None
        self.executor_fence = None
        self.workspace_id = None
        self.hello_request_id = None
        self.pending = {}
        self.completions = {}
        self.accepted_completions = {}
        self.completion_timeout = 15
        self.pending_cancellations = {}
        self.workspace_requests = {}
        self.transport_lock = threading.RLock()
        self.workspace_timeout = 15
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
            state["workspace_id"] = self.workspace_id
            state["executor_fence"] = self.executor_fence
        if self.owner_display is not None:
            state["owner_display"] = self.owner_display
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
                "capabilities": {"cancellation": True, "owned_tab_group": True},
            })
            prepared = read_peer_message(sock)
            self._accept_prepared(prepared)
            self.send({
                "type": "hello_ready",
                "request_id": prepared["request_id"],
                "executor_fence": prepared["executor_fence"],
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

    def _accept_prepared(self, message):
        if not self._exact_fields(message, {
            "type", "request_id", "principal_id", "workspace_id", "executor_fence", "owner_display"
        }) or message.get("type") != "hello_prepared":
            raise ValueError("invalid prepared hello response")
        request_id = message.get("request_id")
        principal_id = message.get("principal_id")
        fence = message.get("executor_fence")
        owner_display = message.get("owner_display")
        if not isinstance(request_id, str) or not request_id or \
                not isinstance(principal_id, str) or not principal_id or \
                not self._valid_executor_fence(fence) or \
                not self._valid_owner_display(owner_display):
            raise ValueError("invalid prepared hello response")
        workspace_id = message.get("workspace_id")
        if not isinstance(workspace_id, str) or not workspace_id:
            raise ValueError("invalid hello workspace")
        self.workspace_id = workspace_id
        self.hello_request_id = request_id
        self.principal_id = principal_id
        self.owner_display = owner_display
        self.executor_fence = fence

    def _accept_hello(self, message):
        if message.get("type") == "hello_rejected":
            if not self._exact_fields(message, {"type", "request_id", "reason"}) or \
                    not isinstance(message.get("request_id"), str) or \
                    not message.get("request_id") or message.get("reason") not in {
                        "unsupported_version", "invalid_hello", "principal_unavailable"
                    }:
                raise ValueError("invalid hello rejection")
            raise ValueError(f"peer rejected hello: {message['reason']}")
        if not self._exact_fields(message, {
            "type", "request_id", "principal_id", "workspace_id", "executor_fence", "owner_display"
        }) or message.get("type") != "hello_accepted":
            raise ValueError("invalid hello response")
        request_id = message.get("request_id")
        principal_id = message.get("principal_id")
        fence = message.get("executor_fence")
        owner_display = message.get("owner_display")
        if not isinstance(request_id, str) or not request_id:
            raise ValueError("invalid hello request id")
        if not isinstance(principal_id, str) or not principal_id:
            raise ValueError("invalid principal id")
        if not self._valid_executor_fence(fence):
            raise ValueError("invalid executor fence")
        if not self._valid_owner_display(owner_display):
            raise ValueError("invalid owner display")
        if request_id != self.hello_request_id or message["workspace_id"] != self.workspace_id or \
                principal_id != self.principal_id or fence != self.executor_fence or \
                owner_display != self.owner_display:
            raise ValueError("hello acceptance changed prepared identity")
        self.principal_id = principal_id

    @staticmethod
    def _valid_owner_display(value):
        fields = {"daemon_instance_id", "daemon_boot_id", "workbench_name"}
        return isinstance(value, dict) and set(value) == fields and all(
            isinstance(value[field], str) and value[field] and len(value[field]) <= 256
            for field in fields
        )

    @staticmethod
    def _valid_executor_fence(value):
        return isinstance(value, dict) and set(value) == {"executor_id", "epoch"} and \
            isinstance(value["executor_id"], str) and value["executor_id"] and \
            isinstance(value["epoch"], int) and not isinstance(value["epoch"], bool) and \
            0 < value["epoch"] <= 2 ** 64 - 1

    @staticmethod
    def _positive(value):
        return isinstance(value, int) and not isinstance(value, bool) and 0 < value <= 2 ** 64 - 1

    @classmethod
    def _valid_page_ref(cls, value):
        return cls._exact_fields(value, {"workspace_id", "page_id", "generation"}) and \
            all(isinstance(value[key], str) and value[key]
                for key in ("workspace_id", "page_id")) and cls._positive(value["generation"])

    @classmethod
    def _valid_authorization(cls, value):
        return cls._exact_fields(value, {"browser_boot_id", "enable_id", "revision"}) and \
            all(isinstance(value[key], str) and value[key]
                for key in ("browser_boot_id", "enable_id")) and cls._positive(value["revision"])

    def _valid_workspace_state(self, value):
        if not self._exact_fields(value, {"browser_boot_id", "revision", "phase", "binding"}) or \
                not isinstance(value["browser_boot_id"], str) or not value["browser_boot_id"] or \
                not self._positive(value["revision"]) or \
                not isinstance(value["phase"], str) or \
                value["phase"] not in {"disabled", "enabled", "paused"}:
            return False
        binding = value["binding"]
        if binding is None:
            return value["phase"] == "disabled"
        return value["phase"] in {"enabled", "paused"} and \
            self._exact_fields(binding, {"enable_id", "page"}) and \
            isinstance(binding["enable_id"], str) and bool(binding["enable_id"]) and \
            self._valid_page_ref(binding["page"]) and \
            binding["page"]["workspace_id"] == self.workspace_id

    def complete_cancellation(self, request_id, effect_id, fence, completion):
        if not isinstance(completion, str) or completion not in {"not_applied", "unknown"}:
            raise ValueError("invalid cancellation completion")
        receipt = self._complete_request(self.pending_cancellations, effect_id, {
            "request_id": request_id, "fence": fence,
        }, {
            "type": "cancellation_completed",
            "request_id": request_id,
            "effect_id": effect_id,
            "executor_fence": fence,
            "completion": completion,
        })
        pending = self.pending.get(effect_id)
        if pending is not None and pending["fence"] == fence:
            self.pending.pop(effect_id, None)
        return receipt

    def complete(self, request_id, effect_id, fence, completion, result=None, error=None):
        if not self._valid_output(completion, result, error):
            raise ValueError("invalid executor completion output")
        return self._complete_request(self.pending, effect_id, {
            "request_id": request_id, "fence": fence,
        }, {
            "type": "completion",
            "request_id": request_id,
            "effect_id": effect_id,
            "executor_fence": fence,
            "completion": completion, "result": result, "error": error,
        })

    @staticmethod
    def _valid_output(completion, result, error):
        if completion == "applied":
            return valid_result(result) and error is None
        return completion in {"not_applied", "unknown"} and result is None and \
            isinstance(error, str) and bool(error)

    @staticmethod
    def _retain_bounded(history, key, value):
        history[key] = value
        while len(history) > 256:
            history.pop(next(iter(history)))

    @staticmethod
    def _receipt_key(message):
        fence = message["executor_fence"]
        return (message["type"], message["request_id"],
                message["effect_id"],
                fence["executor_id"], fence["epoch"])

    def _handle_completion_accepted(self, message):
        fields = {"type", "request_id", "executor_fence", "effect_id", "completion"}
        if message.get("type") == "completion_accepted":
            fields |= {"result", "error"}
            if not self._valid_output(message.get("completion"), message.get("result"),
                                      message.get("error")):
                raise ValueError("invalid completion receipt output")
        if not self._exact_fields(message, fields) or not all(
                isinstance(message.get(field), str) and message[field]
                for field in ("request_id", "effect_id")) or \
                not self._valid_executor_fence(message.get("executor_fence")):
            raise ValueError("invalid completion receipt")
        if not isinstance(message["completion"], str) or \
                message["completion"] not in {"applied", "not_applied", "unknown"} or \
                (message["type"] == "cancellation_completion_accepted" and
                 message["completion"] == "applied"):
            raise ValueError("invalid completion receipt outcome")
        receipt_key = self._receipt_key(message)
        with self.transport_lock:
            previous = self.accepted_completions.get(receipt_key)
            if previous is not None and previous != message:
                raise ValueError("conflicting completion receipt")
            for state in self.completions.values():
                expected = {**state["message"],
                            "type": COMPLETION_RECEIPTS[state["message"]["type"]]}
                if self._receipt_key(expected) == receipt_key:
                    if expected != message:
                        raise ValueError("conflicting completion receipt")
                    state["response"] = json.loads(json.dumps(message))
                    state["done"].set()
            self._retain_bounded(self.accepted_completions, receipt_key,
                                 json.loads(json.dumps(message)))

    def _complete_request(self, requests, key, identity, message):
        expected = {**message, "type": COMPLETION_RECEIPTS[message["type"]]}
        if not self._valid_executor_fence(message.get("executor_fence")) or not all(
                isinstance(value, str) and value for value in (message["request_id"], key)):
            raise ValueError("invalid peer completion identity")
        with self.transport_lock:
            history_key = (message["type"], key)
            completed = self.completions.get(history_key)
            receipt = self.accepted_completions.get(self._receipt_key(expected))
            if receipt is not None and receipt != expected:
                raise ValueError("conflicting duplicate peer completion")
            if completed is not None:
                if completed["message"] != message or any(
                        completed["identity"].get(field) != value
                        for field, value in identity.items()):
                    raise ValueError("conflicting duplicate peer completion")
            else:
                pending = requests.get(key)
                if pending is not None and any(
                        pending.get(field) != value for field, value in identity.items()):
                    raise ValueError("uncorrelated peer completion")
                # A restarted host has no origin command memory. Forward only the
                # terminal selection; Rust checks durable command admission and
                # returns an exact receipt before native success is possible.
                completed = json.loads(json.dumps({
                    "identity": pending if pending is not None else identity, "message": message}))
                completed.update(done=threading.Event(), response=receipt)
                if len(self.completions) >= 256:
                    evict = next((entry for entry, state in self.completions.items()
                                  if state["response"] is not None), None)
                    if evict is None:
                        raise ValueError("completion receipt backlog is full")
                    self.completions.pop(evict)
                self.completions[history_key] = completed
                requests.pop(key, None)
            if completed["response"] is not None:
                return json.loads(json.dumps(completed["response"]))
            completed["done"].clear()
        # Register immutable selection and waiter before writing; the reader must
        # acquire transport_lock even while sendall is blocked or fails ambiguously.
        try:
            self.send(completed["message"])
        except OSError:
            with self.transport_lock:
                if completed["response"] is not None:
                    return json.loads(json.dumps(completed["response"]))
            raise
        completed["done"].wait(self.completion_timeout)
        with self.transport_lock:
            receipt = completed["response"]
            if receipt is None:
                raise TimeoutError("durable completion receipt timed out")
            return json.loads(json.dumps(receipt))

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
                try:
                    self.handle(message)
                except ValueError as error:
                    logging.warning("browser peer message rejected: %s", error)
        except (OSError, EOFError, ValueError, json.JSONDecodeError, queue.Full) as error:
            if not self.stop_event.is_set():
                logging.info("browser peer closed remote=%s connection=%s error=%s",
                             self.metadata["remote_key"], self.metadata["connection_id"], error)
        finally:
            self.close(emit=True, from_reader=True)

    def handle(self, message):
        message_type = message.get("type")
        if not isinstance(message_type, str):
            raise ValueError("invalid peer message type")
        if message_type == "workspace_state_accepted":
            self._handle_workspace_state_accepted(message)
            return
        if message_type in COMPLETION_RECEIPTS.values():
            self._handle_completion_accepted(message)
            return
        if message_type == "ping":
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
        if message_type == "execute":
            self._handle_execute(message)
            return
        if message_type == "cancel":
            self._handle_cancel(message)
            return
        raise ValueError("unknown peer message")

    def workspace_state(self, request_id, fence, value):
        if not isinstance(request_id, str) or not request_id or \
                not self._valid_executor_fence(fence) or fence != self.executor_fence or \
                not self._valid_workspace_state(value):
            raise ValueError("invalid workspace state")
        browser = self.broker.browser
        if browser is None or value["browser_boot_id"] != browser["browser_boot_id"]:
            raise ValueError("workspace state browser incarnation mismatch")
        message = json.loads(json.dumps({
            "type": "workspace_state", "request_id": request_id,
            "executor_fence": fence, "state": value,
        }))
        with self.transport_lock:
            state = self.workspace_requests.get(request_id)
            if state is not None:
                if state["message"] != message:
                    raise ValueError("conflicting duplicate workspace state")
                if state["response"] is not None:
                    return json.loads(json.dumps(state["response"]))
            else:
                if len(self.workspace_requests) >= 256:
                    evict = next((key for key, entry in self.workspace_requests.items()
                                  if entry["response"] is not None), None)
                    if evict is None:
                        raise ValueError("workspace state receipt backlog is full")
                    self.workspace_requests.pop(evict)
                state = {"message": message, "done": threading.Event(), "response": None}
                self.workspace_requests[request_id] = state
        try:
            self.send(message)
        except OSError:
            with self.transport_lock:
                if state["response"] is not None:
                    return json.loads(json.dumps(state["response"]))
            raise
        state["done"].wait(self.workspace_timeout)
        with self.transport_lock:
            if state["response"] is None:
                raise TimeoutError("workspace state receipt timed out")
            return json.loads(json.dumps(state["response"]))

    def _handle_workspace_state_accepted(self, message):
        if not self._exact_fields(message, {"type", "request_id", "executor_fence", "state"}) or \
                not isinstance(message["request_id"], str) or not message["request_id"] or \
                not self._valid_executor_fence(message["executor_fence"]) or \
                not self._valid_workspace_state(message["state"]):
            raise ValueError("invalid workspace state receipt")
        with self.transport_lock:
            state = self.workspace_requests.get(message["request_id"])
            if state is None or message != {
                    **state["message"], "type": "workspace_state_accepted"}:
                raise ValueError("uncorrelated workspace state receipt")
            state["response"] = json.loads(json.dumps(message))
            state["done"].set()

    @classmethod
    def _valid_command(cls, command):
        if cls._exact_fields(command, {"command", "page", "action"}) and \
                command["command"] == "action" and cls._valid_page_ref(command["page"]):
            return valid_action(command["action"])
        if not cls._exact_fields(command, {"command", "page", "url"}) or \
                command["command"] != "navigate" or not cls._valid_page_ref(command["page"]):
            return False
        url = command["url"]
        if not isinstance(url, str) or not url or len(url.encode()) > 32768 or \
                any(character.isspace() or ord(character) < 32 for character in url):
            return False
        try:
            parsed = urlsplit(url)
            return parsed.scheme in {"https", "http"} and bool(parsed.hostname) and \
                not parsed.username and not parsed.password and \
                (parsed.port is None or 0 < parsed.port < 65536) and \
                url.startswith(f"{parsed.scheme}://")
        except ValueError:
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
        if self.broker.peer({
                "remote_key": self.metadata["remote_key"],
                "connection_id": self.metadata["connection_id"],
                "principal_id": self.principal_id,
        }) is not self:
            raise ValueError("stale peer transport")
        return request_id, effect_id, fence

    def _handle_execute(self, message):
        request_id, effect_id, fence = self._correlation(message, {
            "type", "request_id", "effect_id", "executor_fence", "authorization", "command"
        })
        command = message.get("command")
        authorization = message.get("authorization")
        if not self._valid_command(command) or \
                command["page"]["workspace_id"] != self.workspace_id or \
                not self._valid_authorization(authorization) or self.broker.browser is None or \
                authorization["browser_boot_id"] != self.broker.browser["browser_boot_id"]:
            raise ValueError("invalid browser command")
        correlation = json.loads(json.dumps({
            "request_id": request_id, "fence": fence, "command": command,
            "authorization": authorization,
        }))
        with self.transport_lock:
            completed = self.completions.get(("completion", effect_id))
            known = self.pending.get(effect_id)
            if completed is not None:
                known = completed["identity"]
            if known is not None:
                if known != correlation:
                    raise ValueError("conflicting duplicate execute effect")
                return
            pending_bytes = sum(len(json.dumps(value, separators=(",", ":")).encode())
                                for value in self.pending.values())
            incoming_bytes = len(json.dumps(correlation, separators=(",", ":")).encode())
            if len(self.pending) >= 32 or pending_bytes + incoming_bytes > 4 * MAX_MESSAGE_SIZE:
                raise OSError("browser execution admission exhausted (32 / 4 MiB per remote)")
            self.pending[effect_id] = correlation
        self.broker.outbound.put(event("abush_browser.execute", "abush_browser", {
            "remote_key": self.metadata["remote_key"],
            "connection_id": self.metadata["connection_id"],
            "principal_id": self.principal_id,
            "request_id": request_id,
            "effect_id": effect_id,
            "executor_fence": fence,
            "authorization": authorization,
            "command": command,
        }))

    def _handle_cancel(self, message):
        request_id, effect_id, fence = self._correlation(message, {
            "type", "request_id", "effect_id", "executor_fence"
        })
        identity = {"request_id": request_id, "fence": fence}
        with self.transport_lock:
            known = self.pending_cancellations.get(effect_id)
            completed = self.completions.get(("cancellation_completed", effect_id))
            if completed is not None:
                known = completed["identity"]
            if known is not None:
                if known != identity:
                    raise ValueError("conflicting duplicate cancellation")
                return
            pending = self.pending.get(effect_id)
            if pending is None:
                ordinary = self.completions.get(("completion", effect_id))
                pending = ordinary["identity"] if ordinary is not None else None
            if pending is None or pending["fence"] != fence or request_id == pending["request_id"]:
                raise ValueError("invalid cancel correlation")
            self.pending_cancellations[effect_id] = identity
        self.broker.outbound.put(event("abush_browser.cancel", "abush_browser", {
            "remote_key": self.metadata["remote_key"],
            "connection_id": self.metadata["connection_id"],
            "principal_id": self.principal_id,
            "request_id": request_id,
            "execution_request_id": pending["request_id"],
            "effect_id": effect_id,
            "executor_fence": fence,
        }))

    def close(self, emit=False, from_reader=False, deadline=None):
        deadline = deadline or time.monotonic() + SHUTDOWN_TIMEOUT
        was_connected = self.connected
        self.connected = False
        self.broker.close_engine_peer(self)
        self.stop_event.set()
        sock, self.socket = self.socket, None
        if sock is not None:
            try:
                sock.shutdown(socket.SHUT_RDWR)
            except OSError:
                pass
            sock.close()
        with self.transport_lock:
            transport_states = [
                *self.completions.values(),
                *self.workspace_requests.values(),
            ]
        for state in transport_states:
            state["done"].set()
        if self.reader and not from_reader and self.reader is not threading.current_thread():
            self.reader.join(timeout=max(0, deadline - time.monotonic()))
        if emit and was_connected:
            self.broker.remote_disconnected(self)


class BrowserBroker:
    def __init__(self, remote_root=None, outbound=None, ordinary_open=None,
                 write_barrier=None, browser=None):
        self.browser = browser
        self.remote_root = pathlib.Path(remote_root) if remote_root else None
        self.outbound = outbound if outbound is not None else BoundedQueue()
        self.ordinary_open = ordinary_open
        self.write_barrier = write_barrier
        self.peers = {}
        self.principals = {}
        self.completion_history = {}
        self.candidates = {}
        self.lock = threading.RLock()
        self.engine_lock = threading.RLock()
        self.engine_leases = {}
        self.engine_retirements = set()
        self.engine_shutdown_errors = []
        install = f"{pathlib.Path(__file__).resolve()}:{os.getuid()}".encode()
        self.install_id = hashlib.sha256(install).hexdigest()[:24]

    def close_engine_peer(self, peer):
        with self.engine_lock:
            leases = [(key, value) for key, value in self.engine_leases.items()
                      if value["peer"] is peer]
            for key, _state in leases:
                self.engine_leases.pop(key, None)
        # Peer retirement can be called inside broker selection's lock. Fence now,
        # but never wait for an owned child while holding that selection lock.
        if leases:
            def stop_retired():
                deadline = time.monotonic() + SHUTDOWN_TIMEOUT
                try:
                    for key, state in leases:
                        try:
                            self._stop_engine(key, state, deadline)
                        except OSError as error:
                            logging.error("retired engine shutdown failed: %s", error)
                            with self.engine_lock:
                                self.engine_shutdown_errors.append(str(error))
                                del self.engine_shutdown_errors[:-16]
                finally:
                    with self.engine_lock:
                        self.engine_retirements.discard(threading.current_thread())
            worker = threading.Thread(target=stop_retired, daemon=True)
            with self.engine_lock:
                self.engine_retirements.add(worker)
            worker.start()

    def close_engine_lease(self, lease_id, deadline=None):
        with self.engine_lock:
            state = self.engine_leases.pop(lease_id, None)
        self._stop_engine(lease_id, state, deadline)

    @staticmethod
    def _stop_engine(lease_id, state, deadline=None):
        if state and state.get("engine"):
            try:
                if deadline is None:
                    state["engine"].close_lease(lease_id)
            except OSError as error:
                logging.warning("engine lease close failed: %s", error)
                raise
            finally:
                if deadline is None or not isinstance(state["engine"], _browser_engine.BrowserEngine):
                    state["engine"].close()
                else:
                    state["engine"].close(deadline=deadline)

    def engine_request(self, operation, payload):
        context_fields = {"remote_key", "connection_id", "principal_id", "request_id",
                          "effect_id", "executor_fence", "authorization", "command"}
        extras = {"engine_open": set(), "engine_execute": {"lease_id"},
                  "engine_cdp": {"lease_id", "message"}, "engine_close": {"lease_id"}}
        if operation not in extras or not isinstance(payload, dict) or \
                set(payload) != context_fields | extras[operation]:
            raise ValueError("invalid engine operation")
        peer = self.peer(payload)
        context = {key: payload[key] for key in context_fields}
        expected = {"request_id": payload["request_id"], "fence": payload["executor_fence"],
                    "authorization": payload["authorization"], "command": payload["command"]}
        with peer.transport_lock:
            admitted = peer.pending.get(payload["effect_id"]) == expected
        if not admitted and operation not in {"engine_close", "engine_cdp"}:
            raise ValueError("engine operation has no current daemon admission")
        if operation == "engine_open":
            lease_id = uuid.uuid4().hex
            state = {"peer": peer, "context": context, "engine": None, "executions": set()}
            with self.engine_lock:
                if any(value["peer"] is peer and value["context"]["effect_id"] == payload["effect_id"]
                       for value in self.engine_leases.values()):
                    raise ValueError("engine opening is never replayed")
                if len(self.engine_leases) + len(self.engine_retirements) >= 16:
                    raise ValueError("engine lease limit reached")
                self.engine_leases[lease_id] = state
            self.outbound.put(event("abush_browser.engine_lease", "abush_browser", {
                **json.loads(json.dumps(context)), "lease_id": lease_id}))
            def emit(message):
                with self.engine_lock:
                    current = self.engine_leases.get(lease_id)
                    if current is not state or not peer.connected:
                        return
                    origin = json.loads(json.dumps(state["context"]))
                self.outbound.put(event("abush_browser.engine_cdp", "abush_browser", {
                    **origin, "lease_id": lease_id, "message": message["message"]}))
            try:
                def revoked():
                    with self.engine_lock:
                        if self.engine_leases.get(lease_id) is state:
                            self.engine_leases.pop(lease_id)
                engine = BrowserEngine(emit)
                engine.on_close = revoked
                if isinstance(engine, _browser_engine.BrowserEngine) and engine.closed:
                    revoked()
                with self.engine_lock:
                    current = self.engine_leases.get(lease_id)
                    if current is state:
                        state["engine"] = engine
                if current is not state:
                    engine.close()
                    raise ValueError("engine lease revoked during spawn")
                reply = engine.request("open", lease_id)
                with self.engine_lock:
                    if self.engine_leases.get(lease_id) is not state or not peer.connected:
                        raise ValueError("engine opening authority revoked")
                if reply.get("ok") is not True:
                    raise ValueError(reply.get("error", "engine initialization failed"))
                return {"lease_id": lease_id}
            except BaseException:
                self.close_engine_lease(lease_id)
                raise
        lease_id = payload["lease_id"]
        with self.engine_lock:
            state = self.engine_leases.get(lease_id)
            if state is None or state["peer"] is not peer:
                raise ValueError("unknown engine lease")
            # Rebinding requires a newly admitted operation on the identical page authority.
            previous = state["context"]
            if operation in {"engine_execute", "engine_cdp"} and previous != context and admitted:
                if any(previous[key] != context[key] for key in
                       ("remote_key", "connection_id", "principal_id", "executor_fence", "authorization")) or \
                        previous["command"]["page"] != context["command"]["page"]:
                    raise ValueError("engine lease authority mismatch")
                state["context"] = context
            elif previous != context:
                raise ValueError("engine lease context mismatch")
            engine = state["engine"]
            if operation == "engine_execute":
                identity = (payload["request_id"], payload["effect_id"])
                if identity in state["executions"]:
                    raise ValueError("engine execution is never replayed")
                if len(state["executions"]) >= 4096:
                    raise ValueError("engine execution limit reached; renew authorization")
                state["executions"].add(identity)
        if operation == "engine_close":
            self.close_engine_lease(lease_id)
            return {}
        if engine is None:
            raise ValueError("engine lease not ready")
        if operation == "engine_cdp":
            if not isinstance(payload["message"], dict):
                raise ValueError("invalid CDP message")
            engine.send({"type": "cdp", "lease_id": lease_id, "message": payload["message"]})
            return {}
        command = payload["command"]
        action = ({"type": "navigate", "url": command["url"]}
                  if command["command"] == "navigate" else command["action"])
        try:
            reply = engine.request("execute", lease_id, action=action)
        except (OSError, TimeoutError):
            self.close_engine_lease(lease_id)
            raise
        with self.engine_lock:
            if self.engine_leases.get(lease_id) is not state or state["context"] != context or \
                    not peer.connected:
                raise ValueError("engine output authority revoked")
        if reply.get("ok") is True and valid_result(reply.get("result")):
            return {"ok": True, "result": reply["result"], "error": None}
        return {"ok": False, "result": None, "error": str(reply.get("error") or "invalid engine result")}

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
            replacement = self._select(remote_key, exclude={peer.key})
            if replacement is None:
                self.remote_changed(peer)

    @staticmethod
    def _same_principal_instance(pinned, candidate):
        pinned_principal, pinned_owner = pinned
        candidate_principal, candidate_owner = candidate
        return pinned_principal == candidate_principal and \
            pinned_owner["daemon_instance_id"] == candidate_owner["daemon_instance_id"]

    def _select_candidate(self, remote_key, exclude=frozenset()):
        for metadata in self.candidates.get(remote_key, []):
            key = (remote_key, metadata["connection_id"])
            if key in exclude:
                continue
            peer = RemotePeer(self, metadata)
            logging.info("browser peer candidate selected remote=%s connection=%s",
                         remote_key, metadata["connection_id"])
            if peer.connect():
                identity = (peer.principal_id, peer.owner_display)
                pinned = self.principals.get(remote_key)
                if pinned is None:
                    self.principals[remote_key] = identity
                    self._bind_completion_history(peer)
                    return peer
                if self._same_principal_instance(pinned, identity):
                    self.principals[remote_key] = identity
                    self._bind_completion_history(peer)
                    return peer
                logging.warning(
                    "browser peer principal replacement rejected remote=%s connection=%s "
                    "expected=%s actual=%s", remote_key, metadata["connection_id"], pinned,
                    identity)
                peer.close(emit=False)
            logging.info("browser peer candidate failed remote=%s connection=%s",
                         remote_key, metadata["connection_id"])
        return None

    def _bind_completion_history(self, peer):
        key = (peer.metadata["remote_key"], peer.principal_id,
               peer.owner_display["daemon_instance_id"])
        state = self.completion_history.setdefault(
            key, ({}, {}, threading.RLock()))
        peer.completions, peer.accepted_completions, peer.transport_lock = state

    def _activate_after_remote_state(self, peer):
        self.remote_changed(peer)
        barrier = getattr(self, "write_barrier", None)
        if callable(barrier):
            barrier = barrier()
            self.outbound.put((None, barrier))
        if barrier is not None and not barrier.wait(2):
            peer.close(emit=False)
            raise TimeoutError("remote state was not written before peer replay activation")
        peer.activate()

    def _select(self, remote_key, exclude=frozenset()):
        current = self.peers.get(remote_key)
        if current and current.connected:
            return current
        peer = self._select_candidate(remote_key, exclude)
        if peer:
            self.peers[remote_key] = peer
            self._activate_after_remote_state(peer)
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
                    logging.info(
                        "browser peer active candidate disappeared remote=%s connection=%s",
                                 remote_key, peer.metadata["connection_id"])
                    replacement = self._select_candidate(remote_key, exclude={peer.key})
                    if replacement:
                        self.peers[remote_key] = replacement
                        self._activate_after_remote_state(replacement)
                        peer.close(emit=False)
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

    def close(self, deadline=None):
        deadline = deadline or time.monotonic() + SHUTDOWN_TIMEOUT
        with self.engine_lock:
            leases = list(self.engine_leases)
        failures = []
        for lease_id in leases:
            try:
                self.close_engine_lease(lease_id, deadline)
            except OSError as error:
                failures.append(error)
        with self.lock:
            peers = list(self.peers.values())
            self.peers.clear()
            self.candidates.clear()
        for peer in peers:
            peer.close(deadline=deadline)
        with self.engine_lock:
            retirements = list(self.engine_retirements)
        for worker in retirements:
            worker.join(timeout=max(0, deadline - time.monotonic()))
        with self.engine_lock:
            failures.extend(self.engine_shutdown_errors)
        if failures:
            raise OSError("engine shutdown failed: " + "; ".join(map(str, failures)))

    def peer(self, payload, completion_type=None):
        remote_key = payload.get("remote_key")
        connection_id = payload.get("connection_id")
        principal_id = payload.get("principal_id")
        if not isinstance(remote_key, str) or not REMOTE_KEY_RE.fullmatch(remote_key) or \
                not isinstance(connection_id, str) or not CONNECTION_ID_RE.fullmatch(connection_id) or \
                not isinstance(principal_id, str) or not principal_id:
            raise ValueError("invalid native peer identity")
        with self.lock:
            peer = self.peers.get(remote_key)
        if peer is None or not peer.connected:
            raise ValueError("remote is unavailable")
        if principal_id != peer.principal_id:
            raise ValueError("principal mismatch")
        if peer.metadata["connection_id"] != connection_id and completion_type is None:
            raise ValueError("remote is unavailable")
        return peer

    def x11_binding(self):
        try:
            from . import x11_binding
        except (ImportError, ValueError):
            import importlib.util
            path = pathlib.Path(__file__).with_name("x11_binding.py")
            spec = importlib.util.spec_from_file_location("external_links_x11_binding", path)
            if spec is None or spec.loader is None:
                raise ImportError("cannot load X11 binding module")
            x11_binding = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(x11_binding)
        return x11_binding

    def _durable_payload(self, payload, fields, error):
        if set(payload) != fields:
            raise ValueError(error)
        return self.peer(payload)

    def handle_request(self, message):
        request = validate_request(message)
        service = request["service"]
        operation = request["operation"]
        payload = request["payload"]
        if service == "ordinary_open" and operation == "prepare":
            deadline_ms = _ordinary_deadline(payload, {"nonce"})
            deadline = time.monotonic() + (deadline_ms / 1000 - time.time())
            binding = self.x11_binding()
            if self.browser is None:
                raise ValueError("trusted ordinary browser identity is unavailable")
            identity = binding.browser_incarnation(self.browser["pid"])
            if identity is None or identity[2] != self.browser["starttime"]:
                raise ValueError("ordinary browser incarnation changed")
            try:
                return binding.prepare_ordinary_window(_without_deadline(payload),
                                                       expected_identity=identity,
                                                       deadline=deadline)
            except binding.X11Unavailable as error:
                logging.warning("ordinary prepare failed error=%s", error)
                raise
        if service == "ordinary_open" and operation == "ordinary_file_navigate":
            deadline_ms = _ordinary_deadline(payload, {"nonce", "xid", "url"})
            binding = self.x11_binding()
            result = navigate_ordinary_file(
                _without_deadline(payload),
                lambda identity: binding.verify_prepared_ordinary_window(identity),
            )
            return {**result, "deadline_ms": deadline_ms}
        if service == "ordinary_open" and operation == "activate":
            deadline_ms = _ordinary_deadline(payload, {"nonce", "xid"})
            deadline = time.monotonic() + (deadline_ms / 1000 - time.time())
            return self.x11_binding().activate_ordinary_window(
                _without_deadline(payload), deadline=deadline)
        if service == "ordinary_open" and operation == "ordinary_open_completion":
            if self.ordinary_open is None:
                raise ValueError("ordinary-open broker is unavailable")
            self.ordinary_open.complete(payload)
            return {}
        if service == "external_links" and operation == "open":
            submit = self.ordinary_open.submit if self.ordinary_open is not None else None
            result = handle_message(payload, ordinary_submit=submit)
            if not result.get("ok"):
                completion = result.get("completion", "not_applied")
                raise ValueError(f"external link completed as {completion}")
            return {"completion": result.get("completion", "applied")}
        if service == "external_links" and operation == "scroll":
            result = handle_message({"command": "scroll", **payload})
            if not result.get("ok"):
                raise ValueError("failed to scroll page")
            return {}
        if service == "abush_browser" and operation == "list_remotes":
            return {"remotes": self.list_remotes()}
        if service == "abush_browser" and operation == "refresh_remotes":
            return {"remotes": self.refresh()}
        if service == "abush_browser" and operation == "workspace_state":
            peer = self._durable_payload(payload, {
                "remote_key", "connection_id", "principal_id", "request_id",
                "executor_fence", "state",
            }, "invalid workspace state payload")
            return peer.workspace_state(
                payload["request_id"], payload["executor_fence"], payload["state"])
        if service == "abush_browser" and operation == "executor_completion":
            if set(payload) != {
                "remote_key", "connection_id", "principal_id", "request_id", "effect_id",
                "executor_fence", "completion", "result", "error",
            }:
                raise ValueError("invalid executor completion payload")
            return self.peer(payload, "completion").complete(
                payload.get("request_id"), payload.get("effect_id"),
                payload.get("executor_fence"), payload.get("completion"),
                payload.get("result"), payload.get("error"),
            )
        if service == "abush_browser" and operation == "cancellation_completion":
            if set(payload) != {
                "remote_key", "connection_id", "principal_id", "request_id", "effect_id",
                "executor_fence", "completion",
            }:
                raise ValueError("invalid cancellation completion payload")
            return self.peer(payload, "cancellation_completed").complete_cancellation(
                payload.get("request_id"), payload.get("effect_id"),
                payload.get("executor_fence"), payload.get("completion"))
        if service == "abush_browser" and operation.startswith("engine_"):
            return self.engine_request(operation, payload)
        raise ValueError("unsupported native operation")


class NativeBrokerHost:
    def __init__(self, input_stream=None, output_stream=None, remote_root=None,
                 ordinary_open_factory=OrdinaryOpenServer, browser=None):
        self.input = input_stream or sys.stdin.buffer
        self.output = output_stream or sys.stdout.buffer
        self.outbound = BoundedQueue(on_full=self._fatal)
        self.browser = browser or native_host_browser()
        if self.browser is None:
            if ordinary_open_factory is not None:
                raise ValueError("native host has no Chrome browser identity")
            self.browser = {
                "pid": 0,
                "starttime": "0",
                "profile": pathlib.Path(),
                "browser_boot_id": "test-no-browser",
            }
        self.bootstrap_requests = []
        self.ordinary_open = None
        expected_profile = pathlib.Path(os.environ.get(
            "ABUSH_CHROME_PROFILE", pathlib.Path.home() / ".config/google-chrome-main"
        )).expanduser().resolve()
        if ordinary_open_factory is not None and self.browser["profile"] == expected_profile:
            claimed = claim_ordinary_launch_leases(self.browser["profile"])
            leases = [entry["lease"] for entry in claimed]
            self.bootstrap_requests = [ordinary_open_request(
                lease["url"], request_id=lease["request_id"],
                deadline_ms=lease["deadline_ms"],
            ) for lease in leases]
            claims = {entry["lease"]["request_id"]: entry["claim_path"] for entry in claimed}
            self.ordinary_open = ordinary_open_factory(
                self.outbound, bootstrap_requests=self.bootstrap_requests,
                bootstrap_claims=claims)
        self.broker = BrowserBroker(remote_root=remote_root, outbound=self.outbound,
                                    ordinary_open=self.ordinary_open,
                                    write_barrier=threading.Event, browser=self.browser)
        self.refresh_stop = threading.Event()
        self.refresh_wakeup = threading.Event()
        self.refresh_interval = 1.0
        self.refresh_started = threading.Event()
        self.refresh_thread = threading.Thread(target=self._refresh_loop, daemon=True)
        self.writer_error = None
        self.writer = threading.Thread(target=self._write_loop, daemon=True)
        self.request_workers = set()
        self.request_workers_lock = threading.Lock()
        self.fatal_error = None
        self.input_queue = queue.Queue(maxsize=1)
        self.input_stop = threading.Event()
        self.input_thread = threading.Thread(target=self._read_loop, daemon=True)
        self.write_started = None
        self.read_started = None
        self.worker_classes = {}
        self.normal_workers = 0
        self.worker_bytes = 0
        self.normal_worker_bytes = 0

    def _fatal(self, error):
        self.fatal_error = error
        logging.error("native transport failed closed: %s", error)
        self.refresh_stop.set()
        self.refresh_wakeup.set()

    def _read_loop(self):
        try:
            while not self.input_stop.is_set():
                message = self._receive()
                self.read_started = None
                while not self.input_stop.is_set():
                    try:
                        self.input_queue.put(message, timeout=0.1)
                        break
                    except queue.Full:
                        continue
                if message is None:
                    return
        except (OSError, EOFError, ValueError) as error:
            self._fatal(error)

    def _receive(self):
        try:
            fd = self.input.fileno()
        except (AttributeError, OSError, ValueError):
            fd = None
        if type(fd) is not int:
            return read_message(self.input, on_frame=lambda: setattr(
                self, "read_started", time.monotonic()))
        os.set_blocking(fd, False)
        data = bytearray()
        size = 4
        deadline = None
        while len(data) < size:
            if self.input_stop.is_set():
                return None
            remaining = 0.05 if deadline is None else min(0.05, deadline - time.monotonic())
            if remaining <= 0:
                raise TimeoutError("native frame assembly deadline exceeded")
            if not select.select([fd], [], [], remaining)[0]:
                continue
            try:
                part = os.read(fd, size - len(data))
            except BlockingIOError:
                continue
            if not part:
                if not data:
                    return None
                raise EOFError("native frame truncated")
            if deadline is None:
                deadline = time.monotonic() + NATIVE_IO_TIMEOUT
            data.extend(part)
            if len(data) == 4 and size == 4:
                length = struct.unpack("<I", data)[0]
                if not 0 < length <= MAX_MESSAGE_SIZE:
                    raise ValueError("invalid native frame size")
                size += length
        return json.loads(bytes(data[4:]).decode("utf-8"))

    def _send(self, message):
        try:
            fd = self.output.fileno()
        except (AttributeError, OSError, ValueError):
            fd = None
        if type(fd) is not int:
            send_message(message, self.output)
            return
        payload = json.dumps(message, separators=(",", ":")).encode()
        if len(payload) > MAX_MESSAGE_SIZE:
            raise ValueError("native message is too large")
        frame = memoryview(struct.pack("<I", len(payload)) + payload)
        deadline = time.monotonic() + NATIVE_IO_TIMEOUT
        os.set_blocking(fd, False)
        while frame:
            remaining = deadline - time.monotonic()
            if remaining <= 0 or not select.select([], [fd], [], remaining)[1]:
                raise TimeoutError("native stdout write deadline exceeded")
            try:
                frame = frame[os.write(fd, frame):]
            except BlockingIOError:
                continue

    def bootstrap_event(self):
        return event("bootstrap", "native_host", {
            "browser_boot_id": self.browser["browser_boot_id"],
            "browser_pid": self.browser["pid"],
            "browser_starttime": self.browser["starttime"],
            "ordinary_launch_leases": self.bootstrap_requests,
        })

    def _refresh_loop(self):
        self.refresh_started.set()
        while not self.refresh_stop.is_set():
            try:
                self.broker.refresh()
            except (OSError, ValueError, queue.Full) as error:
                logging.info("browser remote refresh failed error=%s", error)
            self.refresh_wakeup.wait(self.refresh_interval)
            self.refresh_wakeup.clear()

    def _write_loop(self):
        try:
            while True:
                queued = self.outbound.get()
                if queued is None:
                    return
                message, barrier = queued if isinstance(queued, tuple) else (queued, None)
                if message is not None:
                    self.write_started = time.monotonic()
                    self._send(message)
                    self.write_started = None
                if barrier is not None:
                    barrier.set()
        except (OSError, ValueError, TypeError, queue.Full) as error:
            self.writer_error = error
            self._fatal(error)

    def _respond(self, message):
        request_id = message.get("id") if isinstance(message, dict) else "invalid"
        try:
            request = validate_request(message)
            if request["service"] == "native_host" and request["operation"] == "bootstrap":
                if request["payload"]:
                    raise ValueError("native bootstrap request payload must be empty")
                payload = self.bootstrap_event()["payload"]
            else:
                payload = self.broker.handle_request(request)
            self.outbound.put(response(request_id, payload=payload))
        except MemoryError as error:
            self.fatal_error = error
            raise
        except queue.Full as error:
            self._fatal(error)
        except Exception as error:
            try:
                self.outbound.put(response(request_id, error=error), control=True)
            except queue.Full as full:
                self._fatal(full)
        finally:
            with self.request_workers_lock:
                current = threading.current_thread()
                self.request_workers.discard(current)
                admission = self.worker_classes.pop(current, None)
                if admission is not None:
                    normal, size = admission
                    self.normal_workers -= int(normal)
                    self.worker_bytes -= size
                    self.normal_worker_bytes -= size if normal else 0

    def _is_blocking_request(self, message):
        if not isinstance(message, dict):
            return False
        service = message.get("service")
        operation = message.get("operation")
        return (service == "external_links" and operation == "open") or \
            (service == "abush_browser" and operation in {
                "workspace_state", "executor_completion", "cancellation_completion",
                "engine_open", "engine_execute", "engine_close",
            })

    def _dispatch(self, message):
        if not self._is_blocking_request(message):
            self._respond(message)
            return
        worker = threading.Thread(target=self._respond, args=(message,), daemon=True)
        control = message.get("operation") in {
            "engine_close", "cancellation_completion", "executor_completion"}
        size = len(json.dumps(message, separators=(",", ":")).encode())
        with self.request_workers_lock:
            full = len(self.request_workers) >= MAX_REQUEST_WORKERS + RESERVED_REQUEST_WORKERS or \
                self.worker_bytes + size > 5 * MAX_MESSAGE_SIZE or \
                (not control and (self.normal_workers >= MAX_REQUEST_WORKERS or
                                  self.normal_worker_bytes + size > 4 * MAX_MESSAGE_SIZE))
            if not full:
                self.request_workers.add(worker)
                self.worker_classes[worker] = (not control, size)
                self.normal_workers += int(not control)
                self.worker_bytes += size
                self.normal_worker_bytes += size if not control else 0
        if full:
            self.outbound.put(response(message.get("id"), error=ValueError(
                "native request admission exhausted")), control=True)
            return
        worker.start()

    def run(self):
        self.writer.start()
        try:
            if self.ordinary_open is not None:
                self.ordinary_open.start()
            self.outbound.put(self.bootstrap_event())
            self.broker.refresh()
            self.refresh_thread.start()
            self.input_thread.start()
            while self.writer_error is None and self.fatal_error is None:
                if self.read_started is not None and \
                        time.monotonic() - self.read_started >= NATIVE_IO_TIMEOUT:
                    self._fatal(TimeoutError("native frame assembly deadline exceeded"))
                    break
                if self.write_started is not None and \
                        time.monotonic() - self.write_started >= NATIVE_IO_TIMEOUT:
                    self._fatal(TimeoutError("native stdout write deadline exceeded"))
                    break
                try:
                    message = self.input_queue.get(timeout=0.05)
                except queue.Empty:
                    continue
                if message is None:
                    break
                self._dispatch(message)
        finally:
            deadline = time.monotonic() + SHUTDOWN_TIMEOUT
            self.input_stop.set()
            self.refresh_stop.set()
            self.refresh_wakeup.set()
            if self.ordinary_open is not None:
                try:
                    self.ordinary_open.close(deadline=deadline)
                except (OSError, ValueError) as error:
                    self._fatal(error)
            try:
                self.broker.close(deadline=deadline)
            except (OSError, ValueError) as error:
                self._fatal(error)
            if self.refresh_thread.ident is not None:
                self.refresh_thread.join(timeout=max(0, deadline - time.monotonic()))
            with self.request_workers_lock:
                workers = list(self.request_workers)
            for worker in workers:
                worker.join(timeout=max(0, deadline - time.monotonic()))
            if self.fatal_error is None:
                self.outbound.put(None)
            else:
                self.outbound.close()
            self.writer.join(timeout=max(0, deadline - time.monotonic()))
            if self.writer.is_alive():
                self._fatal(TimeoutError("native writer did not exit before shutdown deadline"))
                self.outbound.close()
            if self.input_thread.ident is not None:
                self.input_thread.join(timeout=max(0, deadline - time.monotonic()))
        if self.fatal_error is not None:
            raise self.fatal_error


def main():
    NativeBrokerHost().run()


if __name__ == "__main__":
    main()
