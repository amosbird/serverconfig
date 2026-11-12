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
PEER_PROTOCOL_VERSION = 7
BROWSER_ERROR_CODES = {
    "unknown_principal", "unknown_connection", "stale_session", "stale_authority",
    "human_controlled", "missing_capability", "capability_grant_mismatch", "backpressure",
    "stale_window", "stale_page", "page_quarantined", "operation_conflict",
    "unknown_effect", "stale_effect", "generation_exhausted", "executor_unavailable",
    "invalid_command",
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
    if group and reuse_tab(group, hostname, url):
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


def command_profile(arguments):
    prefix = b"--user-data-dir="
    for index, argument in enumerate(arguments):
        if argument.startswith(prefix):
            return os.fsdecode(argument[len(prefix):])
        if argument == b"--user-data-dir" and index + 1 < len(arguments):
            return os.fsdecode(arguments[index + 1])
    return None


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


def native_host_browser(pid=None, proc_root="/proc", max_depth=32):
    current = os.getppid() if pid is None else pid
    seen = set()
    for _ in range(max_depth):
        if current <= 1 or current in seen:
            break
        seen.add(current)
        profile = command_profile(command_line(current, proc_root))
        if profile is not None:
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

    def close(self):
        self.stopping.set()
        if self.server is not None:
            self.server.close()
        if self.thread is not None:
            self.thread.join(timeout=1)
        with self.lock:
            states = list(self.requests.values())
            workers = list(self.workers)
        for state in states:
            state["done"].set()
        for worker in workers:
            worker.join(timeout=1)
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
        self.capabilities = {}
        self.pending = {}
        self.ensure_pending = {}
        self.ensure_cancellations = {}
        self.ensure_cancellation_completed = {}
        self.pending_cancellations = {}
        self.topology_requests = {}
        self.retire_requests = {}
        self.transport_lock = threading.RLock()
        self.topology_timeout = 15
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
                "capabilities": {"cancellation": True},
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
            "type", "request_id", "principal_id", "executor_fence", "owner_display"
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
            "type", "request_id", "principal_id", "executor_fence", "owner_display"
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
        if principal_id != self.principal_id or fence != self.executor_fence or \
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
            value["epoch"] > 0

    @staticmethod
    def _valid_window_ref(value):
        return isinstance(value, dict) and set(value) == {
            "workspace_id", "window_id", "generation"
        } and all(isinstance(value[key], str) and value[key]
                  for key in ("workspace_id", "window_id")) and \
            isinstance(value["generation"], int) and not isinstance(value["generation"], bool) and \
            value["generation"] > 0

    @classmethod
    def _valid_page_ref(cls, value):
        return isinstance(value, dict) and set(value) == {"window", "page_id", "generation"} and \
            cls._valid_window_ref(value["window"]) and \
            isinstance(value["page_id"], str) and value["page_id"] and \
            isinstance(value["generation"], int) and not isinstance(value["generation"], bool) and \
            value["generation"] > 0

    def _handle_ensure_managed_page(self, message):
        if not self._exact_fields(message, {
            "type", "request_id", "ensure_id", "workspace_id", "executor_fence",
            "expected_page"
        }):
            raise ValueError("invalid ensure managed page")
        request_id = message.get("request_id")
        ensure_id = message.get("ensure_id")
        workspace_id = message.get("workspace_id")
        fence = message.get("executor_fence")
        expected_page = message.get("expected_page")
        if not isinstance(request_id, str) or not request_id or \
                not isinstance(ensure_id, str) or not ensure_id or \
                not isinstance(workspace_id, str) or not workspace_id or \
                (expected_page is not None and not self._valid_page_ref(expected_page)) or \
                (expected_page is not None and
                 expected_page["window"]["workspace_id"] != workspace_id) or \
                not self._valid_executor_fence(fence) or fence != self.executor_fence:
            raise ValueError("invalid ensure managed page correlation")
        if ensure_id in self.ensure_pending:
            raise ValueError("duplicate ensure managed page")
        self.ensure_pending[ensure_id] = {
            "request_id": request_id, "workspace_id": workspace_id, "fence": fence,
            "expected_page": expected_page,
        }
        self.broker.outbound.put(event("abush_browser.ensure_managed_page", "abush_browser", {
            "remote_key": self.metadata["remote_key"],
            "connection_id": self.metadata["connection_id"],
            "principal_id": self.principal_id,
            "workspace_id": workspace_id,
            "request_id": request_id,
            "ensure_id": ensure_id,
            "executor_fence": fence,
            "owner_display": self.owner_display,
            "connection_display_name": self.metadata["display_name"],
            "expected_page": expected_page,
        }))

    def _handle_retire_managed_page(self, message):
        if not self._exact_fields(message, {
            "type", "request_id", "retire_id", "ensure_id", "executor_fence",
            "origin_executor_fence", "window_id", "page_id"
        }):
            raise ValueError("invalid retire managed page")
        request_id = message.get("request_id")
        retire_id = message.get("retire_id")
        ensure_id = message.get("ensure_id")
        fence = message.get("executor_fence")
        origin_fence = message.get("origin_executor_fence")
        if not all(isinstance(value, str) and value for value in (
                request_id, retire_id, ensure_id, message.get("window_id"),
                message.get("page_id"))) or not self._valid_executor_fence(fence) or \
                fence != self.executor_fence or not self._valid_executor_fence(origin_fence):
            raise ValueError("invalid retire managed page correlation")
        self.broker.outbound.put(event("abush_browser.retire_managed_page", "abush_browser", {
            "remote_key": self.metadata["remote_key"],
            "connection_id": self.metadata["connection_id"],
            "principal_id": self.principal_id,
            "request_id": request_id,
            "retire_id": retire_id,
            "ensure_id": ensure_id,
            "executor_fence": fence,
            "origin_executor_fence": origin_fence,
            "window_id": message["window_id"],
            "page_id": message["page_id"],
        }))

    def _handle_cancel_ensure_managed_page(self, message):
        if not self._exact_fields(message, {
            "type", "request_id", "ensure_id", "executor_fence"
        }):
            raise ValueError("invalid cancel ensure managed page")
        request_id = message.get("request_id")
        ensure_id = message.get("ensure_id")
        fence = message.get("executor_fence")
        if not isinstance(request_id, str) or not request_id or \
                not isinstance(ensure_id, str) or not ensure_id or \
                not self._valid_executor_fence(fence):
            raise ValueError("invalid cancel ensure managed page correlation")
        pending = self.ensure_pending.get(ensure_id)
        if fence != self.executor_fence or pending is None or \
                pending["request_id"] != request_id or \
                pending["fence"] != fence:
            return
        self.ensure_pending.pop(ensure_id)
        self.ensure_cancellations[ensure_id] = pending
        self.broker.outbound.put(event(
            "abush_browser.cancel_ensure_managed_page", "abush_browser", {
                "remote_key": self.metadata["remote_key"],
                "connection_id": self.metadata["connection_id"],
                "principal_id": self.principal_id,
                "request_id": request_id,
                "ensure_id": ensure_id,
                "executor_fence": fence,
            }))

    def complete_ensure_cancellation(self, principal_id, request_id, ensure_id, fence):
        if principal_id != self.principal_id or fence != self.executor_fence:
            raise ValueError("stale ensure cancellation completion")
        pending = self.ensure_cancellations.get(ensure_id)
        identity = {"request_id": request_id, "ensure_id": ensure_id, "fence": fence}
        completed = self.ensure_cancellation_completed.get(request_id)
        if completed is not None:
            if completed != identity:
                raise ValueError("conflicting ensure cancellation completion")
            self.send({
                "type": "ensure_managed_page_cancellation_completed",
                "request_id": request_id,
                "ensure_id": ensure_id,
                "executor_fence": fence,
            })
            return
        if pending is None or pending["request_id"] != request_id or pending["fence"] != fence:
            raise ValueError("uncorrelated ensure cancellation completion")
        self.ensure_cancellations.pop(ensure_id)
        self.ensure_cancellation_completed[request_id] = identity
        while len(self.ensure_cancellation_completed) > 256:
            self.ensure_cancellation_completed.pop(next(iter(self.ensure_cancellation_completed)))
        self.send({
            "type": "ensure_managed_page_cancellation_completed",
            "request_id": request_id,
            "ensure_id": ensure_id,
            "executor_fence": fence,
        })

    def complete_ensure(self, principal_id, workspace_id, request_id, ensure_id, fence, completion):
        if principal_id != self.principal_id or fence != self.executor_fence:
            raise ValueError("stale ensure managed page completion")
        pending = self.ensure_pending.get(ensure_id)
        if pending is None or pending != {
                "request_id": request_id, "workspace_id": workspace_id, "fence": fence,
                "expected_page": pending.get("expected_page")}:
            raise ValueError("uncorrelated ensure managed page completion")
        if not isinstance(completion, dict):
            raise ValueError("invalid ensure managed page completion")
        outcome = completion.get("outcome")
        if outcome == "created":
            valid = set(completion) == {"outcome", "window_id", "page_id"} and \
                all(isinstance(completion[key], str) and completion[key]
                    for key in ("window_id", "page_id"))
        elif outcome == "rejected":
            valid = set(completion) == {"outcome", "code"} and \
                completion.get("code") in BROWSER_ERROR_CODES
        else:
            valid = False
        if not valid:
            raise ValueError("invalid ensure managed page completion")
        self.ensure_pending.pop(ensure_id)
        self.send({
            "type": "ensure_managed_page_completed",
            "request_id": request_id,
            "ensure_id": ensure_id,
            "executor_fence": fence,
            "completion": completion,
        })

    def complete_cancellation(self, request_id, effect_id, fence, completion):
        if fence != self.executor_fence or completion not in {"not_applied", "unknown"}:
            raise ValueError("invalid cancellation completion")
        pending = self.pending_cancellations.get(effect_id)
        if pending is None or pending["request_id"] != request_id or pending["fence"] != fence:
            raise ValueError("uncorrelated cancellation completion")
        self.pending_cancellations.pop(effect_id)
        self.pending.pop(effect_id, None)
        self.send({
            "type": "cancellation_completed",
            "request_id": request_id,
            "effect_id": effect_id,
            "executor_fence": fence,
            "completion": completion,
        })

    def complete(self, request_id, effect_id, fence, completion):
        if fence != self.executor_fence or completion not in {"applied", "not_applied", "unknown"}:
            raise ValueError("invalid executor completion")
        pending = self.pending.get(effect_id)
        if pending is None or pending["request_id"] != request_id:
            raise ValueError("uncorrelated executor completion")
        self.pending.pop(effect_id)
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
        if normalized_type == "ensure_managed_page":
            self._handle_ensure_managed_page(message)
            return
        if normalized_type == "retire_managed_page":
            self._handle_retire_managed_page(message)
            return
        if normalized_type == "cancel_ensure_managed_page":
            self._handle_cancel_ensure_managed_page(message)
            return
        if normalized_type == "topology_report_response":
            self._handle_topology_report_response(message)
            return
        if normalized_type == "retire_managed_page_ack":
            self._handle_retire_managed_page_ack(message)
            return
        raise ValueError("unknown peer message")

    @staticmethod
    def _valid_topology_report_result(result):
        if not isinstance(result, dict):
            return False
        outcome = result.get("outcome")
        if outcome in {"applied", "already_converged"}:
            return set(result) == {"outcome"}
        return outcome == "rejected" and set(result) == {"outcome", "code"} and \
            result.get("code") in BROWSER_ERROR_CODES

    @staticmethod
    def _valid_retire_completion(completion):
        if not isinstance(completion, dict):
            return False
        outcome = completion.get("outcome")
        if outcome in {"retired", "already_retired"}:
            return set(completion) == {"outcome"}
        return outcome == "rejected" and set(completion) == {"outcome", "code"} and \
            completion.get("code") in BROWSER_ERROR_CODES

    def _transport_request(self, requests, message, identity, timeout, timeout_message):
        request_id = message["request_id"]
        state = {"identity": identity, "done": threading.Event(), "response": None}
        with self.transport_lock:
            if request_id in requests:
                raise ValueError("duplicate in-flight peer delivery request")
            requests[request_id] = state
            try:
                self.send(message)
            except Exception:
                requests.pop(request_id, None)
                raise
        if not state["done"].wait(timeout):
            with self.transport_lock:
                requests.pop(request_id, None)
            raise TimeoutError(timeout_message)
        with self.transport_lock:
            requests.pop(request_id, None)
            result = state["response"]
        if result is None:
            raise TimeoutError(f"{timeout_message} was lost")
        return result

    def _resolve_transport_response(self, requests, request_id, identity, response):
        with self.transport_lock:
            state = requests.get(request_id)
            if state is None:
                return False
            if state["identity"] != identity:
                raise ValueError("uncorrelated durable peer response")
            state["response"] = response
            state["done"].set()
            return True

    def report_managed_page_moved_outside(
            self, request_id, report_id, fence, origin_fence, page, timeout=None):
        if not all(isinstance(value, str) and value for value in (request_id, report_id)) or \
                fence != self.executor_fence or not self._valid_executor_fence(origin_fence) or \
                not self._valid_page_ref(page):
            raise ValueError("invalid managed page moved outside report")
        message = {
            "type": "managed_page_moved_outside",
            "request_id": request_id,
            "report_id": report_id,
            "executor_fence": fence,
            "origin_executor_fence": origin_fence,
            "page": page,
        }
        identity = {"report_id": report_id, "fence": fence}
        return self._transport_request(
            self.topology_requests,
            message,
            identity,
            self.topology_timeout if timeout is None else timeout,
            "managed topology response timed out",
        )

    def _authenticated_event(self, name, payload):
        self.broker.outbound.put(event(name, "abush_browser", {
            "remote_key": self.metadata["remote_key"],
            "connection_id": self.metadata["connection_id"],
            "principal_id": self.principal_id,
            **payload,
        }))

    def _handle_topology_report_response(self, message):
        fields = {"type", "request_id", "report_id", "executor_fence",
                  "response_delivery_id", "result"}
        if not self._exact_fields(message, fields):
            raise ValueError("invalid topology report response")
        request_id = message.get("request_id")
        report_id = message.get("report_id")
        fence = message.get("executor_fence")
        delivery_id = message.get("response_delivery_id")
        result = message.get("result")
        if not all(isinstance(value, str) and value for value in (
                request_id, report_id, delivery_id)) or not self._valid_executor_fence(fence) or \
                fence != self.executor_fence or not self._valid_topology_report_result(result):
            raise ValueError("invalid topology report response correlation")
        response_message = {key: message[key] for key in fields if key != "type"}
        if self._resolve_transport_response(
                self.topology_requests,
                request_id,
                {"report_id": report_id, "fence": fence},
                response_message):
            return
        self._authenticated_event("abush_browser.topology_report_response", response_message)

    def topology_response_consumed(self, request_id, report_id, fence, delivery_id):
        if not all(isinstance(value, str) and value for value in (
                request_id, report_id, delivery_id)) or fence != self.executor_fence:
            raise ValueError("invalid topology response consumption")
        self.send({
            "type": "topology_response_consumed",
            "request_id": request_id,
            "report_id": report_id,
            "executor_fence": fence,
            "response_delivery_id": delivery_id,
        })
        return {
            "accepted": True,
            "request_id": request_id,
            "report_id": report_id,
            "response_delivery_id": delivery_id,
        }

    def complete_retire(
            self, request_id, retire_id, fence, origin_fence, completion, timeout=None):
        if not all(isinstance(value, str) and value for value in (request_id, retire_id)) or \
                fence != self.executor_fence or not self._valid_executor_fence(origin_fence) or \
                not self._valid_retire_completion(completion):
            raise ValueError("invalid retire managed page completion")
        message = {
            "type": "retire_managed_page_completed",
            "request_id": request_id,
            "retire_id": retire_id,
            "executor_fence": fence,
            "origin_executor_fence": origin_fence,
            "completion": completion,
        }
        identity = {
            "retire_id": retire_id,
            "fence": fence,
            "origin_fence": origin_fence,
            "completion": completion,
        }
        return self._transport_request(
            self.retire_requests,
            message,
            identity,
            self.topology_timeout if timeout is None else timeout,
            "retire managed page acknowledgement timed out",
        )

    def _handle_retire_managed_page_ack(self, message):
        fields = {"type", "request_id", "retire_id", "executor_fence",
                  "response_delivery_id", "origin_executor_fence", "completion"}
        if not self._exact_fields(message, fields):
            raise ValueError("invalid retire managed page acknowledgement")
        request_id = message.get("request_id")
        retire_id = message.get("retire_id")
        fence = message.get("executor_fence")
        delivery_id = message.get("response_delivery_id")
        origin_fence = message.get("origin_executor_fence")
        completion = message.get("completion")
        if not all(isinstance(value, str) and value for value in (
                request_id, retire_id, delivery_id)) or not self._valid_executor_fence(fence) or \
                fence != self.executor_fence or not self._valid_executor_fence(origin_fence) or \
                not self._valid_retire_completion(completion):
            raise ValueError("invalid retire managed page acknowledgement correlation")
        response_message = {key: message[key] for key in fields if key != "type"}
        expected = {
            "retire_id": retire_id,
            "fence": fence,
            "origin_fence": origin_fence,
            "completion": completion,
        }
        if self._resolve_transport_response(
                self.retire_requests, request_id, expected, response_message):
            return
        self._authenticated_event("abush_browser.retire_managed_page_ack", response_message)

    def retire_ack_consumed(self, request_id, retire_id, fence, delivery_id):
        if not all(isinstance(value, str) and value for value in (
                request_id, retire_id, delivery_id)) or fence != self.executor_fence:
            raise ValueError("invalid retire acknowledgement consumption")
        self.send({
            "type": "retire_managed_page_ack_consumed",
            "request_id": request_id,
            "retire_id": retire_id,
            "executor_fence": fence,
            "response_delivery_id": delivery_id,
        })
        return {
            "accepted": True,
            "request_id": request_id,
            "retire_id": retire_id,
            "response_delivery_id": delivery_id,
        }

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
        correlation = {"request_id": request_id, "fence": fence, "command": command}
        if effect_id in self.pending:
            if self.pending[effect_id] != correlation:
                raise ValueError("conflicting duplicate execute effect")
            return
        self.pending[effect_id] = correlation
        self.broker.outbound.put(event("abush_browser.execute", "abush_browser", {
            "remote_key": self.metadata["remote_key"],
            "connection_id": self.metadata["connection_id"],
            "principal_id": self.principal_id,
            "request_id": request_id,
            "effect_id": effect_id,
            "executor_fence": fence,
            "command": command,
        }))

    def _handle_cancel(self, message):
        request_id, effect_id, fence = self._correlation(message, {
            "type", "request_id", "effect_id", "executor_fence"
        })
        pending = self.pending.get(effect_id)
        if pending is None or pending["fence"] != fence or request_id == pending["request_id"]:
            raise ValueError("invalid cancel correlation")
        self.pending_cancellations[effect_id] = {"request_id": request_id, "fence": fence}
        self.broker.outbound.put(event("abush_browser.cancel", "abush_browser", {
            "remote_key": self.metadata["remote_key"],
            "connection_id": self.metadata["connection_id"],
            "principal_id": self.principal_id,
            "request_id": request_id,
            "execution_request_id": pending["request_id"],
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
        with self.transport_lock:
            transport_states = [
                *self.topology_requests.values(),
                *self.retire_requests.values(),
            ]
        for state in transport_states:
            state["done"].set()
        if self.reader and not from_reader and self.reader is not threading.current_thread():
            self.reader.join(timeout=1)
        if emit and was_connected:
            self.broker.remote_disconnected(self)


class BrowserBroker:
    def __init__(self, remote_root=None, outbound=None, ordinary_open=None):
        self.remote_root = pathlib.Path(remote_root) if remote_root else None
        self.outbound = outbound or queue.Queue()
        self.ordinary_open = ordinary_open
        self.peers = {}
        self.principals = {}
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
                    return peer
                if self._same_principal_instance(pinned, identity):
                    self.principals[remote_key] = identity
                    return peer
                logging.warning(
                    "browser peer principal replacement rejected remote=%s connection=%s "
                    "expected=%s actual=%s", remote_key, metadata["connection_id"], pinned,
                    identity)
                peer.close(emit=False)
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
                    logging.info(
                        "browser peer active candidate disappeared remote=%s connection=%s",
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
        principal_id = payload.get("principal_id")
        with self.lock:
            peer = self.peers.get(remote_key)
        if peer is None or not peer.connected or peer.metadata["connection_id"] != connection_id:
            raise ValueError("remote is unavailable")
        if principal_id != peer.principal_id:
            raise ValueError("principal mismatch")
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
            _ordinary_deadline(payload, {"nonce"})
            return self.x11_binding().prepare_ordinary_window(_without_deadline(payload))
        if service == "ordinary_open" and operation == "ordinary_file_navigate":
            deadline_ms = _ordinary_deadline(payload, {"nonce", "xid", "url"})
            binding = self.x11_binding()
            result = navigate_ordinary_file(
                _without_deadline(payload),
                lambda identity: binding.verify_prepared_ordinary_window(identity),
            )
            return {**result, "deadline_ms": deadline_ms}
        if service == "ordinary_open" and operation == "activate":
            _ordinary_deadline(payload, {"nonce", "xid"})
            return self.x11_binding().activate_ordinary_window(_without_deadline(payload))
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
        if service == "abush_browser" and operation == "ensure_managed_page_completion":
            if set(payload) != {
                "remote_key", "connection_id", "principal_id", "workspace_id", "request_id",
                "ensure_id", "executor_fence", "completion",
            }:
                raise ValueError("invalid ensure managed page completion payload")
            peer = self.peer(payload)
            peer.complete_ensure(
                payload.get("principal_id"), payload.get("workspace_id"),
                payload.get("request_id"), payload.get("ensure_id"),
                payload.get("executor_fence"), payload.get("completion"),
            )
            return {}
        if service == "abush_browser" and \
                operation == "ensure_managed_page_cancellation_completion":
            if set(payload) != {
                "remote_key", "connection_id", "principal_id", "request_id", "ensure_id",
                "executor_fence",
            }:
                raise ValueError("invalid ensure cancellation completion payload")
            self.peer(payload).complete_ensure_cancellation(
                payload.get("principal_id"), payload.get("request_id"), payload.get("ensure_id"),
                payload.get("executor_fence"))
            return {}
        if service == "abush_browser" and operation == "managed_page_moved_outside":
            peer = self._durable_payload(payload, {
                "remote_key", "connection_id", "principal_id", "request_id", "report_id",
                "executor_fence", "origin_executor_fence", "page",
            }, "invalid managed page moved outside payload")
            return peer.report_managed_page_moved_outside(
                payload["request_id"],
                payload["report_id"],
                payload["executor_fence"],
                payload["origin_executor_fence"],
                payload["page"],
            )
        if service == "abush_browser" and operation == "topology_response_consumed":
            peer = self._durable_payload(payload, {
                "remote_key", "connection_id", "principal_id", "request_id", "report_id",
                "executor_fence", "response_delivery_id",
            }, "invalid topology response consumption payload")
            return peer.topology_response_consumed(
                payload["request_id"], payload["report_id"], payload["executor_fence"],
                payload["response_delivery_id"])
        if service == "abush_browser" and operation == "retire_managed_page_completion":
            peer = self._durable_payload(payload, {
                "remote_key", "connection_id", "principal_id", "request_id", "retire_id",
                "executor_fence", "origin_executor_fence", "completion",
            }, "invalid retire managed page completion payload")
            return peer.complete_retire(
                payload["request_id"], payload["retire_id"], payload["executor_fence"],
                payload["origin_executor_fence"], payload["completion"])
        if service == "abush_browser" and operation == "retire_managed_page_ack_consumed":
            peer = self._durable_payload(payload, {
                "remote_key", "connection_id", "principal_id", "request_id", "retire_id",
                "executor_fence", "response_delivery_id",
            }, "invalid retire acknowledgement consumption payload")
            return peer.retire_ack_consumed(
                payload["request_id"], payload["retire_id"], payload["executor_fence"],
                payload["response_delivery_id"])
        if service == "abush_browser" and operation == "executor_completion":
            if set(payload) != {
                "remote_key", "connection_id", "principal_id", "request_id", "effect_id",
                "executor_fence", "completion",
            }:
                raise ValueError("invalid executor completion payload")
            self.peer(payload).complete(
                payload.get("request_id"), payload.get("effect_id"),
                payload.get("executor_fence"), payload.get("completion"),
            )
            return {}
        if service == "abush_browser" and operation == "cancellation_completion":
            if set(payload) != {
                "remote_key", "connection_id", "principal_id", "request_id", "effect_id",
                "executor_fence", "completion",
            }:
                raise ValueError("invalid cancellation completion payload")
            self.peer(payload).complete_cancellation(
                payload.get("request_id"), payload.get("effect_id"),
                payload.get("executor_fence"), payload.get("completion"))
            return {}
        if service == "abush_browser" and operation == "verify_x11_window":
            return self.x11_binding().verify_workspace_window(payload)
        if service == "abush_browser" and operation == "clear_x11_window":
            try:
                return self.x11_binding().clear_workspace_window(payload)
            except Exception as error:
                logging.info("browser X11 cleanup failed xid=%s error=%s",
                             payload.get("xid"), error)
                raise
        if service == "abush_browser" and operation == "bind_x11_window":
            try:
                return self.x11_binding().bind_workspace_window(payload)
            except Exception as error:
                logging.info("browser X11 bind failed remote=%s workspace=%s error=%s",
                             payload.get("remote_key"), payload.get("workspace_id"), error)
                raise
        raise ValueError("unsupported native operation")


class NativeBrokerHost:
    def __init__(self, input_stream=None, output_stream=None, remote_root=None,
                 ordinary_open_factory=OrdinaryOpenServer, browser=None):
        self.input = input_stream or sys.stdin.buffer
        self.output = output_stream or sys.stdout.buffer
        self.outbound = queue.Queue()
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
                                    ordinary_open=self.ordinary_open)
        self.refresh_stop = threading.Event()
        self.refresh_wakeup = threading.Event()
        self.refresh_interval = 1.0
        self.refresh_started = threading.Event()
        self.refresh_thread = threading.Thread(target=self._refresh_loop, daemon=True)
        self.writer_error = None
        self.writer = threading.Thread(target=self._write_loop, daemon=True)
        self.request_workers = set()
        self.request_workers_lock = threading.Lock()

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
            except (OSError, ValueError) as error:
                logging.info("browser remote refresh failed error=%s", error)
            self.refresh_wakeup.wait(self.refresh_interval)
            self.refresh_wakeup.clear()

    def _write_loop(self):
        try:
            while True:
                message = self.outbound.get()
                if message is None:
                    return
                send_message(message, self.output)
        except (OSError, ValueError) as error:
            self.writer_error = error

    def _respond(self, message):
        request_id = message.get("id") if isinstance(message, dict) else "invalid"
        try:
            payload = self.broker.handle_request(message)
            self.outbound.put(response(request_id, payload=payload))
        except (EnvelopeError, OSError, TimeoutError, ValueError,
                WebSocketException) as error:
            self.outbound.put(response(request_id, error=error))
        finally:
            with self.request_workers_lock:
                self.request_workers.discard(threading.current_thread())

    def _is_blocking_request(self, message):
        if not isinstance(message, dict):
            return False
        service = message.get("service")
        operation = message.get("operation")
        return (service == "external_links" and operation == "open") or \
            (service == "abush_browser" and operation in {
                "managed_page_moved_outside", "retire_managed_page_completion",
            })

    def _dispatch(self, message):
        if not self._is_blocking_request(message):
            self._respond(message)
            return
        worker = threading.Thread(target=self._respond, args=(message,), daemon=True)
        with self.request_workers_lock:
            self.request_workers.add(worker)
        worker.start()

    def run(self):
        self.writer.start()
        if self.ordinary_open is not None:
            self.ordinary_open.start()
        self.outbound.put(self.bootstrap_event())
        self.broker.refresh()
        self.refresh_thread.start()
        try:
            while self.writer_error is None:
                message = read_message(self.input)
                if message is None:
                    return
                self._dispatch(message)
        finally:
            self.refresh_stop.set()
            self.refresh_wakeup.set()
            self.refresh_thread.join(timeout=2)
            if self.ordinary_open is not None:
                self.ordinary_open.close()
            with self.request_workers_lock:
                workers = list(self.request_workers)
            for worker in workers:
                worker.join(timeout=1)
            self.broker.close()
            self.outbound.put(None)
            self.writer.join(timeout=2)


def main():
    NativeBrokerHost().run()


if __name__ == "__main__":
    main()
