#!/usr/bin/env python3

import json
import logging
import pathlib
import struct
import subprocess
import sys
import time
import urllib.request
from urllib.parse import urlsplit

from websockets.exceptions import WebSocketException
from websockets.sync.client import connect

MAX_MESSAGE_SIZE = 1024 * 1024
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


def read_message():
    header = sys.stdin.buffer.read(4)
    if not header:
        return None
    if len(header) != 4:
        raise EOFError
    size = struct.unpack("<I", header)[0]
    if size > MAX_MESSAGE_SIZE:
        raise ValueError("native message is too large")
    payload = sys.stdin.buffer.read(size)
    if len(payload) != size:
        raise EOFError
    return json.loads(payload)


def send_message(message):
    payload = json.dumps(message).encode()
    sys.stdout.buffer.write(struct.pack("<I", len(payload)) + payload)
    sys.stdout.buffer.flush()


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


def main():
    while True:
        message = read_message()
        if message is None:
            return
        send_message(handle_message(message))


if __name__ == "__main__":
    main()
