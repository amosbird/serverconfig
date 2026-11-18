#!/usr/bin/env python3

import json
import os
import re
import selectors
import signal
import struct
import subprocess
import sys
import time
from urllib.parse import urlsplit

MAX_MESSAGE_SIZE = 16 * 1024 * 1024
BOOKMARK_OPEN_TIMEOUT = 35.0
BOOKMARK_DIAGNOSTIC_LIMIT = 4096
BOOKMARK_REQUEST_ID = re.compile(r"[0-9a-f]{32}\Z")
BOOKMARK_BROWSER = os.environ.get("ABUSH_CHROMIUM_LAUNCHER", "/home/amos/scripts/chromium")


def read_exact(stream, size):
    data = bytearray()
    while len(data) < size:
        chunk = stream.read(size - len(data))
        if not chunk:
            raise EOFError("native message ended early")
        data.extend(chunk)
    return bytes(data)


def read_message():
    header = sys.stdin.buffer.read(4)
    if not header:
        return None
    if len(header) != 4:
        raise EOFError("native message header ended early")
    size = struct.unpack("<I", header)[0]
    if size > MAX_MESSAGE_SIZE:
        raise ValueError(f"native message is too large: {size} bytes")
    return json.loads(read_exact(sys.stdin.buffer, size).decode("utf-8"))


def send_message(message):
    payload = json.dumps(message).encode("utf-8")
    sys.stdout.buffer.write(struct.pack("<I", len(payload)))
    sys.stdout.buffer.write(payload)
    sys.stdout.buffer.flush()


def rofi_select(param):
    command = ["rofi", "-dmenu", *param.get("rofi-opts", [])]
    result = subprocess.run(
        command,
        input="\n".join(param["opts"]),
        text=True,
        stdout=subprocess.PIPE,
        check=False,
    )
    return result.returncode, result.stdout.rstrip("\n")


def switch_tab(param):
    returncode, selected = rofi_select(param)
    if returncode != 0 or not selected:
        return ""

    try:
        index = param["opts"].index(selected)
    except ValueError:
        return "g " + selected

    tab_ids = param.get("tabIds", [])
    if index < len(tab_ids):
        return tab_ids[index]
    return selected.rsplit(" ::: ", 1)[-1]


def list_downloads(param):
    returncode, selected = rofi_select(param)
    if not selected or returncode not in (0, 10):
        return ""
    try:
        index = param["opts"].index(selected)
        download_id = param["downloadIds"][index]
    except (ValueError, IndexError, KeyError):
        return ""
    return {"action": "copy" if returncode == 0 else "open", "id": download_id}


def select_option(param):
    returncode, selected = rofi_select(param)
    return selected if returncode == 0 else ""


def wait_browser_open(process, cancelled):
    deadline = time.monotonic() + BOOKMARK_OPEN_TIMEOUT
    diagnostic = bytearray()
    pipe_open = True
    with selectors.DefaultSelector() as selector:
        selector.register(process.stderr, selectors.EVENT_READ)
        os.set_blocking(process.stderr.fileno(), False)
        while True:
            if cancelled():
                raise InterruptedError("bookmark opening was cancelled; outcome is unknown")
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError("browser completion deadline expired; outcome is unknown")
            for key, _ in selector.select(min(0.1, remaining)):
                try:
                    chunk = os.read(key.fd, 4096)
                except BlockingIOError:
                    continue
                if not chunk:
                    selector.unregister(key.fileobj)
                    pipe_open = False
                else:
                    diagnostic.extend(chunk)
                    del diagnostic[:-BOOKMARK_DIAGNOSTIC_LIMIT]
            returncode = process.poll()
            if returncode is not None and not pipe_open:
                if cancelled():
                    raise InterruptedError("bookmark opening was cancelled; outcome is unknown")
                return returncode, diagnostic.decode("utf-8", errors="replace").strip()


def stop_browser_open(process):
    if process.poll() is not None:
        return
    try:
        process.terminate()
    except ProcessLookupError:
        pass
    try:
        process.wait(timeout=1)
    except subprocess.TimeoutExpired:
        try:
            process.kill()
        except ProcessLookupError:
            pass
        process.wait(timeout=1)


def open_in_browser(message):
    request_id = message.get("request_id")
    response = {"version": 1, "info": "openInBrowser", "request_id": request_id,
                "completion": "not_started", "message": ""}
    process = None
    interrupted = False
    previous_handlers = {}

    def cancel(_signum, _frame):
        nonlocal interrupted
        interrupted = True

    try:
        if set(message) != {"version", "info", "request_id", "param"} or \
                type(message.get("version")) is not int or message["version"] != 1 or \
                not isinstance(request_id, str) or not BOOKMARK_REQUEST_ID.fullmatch(request_id):
            raise ValueError("invalid bookmark open request")
        param = message["param"]
        if not isinstance(param, dict) or set(param) != {"url"}:
            raise ValueError("invalid bookmark open parameters")
        url = param["url"]
        if not isinstance(url, str) or len(url) > 32768 or \
                any(ord(character) <= 32 or ord(character) == 127 for character in url):
            raise ValueError("invalid bookmark URL")
        parsed = urlsplit(url)
        if parsed.scheme not in {"http", "https"} or not parsed.hostname:
            raise ValueError("unsupported bookmark URL")
        parsed.port
        for signum in (signal.SIGTERM, signal.SIGINT):
            previous_handlers[signum] = signal.signal(signum, cancel)
        if interrupted:
            raise InterruptedError("bookmark opening was cancelled before launch")
        process = subprocess.Popen([BOOKMARK_BROWSER, url], stdin=subprocess.DEVNULL,
                                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        response["completion"] = "unknown"
        if interrupted:
            raise InterruptedError("bookmark opening was cancelled; outcome is unknown")
        returncode, diagnostic = wait_browser_open(process, lambda: interrupted)
        if interrupted:
            raise InterruptedError("bookmark opening was cancelled; outcome is unknown")
        if returncode != 0:
            raise RuntimeError(diagnostic or f"browser launcher exited with status {returncode}")
        response["completion"] = "completed"
    except (OSError, ValueError, RuntimeError) as error:
        response["message"] = str(error)[-BOOKMARK_DIAGNOSTIC_LIMIT:]
    finally:
        if process is not None:
            try:
                stop_browser_open(process)
            except (OSError, subprocess.TimeoutExpired) as error:
                response["completion"] = "unknown"
                response["message"] = f"{response['message']}; launcher cleanup failed: {error}"
            if process.stderr is not None:
                process.stderr.close()
        for signum, handler in previous_handlers.items():
            signal.signal(signum, handler)
    return response


def handle_message(message):
    info = message.get("info", "")
    if info == "openInBrowser":
        return open_in_browser(message)
    param = message.get("param", {})
    handlers = {
        "switchTab": switch_tab,
        "listDownloads": list_downloads,
        "openHistory": select_option,
        "changeToPage": select_option,
    }
    handler = handlers.get(info)
    if handler is None:
        return {"result": "", "info": info, "error": f"unknown command: {info}"}
    try:
        return {"result": handler(param), "info": info}
    except Exception as error:
        return {"result": "", "info": info, "error": str(error)}


def main():
    while True:
        try:
            message = read_message()
            if message is None:
                return
            send_message(handle_message(message))
            if message.get("info") == "openInBrowser":
                return
        except (EOFError, ValueError, json.JSONDecodeError) as error:
            print(error, file=sys.stderr)
            return


if __name__ == "__main__":
    main()
