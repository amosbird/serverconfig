#!/usr/bin/env python3
"""Private, inherited socket transport for the official browser engine."""
import json
import os
import pathlib
import queue
import socket
import stat
import struct
import subprocess
import threading
import uuid

MAX_MESSAGE_SIZE = 1024 * 1024


def valid_action(value):
    if not isinstance(value, dict):
        return False
    kind = value.get("type")
    fields = {
        "snapshot": (), "text": (), "click": ("snapshot_id", "ref"),
        "fill": ("snapshot_id", "ref", "value"),
        "press": ("snapshot_id", "ref", "key"),
        "select": ("snapshot_id", "ref", "values"),
        "check": ("snapshot_id", "ref", "checked"),
        "hover": ("snapshot_id", "ref"), "scroll": ("x", "y"),
        "evaluate": ("expression",), "wait": ("text", "timeout_ms"),
        "network_list": (), "network_body": ("request_id",), "console": (),
        "screenshot": ("full_page",),
    }
    if not isinstance(kind, str) or kind not in fields or set(value) != {"type", *fields[kind]}:
        return False
    for key in fields[kind]:
        item = value[key]
        if key in {"checked", "full_page"}:
            if not isinstance(item, bool):
                return False
        elif key in {"x", "y", "timeout_ms"}:
            if type(item) is not int or abs(item) > 2**53 - 1:
                return False
            if key == "timeout_ms" and not 1 <= item <= 30000:
                return False
        elif key == "values":
            if not isinstance(item, list) or not all(isinstance(v, str) for v in item):
                return False
        elif not isinstance(item, str) or (key != "value" and not item):
            return False
    return True


def valid_result(value):
    return isinstance(value, dict) and set(value) == {"text", "data", "images"} and \
        isinstance(value["text"], str) and isinstance(value["images"], list) and all(
            isinstance(image, dict) and set(image) == {"mime_type", "data_base64"} and
            image["mime_type"] in {"image/png", "image/jpeg"} and
            isinstance(image["data_base64"], str) for image in value["images"])


def artifacts_directory():
    cache = pathlib.Path(os.environ.get("XDG_CACHE_HOME", pathlib.Path.home() / ".cache"))
    if not cache.is_absolute():
        raise ValueError("XDG cache path must be absolute")
    root = cache / "abush" / "browser-engine"
    for directory in (cache, cache / "abush", root):
        directory.mkdir(mode=0o700, parents=True, exist_ok=True)
        info = directory.lstat()
        if directory.is_symlink() or not stat.S_ISDIR(info.st_mode) or \
                info.st_uid != os.getuid() or info.st_mode & 0o022:
            raise ValueError("unsafe engine artifacts directory")
    os.chmod(root, 0o700)
    directory = root / uuid.uuid4().hex
    directory.mkdir(mode=0o700)
    return directory


class BrowserEngine:
    def __init__(self, on_cdp, on_close=None, timeout=35, process_factory=subprocess.Popen):
        data = pathlib.Path(os.environ.get("XDG_DATA_HOME", pathlib.Path.home() / ".local/share"))
        root = pathlib.Path(os.environ.get("ABUSH_BROWSER_ENGINE_ROOT", data / "abush/browser-engine"))
        if not root.is_absolute() or not (root / "main.cjs").is_file():
            raise ValueError("official browser engine is not installed")
        self.on_cdp = on_cdp
        self.on_close = on_close or (lambda: None)
        self.timeout = timeout
        self.lock = threading.Lock()
        self.pending = {}
        self.closed = False
        self.socket, child = socket.socketpair(socket.AF_UNIX, socket.SOCK_STREAM)
        self.socket.settimeout(2)
        try:
            self.process = process_factory(["node", str(root / "main.cjs"), "--fd",
                str(child.fileno()), "--artifacts-dir", str(artifacts_directory())],
                pass_fds=(child.fileno(),), stdin=subprocess.DEVNULL,
                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        except BaseException:
            self.socket.close()
            raise
        finally:
            child.close()
        self.outbound = queue.Queue(maxsize=256)
        self.writer = threading.Thread(target=self._write, daemon=True)
        self.reader = threading.Thread(target=self._read, daemon=True)
        self.writer.start()
        self.reader.start()

    def send(self, message):
        data = json.dumps(message, separators=(",", ":")).encode()
        if len(data) > MAX_MESSAGE_SIZE:
            raise ValueError("engine frame exceeds 1 MiB")
        with self.lock:
            if self.closed:
                raise OSError("engine is closed")
        try:
            self.outbound.put_nowait(struct.pack(">I", len(data)) + data)
        except queue.Full as error:
            self.close()
            raise OSError("engine transport backpressure") from error

    def request(self, kind, lease_id, **fields):
        request_id = uuid.uuid4().hex
        waiter = {"done": threading.Event(), "reply": None, "lease_id": lease_id}
        with self.lock:
            if self.closed:
                raise OSError("engine is closed")
            self.pending[request_id] = waiter
        try:
            self.send({"type": kind, "request_id": request_id, "lease_id": lease_id, **fields})
            if not waiter["done"].wait(self.timeout):
                raise TimeoutError("engine reply timed out")
            if waiter["reply"] is None:
                raise OSError("engine lease closed")
            return waiter["reply"]
        finally:
            with self.lock:
                self.pending.pop(request_id, None)

    def close_lease(self, lease_id):
        with self.lock:
            waiters = [v for v in self.pending.values() if v["lease_id"] == lease_id]
        for waiter in waiters:
            waiter["done"].set()
        self.send({"type": "close", "request_id": uuid.uuid4().hex, "lease_id": lease_id})

    def _write(self):
        try:
            while True:
                data = self.outbound.get()
                if data is None:
                    return
                self.socket.sendall(data)
        except OSError:
            self.close()

    def _exact(self, size):
        data = bytearray()
        while len(data) < size:
            try:
                part = self.socket.recv(size - len(data))
            except socket.timeout:
                with self.lock:
                    if self.closed:
                        raise EOFError()
                continue
            if not part:
                raise EOFError()
            data.extend(part)
        return bytes(data)

    def _read(self):
        try:
            while True:
                size = struct.unpack(">I", self._exact(4))[0]
                if not 0 < size <= MAX_MESSAGE_SIZE:
                    raise ValueError("invalid engine frame")
                message = json.loads(self._exact(size))
                if not isinstance(message, dict) or not isinstance(message.get("lease_id"), str):
                    raise ValueError("invalid engine message")
                if message.get("type") == "cdp":
                    if set(message) != {"type", "lease_id", "message"} or \
                            not isinstance(message["message"], dict):
                        raise ValueError("invalid engine CDP message")
                    self.on_cdp(message)
                elif message.get("type") == "reply":
                    if not isinstance(message.get("request_id"), str) or \
                            type(message.get("ok")) is not bool:
                        raise ValueError("invalid engine reply")
                    with self.lock:
                        waiter = self.pending.get(message.get("request_id"))
                        if waiter and waiter["lease_id"] == message.get("lease_id"):
                            waiter["reply"] = message
                            waiter["done"].set()
                else:
                    raise ValueError("invalid engine message")
        except (OSError, EOFError, ValueError, TypeError, KeyError):
            self.close()

    def close(self):
        with self.lock:
            if self.closed:
                return
            self.closed = True
            waiters = list(self.pending.values())
        for waiter in waiters:
            waiter["done"].set()
        try:
            self.socket.shutdown(socket.SHUT_RDWR)
        except OSError:
            pass
        self.socket.close()
        try:
            self.outbound.put_nowait(None)
        except queue.Full:
            pass
        # Popen owns this exact child, never a process discovered by PID search.
        if self.process.poll() is None:
            self.process.terminate()
            try:
                self.process.wait(timeout=1)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait(timeout=1)
        for thread in (self.reader, self.writer):
            if thread is not threading.current_thread():
                thread.join(timeout=1)
        self.on_close()
