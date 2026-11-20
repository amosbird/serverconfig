#!/usr/bin/env python3
"""Private, inherited socket transport for the official browser engine."""
import collections
import json
import logging
import os
import pathlib
import queue
import select
import shutil
import socket
import stat
import struct
import subprocess
import threading
import time
import uuid

MAX_MESSAGE_SIZE = 1024 * 1024
FRAME_TIMEOUT = 2.0
SHUTDOWN_TIMEOUT = 2.0


class BoundedQueue:
    """Count and byte admission, with separately reserved control capacity."""
    def __init__(self, maxsize=64, max_bytes=4 * MAX_MESSAGE_SIZE,
                 reserve_count=8, reserve_bytes=MAX_MESSAGE_SIZE, on_full=None):
        self.maxsize = maxsize
        self.max_bytes = max_bytes
        self.reserve_count = reserve_count
        self.reserve_bytes = reserve_bytes
        self.on_full = on_full
        self.condition = threading.Condition()
        self.items = collections.deque()
        self.bytes = 0
        self.normal_count = 0
        self.normal_bytes = 0
        self.closed = False

    def put(self, item, block=True, timeout=None, control=False):
        message = item[0] if isinstance(item, tuple) else item
        if isinstance(message, dict):
            operation = message.get("operation", "")
            name = message.get("event", "")
            control = control or "reply_to" in message or message.get("type") == "response" or \
                "cancel" in operation or "cancel" in name or "completion" in operation or \
                "completion" in name
        size = len(message) if isinstance(message, bytes) else \
            len(json.dumps(message, separators=(",", ":"), allow_nan=False).encode())
        if message is None:
            size = 0
            control = True
        # Admission never blocks a producer holding an authority lock.
        with self.condition:
            full = self.closed or size > MAX_MESSAGE_SIZE + 4 or \
                len(self.items) >= self.maxsize + self.reserve_count or \
                self.bytes + size > self.max_bytes + self.reserve_bytes or \
                (not control and (self.normal_count >= self.maxsize or
                                 self.normal_bytes + size > self.max_bytes))
            if not full:
                self.items.append((item, size, control))
                self.bytes += size
                if not control:
                    self.normal_count += 1
                    self.normal_bytes += size
                self.condition.notify()
                return
        if self.on_full:
            self.on_full(OSError("transport admission exhausted or closed"))
        raise queue.Full("transport admission exhausted or closed")

    def put_nowait(self, item, **kwargs):
        self.put(item, block=False, **kwargs)

    def get(self, block=True, timeout=None):
        deadline = None if timeout is None else time.monotonic() + timeout
        with self.condition:
            while not self.items:
                if self.closed:
                    return None
                remaining = None if deadline is None else deadline - time.monotonic()
                if not block or (remaining is not None and remaining <= 0):
                    raise queue.Empty
                self.condition.wait(remaining)
            item, size, control = self.items.popleft()
            self.bytes -= size
            if not control:
                self.normal_count -= 1
                self.normal_bytes -= size
            self.condition.notify_all()
            return item

    def get_nowait(self):
        return self.get(block=False)

    def qsize(self):
        with self.condition:
            return len(self.items)

    def empty(self):
        return self.qsize() == 0

    def close(self):
        with self.condition:
            self.closed = True
            self.items.clear()
            self.bytes = self.normal_bytes = self.normal_count = 0
            self.condition.notify_all()


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
        self.failure = None
        self.close_done = threading.Event()
        self.revoked_leases = set()
        self.stderr_head = bytearray()
        self.stderr_tail = bytearray()
        self.stderr_lock = threading.Lock()
        self.artifacts = artifacts_directory()
        self.outbound = BoundedQueue()
        self.socket, child = socket.socketpair(socket.AF_UNIX, socket.SOCK_STREAM)
        self.socket.settimeout(2)
        try:
            self.process = process_factory(["node", str(root / "main.cjs"), "--fd",
                str(child.fileno()), "--artifacts-dir", str(self.artifacts)],
                pass_fds=(child.fileno(),), stdin=subprocess.DEVNULL,
                stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        except BaseException:
            self.socket.close()
            shutil.rmtree(self.artifacts)
            raise
        finally:
            child.close()
        self.stderr_reader = threading.Thread(target=self._drain_stderr, daemon=True)
        self.stderr_reader.start()
        self.writer = threading.Thread(target=self._write, daemon=True)
        self.reader = threading.Thread(target=self._read, daemon=True)
        self.writer.start()
        self.reader.start()

    def send(self, message):
        data = json.dumps(message, separators=(",", ":"), allow_nan=False).encode()
        if len(data) > MAX_MESSAGE_SIZE:
            raise ValueError("engine frame exceeds 1 MiB")
        with self.lock:
            if self.closed:
                raise OSError(self.failure or "engine is closed")
        try:
            self.outbound.put_nowait(struct.pack(">I", len(data)) + data,
                                      control=message.get("type") == "close")
        except queue.Full as error:
            self.close(error="engine transport backpressure")
            raise OSError("engine transport backpressure") from error

    def request(self, kind, lease_id, **fields):
        request_id = uuid.uuid4().hex
        waiter = {"done": threading.Event(), "reply": None, "lease_id": lease_id}
        with self.lock:
            closed = self.closed
        if closed:
            self.close_done.wait(SHUTDOWN_TIMEOUT)
            raise OSError(self.failure or "engine is closed")
        with self.lock:
            if self.closed:
                raise OSError(self.failure or "engine is closed")
            if lease_id in self.revoked_leases:
                raise OSError("engine lease revoked")
            if len(self.pending) >= 32:
                raise OSError("engine request admission exhausted")
            self.pending[request_id] = waiter
        try:
            self.send({"type": kind, "request_id": request_id, "lease_id": lease_id, **fields})
            if not waiter["done"].wait(self.timeout):
                self.close_lease(lease_id)
                raise TimeoutError("engine reply timed out; lease revoked")
            if waiter["reply"] is None:
                raise OSError(self.failure or "engine lease closed")
            return waiter["reply"]
        finally:
            with self.lock:
                self.pending.pop(request_id, None)

    def close_lease(self, lease_id):
        with self.lock:
            self.revoked_leases.add(lease_id)
            if len(self.revoked_leases) > 4096:
                raise OSError("engine lease history exhausted")
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
        except OSError as error:
            self.close(error=f"engine socket write failed: {error}")

    def _exact(self, size, deadline=None):
        data = bytearray()
        while len(data) < size:
            remaining = None if deadline is None else deadline - time.monotonic()
            if remaining is not None and remaining <= 0:
                raise TimeoutError("engine frame assembly deadline exceeded")
            readable, _, _ = select.select([self.socket], [], [], remaining)
            if not readable:
                raise TimeoutError("engine frame assembly deadline exceeded")
            part = self.socket.recv(size - len(data))
            if not part:
                raise EOFError("engine child closed transport")
            data.extend(part)
        return bytes(data)

    def _read(self):
        try:
            while True:
                first = self._exact(1)
                deadline = time.monotonic() + FRAME_TIMEOUT
                size = struct.unpack(">I", first + self._exact(3, deadline))[0]
                if not 0 < size <= MAX_MESSAGE_SIZE:
                    raise ValueError("invalid engine frame")
                message = json.loads(self._exact(size, deadline).decode("utf-8"),
                                     parse_constant=lambda value: (_ for _ in ()).throw(
                                         ValueError("non-finite engine JSON")))
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
                    fields = {"type", "lease_id", "request_id", "ok"}
                    fields |= {"result"} if message.get("ok") else {"error"}
                    if set(message) != fields or not message.get("request_id") or \
                            (message["ok"] and message["result"] is not None and
                             not valid_result(message["result"])) or \
                            (not message["ok"] and (not isinstance(message["error"], str) or
                                                   not message["error"])):
                        raise ValueError("invalid engine reply output")
                    with self.lock:
                        waiter = self.pending.get(message.get("request_id"))
                        if waiter and waiter["lease_id"] == message.get("lease_id"):
                            if message["ok"] and not valid_result(message["result"]):
                                raise ValueError("invalid engine result")
                            waiter["reply"] = message
                            waiter["done"].set()
                else:
                    raise ValueError("invalid engine message")
        except (OSError, EOFError, ValueError, TypeError, KeyError, queue.Full) as error:
            self.close(error=str(error))

    def _drain_stderr(self):
        try:
            while True:
                chunk = self.process.stderr.read1(4096)
                if not chunk:
                    return
                with self.stderr_lock:
                    self.stderr_head.extend(chunk[:max(0, 4096 - len(self.stderr_head))])
                    self.stderr_tail.extend(chunk)
                    del self.stderr_tail[:-4096]
        except (OSError, ValueError) as error:
            logging.info("engine stderr drain failed: %s", error)

    def diagnostics(self):
        with self.stderr_lock:
            raw = bytes(self.stderr_head + self.stderr_tail)
        text = raw.decode("utf-8", errors="replace")
        text = "".join(c if c.isprintable() or c == "\n" else "?" for c in text)
        return text.encode("utf-8")[:8192].decode("utf-8", errors="ignore")

    def close(self, deadline=None, error=None):
        deadline = deadline or time.monotonic() + SHUTDOWN_TIMEOUT
        with self.lock:
            already_closed = self.closed
            if not already_closed:
                self.closed = True
                self.failure = error
                waiters = list(self.pending.values())
        if already_closed:
            if threading.current_thread() not in (self.reader, self.writer, self.stderr_reader):
                self.close_done.wait(max(0, deadline - time.monotonic()))
            return
        callback_error = None
        try:
            self.on_close()
        except Exception as failure:
            callback_error = failure
            logging.error("engine close callback failed: %s", failure)
        try:
            self.socket.shutdown(socket.SHUT_RDWR)
        except OSError:
            pass
        self.socket.close()
        self.outbound.close()
        # Only the exact inherited child is owned; no process discovery or scavenging.
        if self.process.poll() is None:
            self.process.terminate()
            try:
                self.process.wait(timeout=max(0, (deadline - time.monotonic()) / 2))
            except subprocess.TimeoutExpired:
                self.process.kill()
                try:
                    self.process.wait(timeout=max(0, deadline - time.monotonic()))
                except subprocess.TimeoutExpired:
                    self.failure = "owned engine child did not exit before shutdown deadline"
                    logging.error(self.failure)
        for thread in (self.stderr_reader, self.reader, self.writer):
            if thread is not threading.current_thread():
                thread.join(timeout=max(0, deadline - time.monotonic()))
        detail = self.diagnostics()
        if error and detail:
            self.failure = f"{self.failure or error}: {detail}"
        for waiter in waiters:
            waiter["done"].set()
        if self.process.poll() is not None and not self.stderr_reader.is_alive():
            self.process.stderr.close()
            if not self.artifacts.is_symlink() and shutil.rmtree.avoids_symlink_attacks:
                try:
                    shutil.rmtree(self.artifacts)
                except OSError as cleanup_error:
                    logging.error("engine artifacts cleanup failed: %s", cleanup_error)
                    self.failure = f"{self.failure or 'engine closed'}: {cleanup_error}"
        self.close_done.set()
        if callback_error:
            raise callback_error
