#!/usr/bin/env python3
"""Private, inherited socket transport for the official browser engine."""
import collections
import hashlib
import json
import logging
import os
import pathlib
import queue
import re
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


PLAYWRIGHT_VERSION = "1.65.1"
PLAYWRIGHT_INTEGRITY = (
    "sha512-hqPpz7blSMPxWVdoeE1yiBSzXsKiVOd2k+kjCvmUywD2GPO2Dd9HfcWnq9NN0GHbNZDFgZ9JfBFr+X5/BLo0qw==")
NODE_PATH = "/usr/local/bin:/usr/bin:/bin"
NODE_CHECK_TIMEOUT = 3.0
NODE_CHECK_LOCK = threading.Lock()
NODE_CHECKED = set()


def require(condition, message):
    if not condition:
        raise ValueError(message)


def trusted_path(path):
    require(path.is_absolute() and ".." not in path.parts, "trusted path must be absolute")
    for directory in reversed(path.parents):
        info = directory.lstat()
        require(stat.S_ISDIR(info.st_mode) and info.st_uid in {0, os.getuid()} and
                not info.st_mode & 0o022, "unsafe trusted path parent")
    info = path.lstat()
    require(not path.is_symlink() and info.st_uid in {0, os.getuid()} and
            not info.st_mode & 0o022, "unsafe trusted path owner or permissions")
    return info


def engine_environment():
    return {key: value for key, value in os.environ.items()
            if key not in {"NODE_OPTIONS", "NODE_PATH"} and
            not key.lower().startswith("npm_config_")}


def node_version(node, environment):
    with subprocess.Popen([str(node), "--version"], env=environment,
            stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.STDOUT) as process:
        output = bytearray()
        deadline = time.monotonic() + NODE_CHECK_TIMEOUT
        try:
            while True:
                remaining = deadline - time.monotonic()
                if remaining <= 0 or not select.select([process.stdout], [], [], remaining)[0]:
                    raise ValueError("Node version check timed out")
                chunk = os.read(process.stdout.fileno(), 4096 - len(output))
                if not chunk:
                    break
                output.extend(chunk)
                require(len(output) < 4096, "Node version check output exceeds limit")
            process.wait(timeout=max(0.001, deadline - time.monotonic()))
            return process.returncode, bytes(output)
        except BaseException:
            process.kill()
            process.wait(timeout=SHUTDOWN_TIMEOUT)
            raise


def trusted_node(environment):
    candidate = os.environ.get("ABUSH_BROWSER_ENGINE_NODE")
    if candidate is None:
        candidate = shutil.which("node", path=NODE_PATH)
    require(bool(candidate), "Node >=20 is required; trusted Node executable not found")
    node = pathlib.Path(candidate)
    require(node.is_absolute(), "ABUSH_BROWSER_ENGINE_NODE must be absolute")
    try:
        # A local executable alias may resolve only within its trusted directory.
        if node.is_symlink():
            trusted_path(node.parent)
            require(node.lstat().st_uid in {0, os.getuid()} and
                    node.resolve(strict=True).parent == node.parent.resolve(),
                    "unsafe Node symlink escape")
            node = node.resolve(strict=True)
        info = trusted_path(node)
        require(stat.S_ISREG(info.st_mode) and os.access(node, os.X_OK),
                "Node executable must be a trusted executable file")
        identity = (str(node), info.st_dev, info.st_ino, info.st_size, info.st_mtime_ns,
                    info.st_ctime_ns)
        with NODE_CHECK_LOCK:
            if identity not in NODE_CHECKED:
                try:
                    returncode, output = node_version(node, environment)
                except (OSError, subprocess.TimeoutExpired) as error:
                    raise ValueError("Node version check failed or timed out") from error
                match = re.fullmatch(rb"v(\d+)\.\d+\.\d+\s*", output)
                require(returncode == 0 and match and int(match[1]) >= 20,
                        "Node >=20 is required; Node version check failed")
                NODE_CHECKED.add(identity)
        return str(node)
    except OSError as error:
        raise ValueError("trusted Node executable is missing or invalid") from error


def trusted_engine_root():
    explicit = os.environ.get("ABUSH_BROWSER_ENGINE_ROOT")
    data = pathlib.Path(os.environ.get("XDG_DATA_HOME", pathlib.Path.home() / ".local/share"))
    root = pathlib.Path(explicit) if explicit is not None else data / "abush/browser-engine"
    try:
        if explicit is None:
            trusted_path(root.parent)
            info = root.lstat()
            require(stat.S_ISLNK(info.st_mode) and info.st_uid == os.getuid(),
                    "official browser engine requires a managed installation symlink")
            target = pathlib.Path(os.readlink(root))
            require(not target.is_absolute() and len(target.parts) == 2 and
                    target.parts[0] == "browser-engine-versions" and
                    target.parts[1] not in {".", ".."}, "unsafe managed engine symlink target")
            root = root.parent / target
        require(stat.S_ISDIR(trusted_path(root).st_mode), "engine root must be a directory")
        assets = {}
        files = {}
        def scan_error(error):
            raise error
        for directory, directories, names in os.walk(root, followlinks=False, onerror=scan_error):
            for name in directories + names:
                path = pathlib.Path(directory) / name
                relative = path.relative_to(root).as_posix()
                info = path.lstat()
                require(info.st_uid in {0, os.getuid()}, "untrusted engine entry owner")
                if stat.S_ISLNK(info.st_mode):
                    require(path.resolve(strict=True).is_relative_to(root) and
                            path.resolve().is_file(), "engine dependency symlink escape")
                    files[relative] = {"symlink": os.readlink(path)}
                else:
                    require(not info.st_mode & 0o022, "unsafe writable engine entry")
                    if stat.S_ISREG(info.st_mode):
                        require(explicit is not None or not info.st_mode & 0o222,
                                "managed engine files must be read-only")
                        digest = hashlib.sha256(path.read_bytes()).hexdigest()
                        if relative != "abush-engine-manifest.json":
                            files[relative] = {"sha256": digest}
                        if path.parent == root and (name.endswith(".cjs") or
                                name in {"package.json", "package-lock.json"}):
                            assets[name] = digest
                    else:
                        require(stat.S_ISDIR(info.st_mode), "unsupported engine entry")
        require({"main.cjs", "engine.cjs", "protocol.cjs", "package.json",
                 "package-lock.json"}.issubset(assets), "incomplete official browser engine")
        package = json.loads((root / "package.json").read_bytes())
        lock = json.loads((root / "package-lock.json").read_bytes())
        require(package.get("name") == "abush-browser-engine" and
                package.get("dependencies") == {"playwright-core": PLAYWRIGHT_VERSION} and
                package.get("engines") == {"node": ">=20"}, "engine package dependency mismatch")
        packages = lock.get("packages", {})
        require(lock.get("lockfileVersion") == 3 and
                set(packages) == {"", "node_modules/playwright-core"}, "engine lock mismatch")
        require(packages[""].get("name") == package["name"] and
                packages[""].get("version") == package.get("version") and
                packages[""].get("dependencies") == package["dependencies"],
                "engine package and lock disagree")
        dependency = packages["node_modules/playwright-core"]
        require(dependency.get("version") == PLAYWRIGHT_VERSION and
                dependency.get("integrity") == PLAYWRIGHT_INTEGRITY and
                dependency.get("resolved") ==
                f"https://registry.npmjs.org/playwright-core/-/playwright-core-{PLAYWRIGHT_VERSION}.tgz",
                "engine requires official pinned playwright-core 1.65.1")
        require(json.loads((root / "node_modules/playwright-core/package.json").read_bytes())
                .get("version") == PLAYWRIGHT_VERSION, "installed playwright-core version mismatch")
        if explicit is None:
            manifest = json.loads((root / "abush-engine-manifest.json").read_bytes())
            fingerprint = hashlib.sha256(json.dumps(assets, sort_keys=True,
                separators=(",", ":")).encode()).hexdigest()
            require(manifest.get("schema_version") == 1 and
                    manifest.get("playwright_core") == PLAYWRIGHT_VERSION and
                    manifest.get("assets") == assets and manifest.get("files") == files and
                    manifest.get("fingerprint") == fingerprint and
                    manifest.get("engine_version") == package.get("version") and
                    root.name == f"{package.get('version')}-{fingerprint}",
                    "managed engine manifest mismatch")
        return root
    except (OSError, KeyError, TypeError, AttributeError, json.JSONDecodeError) as error:
        raise ValueError("official browser engine root is missing or invalid") from error


class BrowserEngine:
    def __init__(self, on_cdp, on_close=None, timeout=35, process_factory=subprocess.Popen):
        root = trusted_engine_root()
        environment = engine_environment()
        node = trusted_node(environment)
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
            self.process = process_factory([node, str(root / "main.cjs"), "--fd",
                str(child.fileno()), "--artifacts-dir", str(self.artifacts)],
                pass_fds=(child.fileno(),), stdin=subprocess.DEVNULL,
                stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, env=environment)
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
