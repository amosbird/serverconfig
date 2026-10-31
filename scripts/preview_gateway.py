#!/usr/bin/env python3
"""Remote preview gateway using Unix sockets only."""

import argparse
import asyncio
import contextlib
import fcntl
import json
import os
import re
import secrets
import select
import signal
import socket
import stat
import struct
import sys
import time
from dataclasses import dataclass, field
from pathlib import Path

VERSION = 2
ROUTE_TTL = 24 * 60 * 60
GC_INTERVAL = 60 * 60
MAX_CONTROL = 8192
MAX_LINE = 8192
MAX_HEADERS = 65536
MAX_TRAILERS = 16384
MAX_NAMES = 32
IDLE_TIMEOUT = 30
CLOSE_TIMEOUT = 2
NAME_RE = re.compile(r"^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?$")
TOKEN_RE = re.compile(rb"^[!#$%&'*+\-.^_`|~0-9A-Za-z]+$")
CHUNK_RE = re.compile(
    rb"^(?P<size>[0-9A-Fa-f]+)"
    rb"(?:;[!#$%&'*+\-.^_`|~0-9A-Za-z]+"
    rb"(?:=(?:[!#$%&'*+\-.^_`|~0-9A-Za-z]+|\"(?:[\x20-\x21\x23-\x5b\x5d-\x7e]|\\[\x20-\x7e])*\"))?)*\r\n$")
HOP_HEADERS = {
    b"connection", b"keep-alive", b"proxy-authenticate", b"proxy-authorization",
    b"proxy-connection", b"te", b"trailer", b"transfer-encoding", b"upgrade",
}
FRAMING_HEADERS = {b"content-length", b"transfer-encoding"}
CRITICAL_CONNECTION_HEADERS = {
    b"host", b"content-length", b"transfer-encoding", b"trailer", b"expect",
}
FORBIDDEN_TRAILERS = HOP_HEADERS | FRAMING_HEADERS | {b"host"}


class ProtocolError(Exception):
    def __init__(self, status=400, message="Bad Request"):
        super().__init__(message)
        self.status = status
        self.message = message


@dataclass(frozen=True)
class Route:
    name: str
    port: int
    generation: str
    created_at: float
    updated_at: float
    expires_at: float

    def as_dict(self):
        return {
            "name": self.name,
            "port": self.port,
            "generation": self.generation,
            "created_at": self.created_at,
            "updated_at": self.updated_at,
            "expires_at": self.expires_at,
        }


@dataclass
class ConnectionGroup:
    tasks: set = field(default_factory=set)
    writers: set = field(default_factory=set)


@dataclass
class Message:
    start: bytes
    headers: list
    declared_length: int | None
    body_length: int | None
    chunked: bool
    close: bool
    upgrade_protocol: bytes | None
    trailer_names: tuple
    status: int | None = None


def _base_runtime_dir():
    value = os.environ.get("XDG_RUNTIME_DIR")
    return Path(value) if value else Path("/run/user") / str(os.getuid())


def _secure_existing_dir(path):
    try:
        info = path.lstat()
    except OSError as error:
        raise RuntimeError(f"runtime directory unavailable: {path}: {error}") from error
    if not stat.S_ISDIR(info.st_mode) or stat.S_ISLNK(info.st_mode):
        raise RuntimeError(f"runtime path is not a real directory: {path}")
    if info.st_uid != os.getuid():
        raise RuntimeError(f"runtime directory is not owned by uid {os.getuid()}: {path}")
    if info.st_mode & 0o022:
        raise RuntimeError(f"runtime directory is group/other writable: {path}")


def runtime_dir(create=True):
    base = _base_runtime_dir()
    _secure_existing_dir(base)
    path = base / "preview-gateway"
    if create:
        try:
            path.mkdir(mode=0o700)
        except FileExistsError:
            pass
    _secure_existing_dir(path)
    if stat.S_IMODE(path.stat().st_mode) != 0o700:
        raise RuntimeError(f"preview runtime directory must have mode 0700: {path}")
    return path


def state_dir():
    root = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local/state"))
    path = root / "preview-gateway"
    try:
        path.mkdir(mode=0o700, parents=True)
    except FileExistsError:
        pass
    info = path.lstat()
    if (not stat.S_ISDIR(info.st_mode) or stat.S_ISLNK(info.st_mode)
            or info.st_uid != os.getuid() or info.st_mode & 0o077):
        raise RuntimeError(f"unsafe state directory: {path}")
    return path


def paths():
    root = runtime_dir()
    return {
        "root": root,
        "control": root / "control.sock",
        "http": root / "http.sock",
        "lock": root / "gateway.lock",
        "pid": root / "gateway.pid",
        "state": state_dir() / "routes.json",
    }


def peer_uid(writer):
    sock = writer.get_extra_info("socket")
    if not sock or not hasattr(socket, "SO_PEERCRED"):
        raise PermissionError("SO_PEERCRED is unavailable")
    _pid, uid, _gid = struct.unpack("3i", sock.getsockopt(socket.SOL_SOCKET, socket.SO_PEERCRED, 12))
    return uid


async def idle_readline(reader):
    try:
        return await asyncio.wait_for(reader.readline(), IDLE_TIMEOUT)
    except asyncio.TimeoutError as error:
        raise ProtocolError(408, "Request Timeout") from error


async def read_header_block(reader, backend=False):
    lines = []
    total = 0
    while True:
        line = await idle_readline(reader)
        if not line:
            if not lines:
                return None
            raise ProtocolError(502, "Bad Gateway") if backend else ProtocolError()
        total += len(line)
        if len(line) > MAX_LINE or total > MAX_HEADERS:
            if backend:
                raise ProtocolError(502, "Bad Gateway")
            raise ProtocolError(431, "Request Header Fields Too Large")
        if not line.endswith(b"\r\n") or b"\x00" in line:
            raise ProtocolError(502, "Bad Gateway") if backend else ProtocolError()
        if line == b"\r\n":
            return lines
        if line[:1] in (b" ", b"\t"):
            raise ProtocolError(502, "Bad Gateway") if backend else ProtocolError()
        lines.append(line[:-2])


def _parse_header_lines(lines, error_status):
    headers = []
    values = {}
    for line in lines:
        if b":" not in line:
            raise ProtocolError(error_status, "Bad Gateway" if error_status == 502 else "Bad Request")
        name, value = line.split(b":", 1)
        if not TOKEN_RE.fullmatch(name):
            raise ProtocolError(error_status, "Bad Gateway" if error_status == 502 else "Bad Request")
        value = value.strip(b" \t")
        if any(byte < 32 and byte != 9 or byte == 127 for byte in value):
            raise ProtocolError(error_status, "Bad Gateway" if error_status == 502 else "Bad Request")
        lower = name.lower()
        headers.append((name, value))
        values.setdefault(lower, []).append(value)
    return headers, values


def _comma_tokens(values, error_status):
    result = []
    for value in values:
        for part in value.split(b","):
            token = part.strip()
            if not token or not TOKEN_RE.fullmatch(token):
                raise ProtocolError(error_status, "Bad Gateway" if error_status == 502 else "Bad Request")
            result.append(token.lower())
    return result


def parse_message(lines, request=True, request_method=None):
    if not lines:
        raise ProtocolError()
    start = lines[0]
    error_status = 400 if request else 502
    if any(byte < 32 or byte == 127 for byte in start):
        raise ProtocolError(error_status, "Bad Request" if request else "Bad Gateway")
    headers, values = _parse_header_lines(lines[1:], error_status)

    lengths = values.get(b"content-length", [])
    declared_length = None
    if lengths:
        normalized = [part.strip() for value in lengths for part in value.split(b",")]
        if not normalized or any(not item or not item.isdigit() for item in normalized):
            raise ProtocolError(error_status, "Bad Request" if request else "Bad Gateway")
        numeric = [int(item) for item in normalized]
        if len(set(numeric)) != 1:
            raise ProtocolError(error_status, "Bad Request" if request else "Bad Gateway")
        declared_length = numeric[0]

    transfers = values.get(b"transfer-encoding", [])
    chunked = False
    if transfers:
        if declared_length is not None or len(transfers) != 1 or transfers[0].strip().lower() != b"chunked":
            raise ProtocolError(error_status, "Bad Request" if request else "Bad Gateway")
        chunked = True

    connection_tokens = _comma_tokens(values.get(b"connection", []), error_status)
    if set(connection_tokens) & CRITICAL_CONNECTION_HEADERS:
        raise ProtocolError(error_status, "Bad Request" if request else "Bad Gateway")
    upgrade_values = values.get(b"upgrade", [])
    upgrade_protocol = None
    if upgrade_values:
        protocols = _comma_tokens(upgrade_values, error_status)
        if len(protocols) != 1 or b"upgrade" not in connection_tokens:
            raise ProtocolError(error_status, "Bad Request" if request else "Bad Gateway")
        upgrade_protocol = protocols[0]
    elif b"upgrade" in connection_tokens:
        raise ProtocolError(error_status, "Bad Request" if request else "Bad Gateway")

    trailer_names = tuple(_comma_tokens(values.get(b"trailer", []), error_status))
    if trailer_names and (not chunked or set(trailer_names) & FORBIDDEN_TRAILERS):
        raise ProtocolError(error_status, "Bad Request" if request else "Bad Gateway")

    close = b"close" in connection_tokens
    status = None
    if request:
        parts = start.split(b" ")
        if len(parts) != 3 or not TOKEN_RE.fullmatch(parts[0]) or parts[2] not in (
                b"HTTP/1.0", b"HTTP/1.1"):
            raise ProtocolError()
        method, target, version = parts
        if (not target.startswith(b"/") or target.startswith(b"//") or method.upper() == b"CONNECT"
                or b"://" in target):
            raise ProtocolError()
        hosts = values.get(b"host", [])
        if (version == b"HTTP/1.1" and len(hosts) != 1) or len(hosts) > 1:
            raise ProtocolError()
        if version == b"HTTP/1.0" and b"keep-alive" not in connection_tokens:
            close = True
        body_length = None if chunked else declared_length or 0
    else:
        parts = start.split(b" ", 2)
        if (len(parts) < 2 or parts[0] not in (b"HTTP/1.0", b"HTTP/1.1")
                or len(parts[1]) != 3 or not parts[1].isdigit()):
            raise ProtocolError(502, "Bad Gateway")
        status = int(parts[1])
        if not 100 <= status <= 999:
            raise ProtocolError(502, "Bad Gateway")
        no_body = request_method == b"HEAD" or 100 <= status < 200 or status in (204, 304)
        if (100 <= status < 200 or status == 204) and (
                chunked or declared_length is not None):
            raise ProtocolError(502, "Bad Gateway")
        body_length = 0 if no_body else (None if chunked else declared_length)
        if parts[0] == b"HTTP/1.0" and b"keep-alive" not in connection_tokens:
            close = True
    return Message(start, headers, declared_length, body_length, chunked, close,
                   upgrade_protocol, trailer_names, status)


def header_values(message, name):
    lower = name.lower()
    return [value for key, value in message.headers if key.lower() == lower]


def connection_tokens(message):
    return set(_comma_tokens(header_values(message, b"connection"), 502 if message.status else 400))


def _forwarded_headers(message, response=False, upgrade=False):
    tokens = connection_tokens(message)
    out = [message.start]
    for name, value in message.headers:
        lower = name.lower()
        if (lower in HOP_HEADERS or lower in FRAMING_HEADERS or lower in tokens
                or (not response and lower.startswith(b"x-forwarded-"))):
            continue
        out.append(name + b": " + value)
    if not response:
        hosts = header_values(message, b"host")
        out.extend([
            b"X-Forwarded-For: 127.0.0.1",
            b"X-Forwarded-Proto: http",
            b"X-Forwarded-Host: " + (hosts[0] if hosts else b""),
        ])
    if upgrade:
        out.extend([b"Connection: Upgrade", b"Upgrade: " + message.upgrade_protocol])
    elif response and message.status is not None and 100 <= message.status < 200:
        pass
    else:
        if message.chunked:
            if message.trailer_names:
                out.append(b"Trailer: " + b", ".join(message.trailer_names))
            out.append(b"Transfer-Encoding: chunked")
        elif message.declared_length is not None:
            out.append(b"Content-Length: " + str(message.declared_length).encode())
        out.append(b"Connection: close")
    return b"\r\n".join(out) + b"\r\n\r\n"


def forwarded_request(message):
    return _forwarded_headers(message)


def forwarded_upgrade(message):
    return _forwarded_headers(message, upgrade=True)


def forwarded_response(message):
    return _forwarded_headers(message, response=True)


async def copy_exact(reader, writer, length, backend=False):
    remaining = length
    while remaining:
        try:
            data = await asyncio.wait_for(reader.read(min(65536, remaining)), IDLE_TIMEOUT)
        except asyncio.TimeoutError as error:
            raise ProtocolError(502, "Bad Gateway") if backend else ProtocolError(
                408, "Request Timeout") from error
        if not data:
            raise ProtocolError(502, "Bad Gateway") if backend else EOFError("unexpected EOF")
        writer.write(data)
        await writer.drain()
        remaining -= len(data)


async def copy_chunked(reader, writer, declared_trailers=(), backend=False):
    error = ProtocolError(502, "Bad Gateway") if backend else ProtocolError()
    while True:
        try:
            line = await idle_readline(reader)
        except ProtocolError:
            if backend:
                raise ProtocolError(502, "Bad Gateway")
            raise
        if not line or len(line) > MAX_LINE:
            raise error
        match = CHUNK_RE.fullmatch(line)
        if not match:
            raise error
        size = int(match.group("size"), 16)
        writer.write(line)
        await writer.drain()
        if size:
            await copy_exact(reader, writer, size, backend)
            try:
                ending = await asyncio.wait_for(reader.readexactly(2), IDLE_TIMEOUT)
            except (asyncio.IncompleteReadError, asyncio.TimeoutError) as caught:
                raise error from caught
            if ending != b"\r\n":
                raise error
            writer.write(ending)
            await writer.drain()
            continue
        total = 0
        seen = set()
        while True:
            try:
                trailer = await idle_readline(reader)
            except ProtocolError:
                if backend:
                    raise ProtocolError(502, "Bad Gateway")
                raise
            total += len(trailer)
            if (not trailer or len(trailer) > MAX_LINE or total > MAX_TRAILERS
                    or not trailer.endswith(b"\r\n")):
                raise error
            if trailer == b"\r\n":
                writer.write(trailer)
                await writer.drain()
                return
            parsed, _ = _parse_header_lines([trailer[:-2]], 502 if backend else 400)
            name = parsed[0][0].lower()
            if name in FORBIDDEN_TRAILERS or name not in declared_trailers or name in seen:
                raise error
            seen.add(name)
            writer.write(trailer)
            await writer.drain()


async def relay(source, target):
    while True:
        data = await source.read(65536)
        if not data:
            return
        target.write(data)
        await target.drain()


async def close_writer(writer):
    if not writer:
        return
    writer.close()
    with contextlib.suppress(Exception):
        await asyncio.wait_for(writer.wait_closed(), CLOSE_TIMEOUT)


class Gateway:
    def __init__(self, control_path, http_path, state_path=None, clean_stale=False, clock=time.time):
        self.control_path = Path(control_path)
        self.http_path = Path(http_path)
        self.state_path = Path(state_path) if state_path else self.control_path.parent / "routes.json"
        self.clean_stale = clean_stale
        self.clock = clock
        self.routes = {}
        self.connections = {}
        self.lock = asyncio.Lock()
        self.control_server = None
        self.http_server = None
        self.socket_ids = {}
        self.stopping = asyncio.Event()
        self.gc_task = None

    def _validate_route(self, value):
        if not isinstance(value, dict) or set(value) != {
                "name", "port", "generation", "created_at", "updated_at", "expires_at"}:
            raise ValueError("invalid route fields")
        name = value["name"]
        port = value["port"]
        generation = value["generation"]
        timestamps = (value["created_at"], value["updated_at"], value["expires_at"])
        if (not isinstance(name, str) or not NAME_RE.fullmatch(name)
                or not isinstance(port, int) or isinstance(port, bool) or not 1 <= port <= 65535
                or not isinstance(generation, str) or not re.fullmatch(r"[0-9a-f]{32}", generation)
                or any(not isinstance(item, (int, float)) or isinstance(item, bool) for item in timestamps)
                or not timestamps[0] <= timestamps[1] < timestamps[2]):
            raise ValueError("invalid route value")
        return Route(name, port, generation, *map(float, timestamps))

    def _load_state(self):
        try:
            info = self.state_path.lstat()
        except FileNotFoundError:
            return
        if (not stat.S_ISREG(info.st_mode) or stat.S_ISLNK(info.st_mode)
                or info.st_uid != os.getuid() or stat.S_IMODE(info.st_mode) != 0o600):
            raise RuntimeError(f"unsafe route state file: {self.state_path}")
        try:
            value = json.loads(self.state_path.read_text())
            if not isinstance(value, dict) or set(value) != {"version", "routes"} or value["version"] != VERSION:
                raise ValueError("invalid state envelope")
            routes = [self._validate_route(item) for item in value["routes"]]
            if len({route.name for route in routes}) != len(routes):
                raise ValueError("duplicate route name")
        except (OSError, UnicodeDecodeError, json.JSONDecodeError, ValueError, TypeError) as error:
            corrupt = self.state_path.with_name(
                f"routes.json.corrupt-{int(self.clock())}-{secrets.token_hex(4)}")
            try:
                os.replace(self.state_path, corrupt)
                directory_fd = os.open(self.state_path.parent, os.O_RDONLY | os.O_DIRECTORY)
                try:
                    os.fsync(directory_fd)
                finally:
                    os.close(directory_fd)
            except OSError as rename_error:
                raise RuntimeError(f"corrupt route state (could not preserve it): {rename_error}") from error
            raise RuntimeError(f"corrupt route state preserved as {corrupt}") from error
        self.routes = {route.name: route for route in routes if route.expires_at > self.clock()}

    def _persist_locked(self):
        self.state_path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
        info = self.state_path.parent.lstat()
        if (not stat.S_ISDIR(info.st_mode) or stat.S_ISLNK(info.st_mode)
                or info.st_uid != os.getuid() or info.st_mode & 0o077):
            raise RuntimeError(f"unsafe state directory: {self.state_path.parent}")
        temporary = self.state_path.with_name(f".routes.json.{os.getpid()}.{secrets.token_hex(4)}")
        data = json.dumps({"version": VERSION, "routes": [
            route.as_dict() for route in sorted(self.routes.values(), key=lambda item: item.name)
        ]}, separators=(",", ":"), sort_keys=True).encode() + b"\n"
        fd = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
        try:
            with os.fdopen(fd, "wb", closefd=False) as output:
                output.write(data)
                output.flush()
                os.fsync(output.fileno())
            os.close(fd)
            fd = -1
            os.replace(temporary, self.state_path)
            os.chmod(self.state_path, 0o600)
            directory_fd = os.open(self.state_path.parent, os.O_RDONLY | os.O_DIRECTORY)
            try:
                os.fsync(directory_fd)
            finally:
                os.close(directory_fd)
        finally:
            if fd >= 0:
                os.close(fd)
            with contextlib.suppress(FileNotFoundError):
                temporary.unlink()

    async def _close_generations(self, generations):
        groups = [self.connections.pop(generation, None) for generation in generations]
        groups = [group for group in groups if group]
        writers = [writer for group in groups for writer in list(group.writers)]
        for writer in writers:
            writer.close()
        current = asyncio.current_task()
        tasks = [task for group in groups for task in list(group.tasks)
                 if task is not current and not task.done()]
        for task in tasks:
            task.cancel()
        if tasks:
            with contextlib.suppress(asyncio.TimeoutError):
                await asyncio.wait_for(asyncio.gather(*tasks, return_exceptions=True), CLOSE_TIMEOUT)
        await asyncio.gather(*(close_writer(writer) for writer in writers), return_exceptions=True)

    async def _gc_locked(self):
        now = self.clock()
        expired = [route for route in self.routes.values() if route.expires_at <= now]
        if not expired:
            return set()
        previous_routes = self.routes.copy()
        for route in expired:
            self.routes.pop(route.name, None)
        try:
            self._persist_locked()
        except BaseException:
            self.routes = previous_routes
            raise
        return {route.generation for route in expired}

    async def gc(self):
        async with self.lock:
            generations = await self._gc_locked()
        await self._close_generations(generations)

    async def _gc_loop(self):
        try:
            while True:
                await asyncio.sleep(GC_INTERVAL)
                await self.gc()
        except asyncio.CancelledError:
            pass

    async def start(self):
        self._load_state()
        async with self.lock:
            expired = await self._gc_locked()
            self._persist_locked()
        await self._close_generations(expired)
        for path in (self.control_path, self.http_path):
            try:
                info = path.lstat()
            except FileNotFoundError:
                continue
            if not self.clean_stale or not stat.S_ISSOCK(info.st_mode) or info.st_uid != os.getuid():
                raise RuntimeError(f"refusing to remove existing socket path: {path}")
            path.unlink()
        self.control_server = await asyncio.start_unix_server(self.handle_control, self.control_path)
        os.chmod(self.control_path, 0o600)
        info = self.control_path.lstat()
        self.socket_ids[self.control_path] = (info.st_dev, info.st_ino)
        try:
            self.http_server = await asyncio.start_unix_server(self.handle_http, self.http_path)
        except BaseException:
            self.control_server.close()
            await self.control_server.wait_closed()
            self._unlink_owned(self.control_path)
            raise
        os.chmod(self.http_path, 0o600)
        info = self.http_path.lstat()
        self.socket_ids[self.http_path] = (info.st_dev, info.st_ino)
        self.gc_task = asyncio.create_task(self._gc_loop())

    def _unlink_owned(self, path):
        identity = self.socket_ids.get(path)
        if identity is None:
            return
        try:
            info = path.lstat()
            if stat.S_ISSOCK(info.st_mode) and (info.st_dev, info.st_ino) == identity:
                path.unlink()
        except FileNotFoundError:
            pass

    async def close(self):
        if self.gc_task:
            self.gc_task.cancel()
            await asyncio.gather(self.gc_task, return_exceptions=True)
        for server in (self.control_server, self.http_server):
            if server:
                server.close()
        await asyncio.gather(*(
            server.wait_closed() for server in (self.control_server, self.http_server) if server))
        await self._close_generations(set(self.connections))
        for path in (self.control_path, self.http_path):
            self._unlink_owned(path)

    async def send_json(self, writer, value):
        writer.write(json.dumps(value, separators=(",", ":")).encode() + b"\n")
        await writer.drain()

    @staticmethod
    def _valid_names(names):
        return (isinstance(names, list) and names and len(names) <= MAX_NAMES
                and all(isinstance(name, str) and NAME_RE.fullmatch(name) for name in names)
                and len(set(names)) == len(names))

    async def handle_control(self, reader, writer):
        try:
            if peer_uid(writer) != os.getuid():
                return
            try:
                line = await asyncio.wait_for(reader.readline(), IDLE_TIMEOUT)
            except asyncio.TimeoutError:
                return
            if not line or len(line) > MAX_CONTROL or not line.endswith(b"\n"):
                return
            request = json.loads(line)
            if not isinstance(request, dict) or request.get("version") != VERSION:
                return
            op = request.get("op")
            if op == "ping":
                await self.send_json(writer, {"ok": True, "version": VERSION})
                return
            if op in ("stop", "force_stop"):
                await self.send_json(writer, {"ok": True})
                self.stopping.set()
                return
            generations = set()
            response = None
            async with self.lock:
                generations |= await self._gc_locked()
                if op == "list" and set(request) == {"version", "op"}:
                    response = {"ok": True, "routes": [route.as_dict() for route in
                                sorted(self.routes.values(), key=lambda item: item.name)]}
                elif op == "expose" and set(request) == {"version", "op", "names", "port"}:
                    names = request.get("names")
                    port = request.get("port")
                    if not self._valid_names(names):
                        response = {"ok": False, "code": "invalid_name",
                                    "message": "invalid or duplicate route name"}
                    elif (not isinstance(port, int) or isinstance(port, bool)
                          or not 1 <= port <= 65535):
                        response = {"ok": False, "code": "invalid_port",
                                    "message": "port must be in 1..65535"}
                    else:
                        now = self.clock()
                        generations_by_name = {
                            name: secrets.token_hex(16) for name in names}
                        old = [self.routes.get(name) for name in names]
                        previous_routes = self.routes.copy()
                        try:
                            for name, previous in zip(names, old):
                                created = previous.created_at if previous else now
                                self.routes[name] = Route(
                                    name, port, generations_by_name[name], created, now,
                                    now + ROUTE_TTL)
                            self._persist_locked()
                        except BaseException:
                            self.routes = previous_routes
                            raise
                        generations |= {route.generation for route in old if route}
                        response = {"ok": True, "generations": generations_by_name,
                                    "expires_at": now + ROUTE_TTL}
                elif op == "remove" and set(request) == {"version", "op", "names"}:
                    names = request.get("names")
                    if not self._valid_names(names):
                        response = {"ok": False, "code": "invalid_name",
                                    "message": "invalid or duplicate route name"}
                    else:
                        previous_routes = self.routes.copy()
                        removed = [self.routes.pop(name) for name in names if name in self.routes]
                        try:
                            self._persist_locked()
                        except BaseException:
                            self.routes = previous_routes
                            raise
                        generations |= {route.generation for route in removed}
                        response = {"ok": True, "removed": len(removed)}
                elif op == "clear" and set(request) == {"version", "op"}:
                    previous_routes = self.routes.copy()
                    generations |= {route.generation for route in self.routes.values()}
                    count = len(self.routes)
                    self.routes.clear()
                    try:
                        self._persist_locked()
                    except BaseException:
                        self.routes = previous_routes
                        raise
                    response = {"ok": True, "removed": count}
                else:
                    response = {"ok": False, "code": "invalid_request",
                                "message": "invalid control request"}
            await self._close_generations(generations)
            await self.send_json(writer, response)
        except (json.JSONDecodeError, UnicodeDecodeError, ConnectionError, OSError, RuntimeError):
            pass
        finally:
            await close_writer(writer)

    @staticmethod
    def route_name(message):
        hosts = header_values(message, b"host")
        if len(hosts) != 1:
            return None
        try:
            host = hosts[0].decode("ascii").lower()
        except UnicodeDecodeError:
            return None
        if host.endswith(":17890"):
            host = host[:-6]
        match = re.fullmatch(r"([a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?)\.localhost", host)
        return match.group(1) if match else None

    async def error(self, writer, status, message):
        body = (message + "\n").encode()
        writer.write(f"HTTP/1.1 {status} {message}\r\nContent-Type: text/plain\r\n"
                     f"Content-Length: {len(body)}\r\nConnection: close\r\n\r\n".encode() + body)
        await writer.drain()

    async def handle_http(self, reader, writer):
        backend_writer = None
        route = None
        group = None
        task = asyncio.current_task()
        response_started = False
        try:
            if peer_uid(writer) != os.getuid():
                return
            lines = await read_header_block(reader)
            if lines is None:
                return
            request = parse_message(lines)
            method = request.start.split(b" ", 1)[0].upper()
            if header_values(request, b"expect"):
                await self.error(writer, 417, "Expectation Failed")
                return
            if method == b"PRI" or request.upgrade_protocol == b"h2c":
                raise ProtocolError()
            if request.upgrade_protocol and (request.chunked or request.body_length):
                raise ProtocolError()
            name = self.route_name(request)
            generations = set()
            async with self.lock:
                generations |= await self._gc_locked()
                route = self.routes.get(name)
                if route:
                    group = self.connections.setdefault(route.generation, ConnectionGroup())
                    group.tasks.add(task)
                    group.writers.add(writer)
            await self._close_generations(generations)
            if not route:
                await self.error(writer, 404, "Not Found")
                return
            try:
                backend_reader, backend_writer = await asyncio.wait_for(
                    asyncio.open_connection("127.0.0.1", route.port), 5)
            except (OSError, asyncio.TimeoutError):
                await self.error(writer, 502, "Bad Gateway")
                return
            group.writers.add(backend_writer)
            backend_writer.write(forwarded_upgrade(request) if request.upgrade_protocol
                                 else forwarded_request(request))
            await backend_writer.drain()
            if request.chunked:
                await copy_chunked(reader, backend_writer, request.trailer_names)
            elif request.body_length:
                await copy_exact(reader, backend_writer, request.body_length)

            while True:
                response_lines = await read_header_block(backend_reader, backend=True)
                if response_lines is None:
                    raise ProtocolError(502, "Bad Gateway")
                response = parse_message(response_lines, request=False, request_method=method)
                status = response.status
                if status == 101:
                    if (not request.upgrade_protocol
                            or response.upgrade_protocol != request.upgrade_protocol
                            or b"upgrade" not in connection_tokens(response)):
                        raise ProtocolError(502, "Bad Gateway")
                    writer.write(_forwarded_headers(response, response=True, upgrade=True))
                    await writer.drain()
                    response_started = True
                    copies = [asyncio.create_task(relay(reader, backend_writer)),
                              asyncio.create_task(relay(backend_reader, writer))]
                    done, pending = await asyncio.wait(copies, return_when=asyncio.FIRST_COMPLETED)
                    for item in pending:
                        item.cancel()
                    await asyncio.gather(*done, *pending, return_exceptions=True)
                    return
                if 100 <= status < 200:
                    writer.write(forwarded_response(response))
                    await writer.drain()
                    response_started = True
                    continue
                writer.write(forwarded_response(response))
                await writer.drain()
                response_started = True
                if response.chunked:
                    await copy_chunked(backend_reader, writer, response.trailer_names, backend=True)
                elif response.body_length is not None:
                    if response.body_length:
                        await copy_exact(backend_reader, writer, response.body_length, backend=True)
                else:
                    await relay(backend_reader, writer)
                return
        except ProtocolError as error:
            if not response_started:
                with contextlib.suppress(Exception):
                    await self.error(writer, error.status, error.message)
        except (EOFError, ConnectionError, OSError, asyncio.CancelledError):
            pass
        finally:
            if group:
                group.tasks.discard(task)
                group.writers.discard(writer)
                if backend_writer:
                    group.writers.discard(backend_writer)
                if not group.tasks and not group.writers:
                    self.connections.pop(route.generation, None)
            await close_writer(backend_writer)
            await close_writer(writer)


async def control_request(path, request):
    reader, writer = await asyncio.wait_for(asyncio.open_unix_connection(path), 3)
    writer.write(json.dumps(request, separators=(",", ":")).encode() + b"\n")
    await writer.drain()
    line = await asyncio.wait_for(reader.readline(), 3)
    if not line:
        await close_writer(writer)
        raise RuntimeError("gateway closed the control connection")
    if len(line) > MAX_CONTROL:
        await close_writer(writer)
        raise RuntimeError("oversized gateway response")
    response = json.loads(line)
    await close_writer(writer)
    return response


async def run_gateway(control, http, state_path, ready_fd=None, lock_fd=None, pid_path=None):
    gateway = Gateway(control, http, state_path=state_path, clean_stale=True)
    try:
        await gateway.start()
        if pid_path:
            Path(pid_path).write_text(str(os.getpid()) + "\n")
        if ready_fd is not None:
            os.write(ready_fd, b"READY\n")
            os.close(ready_fd)
            ready_fd = None
        loop = asyncio.get_running_loop()
        for sig in (signal.SIGTERM, signal.SIGINT):
            with contextlib.suppress(NotImplementedError):
                loop.add_signal_handler(sig, gateway.stopping.set)
        await gateway.stopping.wait()
    except Exception as error:
        if ready_fd is not None:
            with contextlib.suppress(OSError):
                os.write(ready_fd, ("ERROR " + str(error) + "\n").encode()[:4096])
                os.close(ready_fd)
        raise
    finally:
        await gateway.close()
        if pid_path:
            with contextlib.suppress(FileNotFoundError):
                info = Path(pid_path).lstat()
                if stat.S_ISREG(info.st_mode) and info.st_uid == os.getuid():
                    Path(pid_path).unlink()
        if lock_fd is not None:
            os.close(lock_fd)


def daemonize(lock_fd, location):
    read_fd, write_fd = os.pipe()
    pid = os.fork()
    if pid == 0:
        try:
            os.close(read_fd)
            os.setsid()
            if os.fork() > 0:
                os._exit(0)
            os.umask(0o077)
            os.chdir("/")
            null = os.open("/dev/null", os.O_RDONLY)
            log = os.open(state_dir() / "gateway.log", os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0o600)
            os.dup2(null, 0)
            os.dup2(log, 1)
            os.dup2(log, 2)
            asyncio.run(run_gateway(location["control"], location["http"], location["state"],
                                    write_fd, lock_fd, location["pid"]))
        except BaseException:
            with contextlib.suppress(Exception):
                os.write(write_fd, b"ERROR daemon startup failed\n")
        finally:
            os._exit(0)
    os.close(write_fd)
    os.waitpid(pid, 0)
    ready, _, _ = select.select([read_fd], [], [], 5)
    data = os.read(read_fd, 4096) if ready else b""
    os.close(read_fd)
    if data != b"READY\n":
        raise RuntimeError(data.decode(errors="replace").strip() or "gateway startup timed out")


def ping(location):
    try:
        response = asyncio.run(control_request(location["control"], {"version": VERSION, "op": "ping"}))
        return response.get("ok") is True and response.get("version") == VERSION
    except (OSError, RuntimeError, asyncio.TimeoutError, json.JSONDecodeError):
        return False


def open_lock(location):
    lock_fd = os.open(location["lock"], os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW, 0o600)
    info = os.fstat(lock_fd)
    if info.st_uid != os.getuid() or not stat.S_ISREG(info.st_mode):
        os.close(lock_fd)
        raise RuntimeError("unsafe gateway lock")
    return lock_fd


def ensure(quiet=False, print_socket=False):
    location = paths()
    lock_fd = open_lock(location)
    try:
        try:
            fcntl.flock(lock_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
            owns_lock = True
        except BlockingIOError:
            owns_lock = False
        if owns_lock:
            if ping(location):
                raise RuntimeError("gateway lock is inconsistent with live control socket")
            daemonize(lock_fd, location)
        else:
            deadline = time.monotonic() + 5
            while not ping(location) and time.monotonic() < deadline:
                time.sleep(0.05)
            if not ping(location):
                raise RuntimeError("gateway owns the lock but did not answer readiness checks")
        if not ping(location):
            raise RuntimeError("gateway did not pass readiness check")
        if print_socket:
            print(location["http"])
        elif not quiet:
            print("preview gateway is ready")
    finally:
        # The daemon inherited this open file description and keeps its lock for its lifetime.
        os.close(lock_fd)


def command_expose(args):
    ensure(quiet=True)
    location = paths()
    response = asyncio.run(control_request(
        location["control"], {"version": VERSION, "op": "expose",
                              "names": args.names, "port": args.port}))
    if not response.get("ok"):
        raise RuntimeError(f"{response.get('code', 'error')}: {response.get('message', '')}")
    urls = ", ".join(f"http://{name}.localhost:17890" for name in args.names)
    print(f"exposing 127.0.0.1:{args.port} at {urls} until "
          f"{format_timestamp(response['expires_at'])}")


def format_timestamp(value):
    return time.strftime("%Y-%m-%d %H:%M:%S %z", time.localtime(value))


def wait_stopped(location, timeout=5):
    lock_fd = open_lock(location)
    try:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            try:
                fcntl.flock(lock_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
                return
            except BlockingIOError:
                time.sleep(0.05)
        raise RuntimeError("gateway shutdown timed out")
    finally:
        os.close(lock_fd)


def command_simple(op, names=None):
    location = paths()
    if not ping(location):
        raise RuntimeError("preview gateway is not running")
    request = {"version": VERSION, "op": op}
    if names is not None:
        request["names"] = names
    response = asyncio.run(control_request(location["control"], request))
    if not response.get("ok"):
        raise RuntimeError(response.get("message", "gateway request failed"))
    if op in ("stop", "force_stop"):
        wait_stopped(location)
    elif op == "list":
        print(f"{'NAME':<30} {'TARGET':<22} EXPIRES")
        for route in response["routes"]:
            print(f"{route['name']:<30} {'127.0.0.1:' + str(route['port']):<22} "
                  f"{format_timestamp(route['expires_at'])}")
    elif op == "ping":
        print(f"running (protocol {response['version']})")


def main(argv=None):
    parser = argparse.ArgumentParser(prog="preview")
    commands = parser.add_subparsers(dest="command", required=True)
    gateway = commands.add_parser("gateway")
    gateway_commands = gateway.add_subparsers(dest="gateway_command", required=True)
    ensure_parser = gateway_commands.add_parser("ensure")
    ensure_parser.add_argument("--quiet", action="store_true")
    ensure_parser.add_argument("--print-http-socket", action="store_true")
    gateway_commands.add_parser("status")
    stop = gateway_commands.add_parser("stop")
    stop.add_argument("--force", action="store_true",
                      help="stop through the healthy control socket")
    expose = commands.add_parser("expose")
    expose.add_argument("port", type=int)
    expose.add_argument("names", nargs="+")
    commands.add_parser("list")
    remove = commands.add_parser("remove")
    remove.add_argument("names", nargs="+")
    commands.add_parser("clear")
    args = parser.parse_args(argv)
    try:
        if args.command == "gateway":
            if args.gateway_command == "ensure":
                ensure(args.quiet, args.print_http_socket)
            elif args.gateway_command == "status":
                command_simple("ping")
            else:
                command_simple("force_stop" if args.force else "stop")
        elif args.command == "expose":
            command_expose(args)
        elif args.command == "remove":
            ensure(quiet=True)
            command_simple("remove", args.names)
        elif args.command == "clear":
            ensure(quiet=True)
            command_simple("clear")
        else:
            ensure(quiet=True)
            command_simple("list")
        return 0
    except (RuntimeError, OSError, ValueError) as error:
        print(f"preview: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
