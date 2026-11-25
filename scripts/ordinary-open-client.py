#!/usr/bin/env -S python3 -B -S

import json
import os
import socket
import stat
import struct
import sys
import time
from urllib.parse import urlsplit

MAX_MESSAGE_SIZE = 64 * 1024
LEASE_VERSION = 1
LEASE_MAX_SIZE = 64 * 1024
LEASE_MAX_LIFETIME = 60.0
DESKTOP_FOCUS_TIMEOUT = 2.0
QTILE_MAX_REPLY = 64 * 1024


def _desktop_socket_str():
    injected = os.environ.get("ABUSH_QTILE_SOCKET")
    if injected:
        if not os.path.isabs(injected):
            raise RuntimeError("Qtile socket path is not absolute")
        return injected
    cache = os.path.join(
        os.environ.get("XDG_CACHE_HOME", os.path.join(os.path.expanduser("~"), ".cache")),
        "qtile",
    )
    display = os.environ.get("WAYLAND_DISPLAY") or os.environ.get("DISPLAY")
    if display:
        return os.path.join(cache, f"qtilesocket.{display}")
    for display in ("wayland-0", ":0"):
        path = os.path.join(cache, f"qtilesocket.{display}")
        if os.path.exists(path):
            return path
    raise RuntimeError("Qtile socket is unavailable")


def desktop_socket_path():
    import pathlib
    return pathlib.Path(_desktop_socket_str())


def focus_browser_desktop(deadline=None):
    local_deadline = time.monotonic() + DESKTOP_FOCUS_TIMEOUT
    deadline = local_deadline if deadline is None else min(local_deadline, deadline)

    def remaining():
        budget = deadline - time.monotonic()
        if budget <= 0:
            raise TimeoutError("Qtile desktop switch deadline expired")
        return budget

    remaining()
    path = _desktop_socket_str()
    before = os.lstat(path)
    if stat.S_ISLNK(before.st_mode) or not stat.S_ISSOCK(before.st_mode) or \
            before.st_uid != os.getuid() or stat.S_IMODE(before.st_mode) != 0o600:
        raise RuntimeError("Qtile socket is unsafe")
    request = json.dumps([[["group", "f"]], "toscreen", [], {}, True],
                         separators=(",", ":")).encode()
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as connection:
        connection.settimeout(remaining())
        connection.connect(path)
        remaining()
        if hasattr(socket, "SO_PEERCRED"):
            credentials = connection.getsockopt(socket.SOL_SOCKET, socket.SO_PEERCRED,
                                               struct.calcsize("3i"))
            if struct.unpack("3i", credentials)[1] != os.getuid():
                raise RuntimeError("Qtile socket peer is not owned by this user")
        after = os.lstat(path)
        if (before.st_dev, before.st_ino) != (after.st_dev, after.st_ino):
            raise RuntimeError("Qtile socket changed during connection")
        connection.settimeout(remaining())
        connection.sendall(request)
        remaining()
        connection.shutdown(socket.SHUT_WR)
        response = bytearray()
        while True:
            connection.settimeout(remaining())
            chunk = connection.recv(min(4096, QTILE_MAX_REPLY + 1 - len(response)))
            remaining()
            if not chunk:
                break
            response.extend(chunk)
            if len(response) > QTILE_MAX_REPLY:
                raise RuntimeError("Qtile desktop switch reply is too large")
    result = json.loads(response)
    if not isinstance(result, list) or len(result) != 2 or type(result[0]) is not int:
        raise RuntimeError("invalid Qtile desktop switch reply")
    if result[0] != 0:
        raise RuntimeError("Qtile desktop switch command failed")
    if result[1] is not None:
        raise RuntimeError("unexpected Qtile desktop switch result")


def runtime_root():
    runtime = os.environ.get("XDG_RUNTIME_DIR")
    if not runtime:
        raise RuntimeError("XDG_RUNTIME_DIR is unavailable")
    return os.path.join(runtime, "abush")


def lease_root():
    injected = os.environ.get("ABUSH_ORDINARY_LAUNCH_LEASE_DIR")
    return injected if injected else os.path.join(runtime_root(), "ordinary-launch")


def secure_directory(path):
    os.makedirs(path, mode=0o700, exist_ok=True)
    info = os.lstat(path)
    if stat.S_ISLNK(info.st_mode) or not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid():
        raise RuntimeError("ordinary launch lease directory is unsafe")
    if stat.S_IMODE(info.st_mode) != 0o700:
        os.chmod(path, 0o700)


def profile_hash(profile):
    import hashlib
    return hashlib.sha256(
        os.fsencode(os.path.realpath(os.path.expanduser(profile)))
    ).hexdigest()


def write_launch_lease(url, request_id, deadline_ms, profile):
    import hashlib
    root = lease_root()
    secure_directory(os.path.dirname(root))
    secure_directory(root)
    lease = {
        "version": LEASE_VERSION,
        "request_id": request_id,
        "url": url,
        "url_hash": hashlib.sha256(url.encode()).hexdigest(),
        "deadline_ms": deadline_ms,
        "profile_hash": profile_hash(profile),
        "boot_target": "next",
    }
    payload = json.dumps(lease, separators=(",", ":")).encode()
    if len(payload) > LEASE_MAX_SIZE:
        raise RuntimeError("ordinary launch lease is too large")
    final = os.path.join(root, f"{request_id}.json")
    temporary = os.path.join(root, f".{request_id}.{os.urandom(16).hex()}.tmp")
    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
    if hasattr(os, "O_NOFOLLOW"):
        flags |= os.O_NOFOLLOW
    descriptor = os.open(temporary, flags, 0o600)
    try:
        os.write(descriptor, payload)
        os.fsync(descriptor)
    finally:
        os.close(descriptor)
    try:
        if os.path.lexists(final):
            raise RuntimeError("ordinary launch lease already exists")
        os.link(temporary, final, follow_symlinks=False)
        os.unlink(temporary)
    except BaseException:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise
    return final


def remove_launch_lease(path):
    if path is None:
        return
    path_str = os.fspath(path)
    if not path_str.endswith(".json"):
        return
    try:
        info = os.lstat(path_str)
        if stat.S_ISLNK(info.st_mode) or not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid():
            raise RuntimeError("ordinary launch lease changed identity")
        os.unlink(path_str)
    except FileNotFoundError:
        pass


def socket_path():
    injected = os.environ.get("ABUSH_ORDINARY_OPEN_SOCKET")
    if injected:
        return injected
    return os.path.join(runtime_root(), "ordinary-open.sock")


def encode(message):
    payload = json.dumps(message, separators=(",", ":")).encode()
    if len(payload) > MAX_MESSAGE_SIZE:
        raise ValueError("ordinary-open request is too large")
    return struct.pack(">I", len(payload)) + payload


def read_exact(sock, size):
    chunks = []
    while size:
        chunk = sock.recv(size)
        if not chunk:
            raise EOFError("ordinary-open broker closed without a completion")
        chunks.append(chunk)
        size -= len(chunk)
    return b"".join(chunks)


def receive(sock):
    size = struct.unpack(">I", read_exact(sock, 4))[0]
    if size > MAX_MESSAGE_SIZE:
        raise ValueError("ordinary-open completion is too large")
    result = json.loads(read_exact(sock, size))
    if not isinstance(result, dict):
        raise ValueError("ordinary-open completion is not an object")
    return result


def connect(path, timeout):
    deadline = time.monotonic() + timeout
    last_error = None
    while True:
        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        client.settimeout(max(0.01, deadline - time.monotonic()))
        try:
            info = os.lstat(path)
            if stat.S_ISLNK(info.st_mode) or not stat.S_ISSOCK(info.st_mode) or \
                    info.st_uid != os.getuid() or stat.S_IMODE(info.st_mode) != 0o600:
                raise RuntimeError("ordinary-open socket is unsafe")
            client.connect(path)
            return client
        except (FileNotFoundError, ConnectionRefusedError) as error:
            last_error = error
            client.close()
            if time.monotonic() >= deadline:
                raise TimeoutError("ordinary-open socket did not become ready") from last_error
            time.sleep(min(0.05, max(0, deadline - time.monotonic())))
        except BaseException:
            client.close()
            raise


def main():
    if sys.argv[1:] == ["--focus-desktop"]:
        focus_browser_desktop()
        return
    if len(sys.argv) != 2:
        raise RuntimeError("usage: ordinary-open-client URL | --focus-desktop")
    url = sys.argv[1]
    parsed = urlsplit(url)
    if parsed.scheme not in {"http", "https", "file"} or \
            (parsed.scheme in {"http", "https"} and not parsed.hostname) or \
            (parsed.scheme == "file" and (not url.startswith("file:///") or parsed.netloc)):
        raise RuntimeError("unsupported ordinary-open URL")
    timeout = float(os.environ.get("ABUSH_ORDINARY_OPEN_TIMEOUT", "30"))
    if timeout <= 0 or timeout > LEASE_MAX_LIFETIME:
        raise RuntimeError("ABUSH_ORDINARY_OPEN_TIMEOUT is outside its safe range")
    injected_deadline = os.environ.get("ABUSH_ORDINARY_OPEN_DEADLINE_MS")
    deadline_ms = int(injected_deadline) if injected_deadline else \
        int(time.time() * 1000 + timeout * 1000)
    now_ms = int(time.time() * 1000)
    if deadline_ms <= now_ms or deadline_ms - now_ms > int(LEASE_MAX_LIFETIME * 1000):
        raise RuntimeError("ABUSH_ORDINARY_OPEN_DEADLINE_MS is outside its safe range")
    request_id = os.environ.get("ABUSH_ORDINARY_OPEN_REQUEST_ID") or os.urandom(16).hex()
    if not isinstance(request_id, str) or len(request_id) != 32 or \
            any(character not in "0123456789abcdef" for character in request_id):
        raise RuntimeError("ABUSH_ORDINARY_OPEN_REQUEST_ID is invalid")
    request = {
        "version": 1,
        "request_id": request_id,
        "operation": "ordinary_open",
        "url": url,
        "focus": True,
        "deadline_ms": deadline_ms,
    }
    path = socket_path()
    lease = None
    inherited_lease = os.environ.get("ABUSH_ORDINARY_OPEN_LEASE_PATH")
    if inherited_lease:
        expected = os.path.join(lease_root(), f"{request_id}.json")
        if os.path.normpath(inherited_lease) != os.path.normpath(expected):
            raise RuntimeError("ABUSH_ORDINARY_OPEN_LEASE_PATH is invalid")
        lease = inherited_lease
    if os.environ.get("ABUSH_ORDINARY_OPEN_WRITE_LEASE") == "1":
        profile = os.environ.get("ABUSH_CHROME_PROFILE")
        if not profile:
            raise RuntimeError("ABUSH_CHROME_PROFILE is unavailable")
        lease = write_launch_lease(url, request_id, deadline_ms, profile)
        if os.environ.get("ABUSH_ORDINARY_OPEN_LEASE_ONLY") == "1":
            info = {
                "request_id": request_id,
                "deadline_ms": deadline_ms,
                "lease_path": lease,
            }
            metadata_path = os.environ.get("ABUSH_ORDINARY_OPEN_LEASE_METADATA")
            if metadata_path:
                with open(metadata_path, "w", encoding="utf-8") as stream:
                    stream.write(json.dumps(info, separators=(",", ":")))
            else:
                print(json.dumps(info, separators=(",", ":")))
            return
    try:
        with connect(path, timeout) as client:
            client.sendall(encode(request))
            client.settimeout(max(0.01, (deadline_ms - int(time.time() * 1000)) / 1000))
            completion = receive(client)
    finally:
        remove_launch_lease(lease)
    expected = {"version", "request_id", "operation", "completion"}
    if set(completion) != expected or type(completion.get("version")) is not int or \
            completion.get("version") != 1 or \
            completion.get("request_id") != request["request_id"] or \
            completion.get("operation") != "ordinary_open_completed" or \
            completion.get("completion") not in ("applied", "not_applied", "unknown"):
        raise RuntimeError("invalid ordinary-open completion")
    if completion["completion"] != "applied":
        raise RuntimeError(f"ordinary-open completed as {completion['completion']}")
    if os.environ.get("ABUSH_ORDINARY_OPEN_FOCUS_DESKTOP") == "1":
        try:
            focus_browser_desktop(time.monotonic() + (deadline_ms / 1000 - time.time()))
        except (EOFError, OSError, RuntimeError, TimeoutError, ValueError) as error:
            raise RuntimeError(
                f"URL opened; desktop switch failed: {error}. Do not retry the URL automatically"
            ) from error


if __name__ == "__main__":
    try:
        main()
    except (EOFError, OSError, RuntimeError, TimeoutError, ValueError,
            json.JSONDecodeError) as error:
        print(f"ordinary-open: {error}", file=sys.stderr)
        raise SystemExit(1)
