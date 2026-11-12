#!/usr/bin/env python3

import hashlib
import json
import os
import pathlib
import socket
import stat
import struct
import sys
import time
import uuid
from urllib.parse import urlsplit

MAX_MESSAGE_SIZE = 64 * 1024
LEASE_VERSION = 1
LEASE_MAX_SIZE = 64 * 1024
LEASE_MAX_LIFETIME = 60.0


def runtime_root():
    runtime = os.environ.get("XDG_RUNTIME_DIR")
    if not runtime:
        raise RuntimeError("XDG_RUNTIME_DIR is unavailable")
    return pathlib.Path(runtime) / "abush"


def lease_root():
    injected = os.environ.get("ABUSH_ORDINARY_LAUNCH_LEASE_DIR")
    return pathlib.Path(injected) if injected else runtime_root() / "ordinary-launch"


def secure_directory(path):
    path.mkdir(mode=0o700, parents=True, exist_ok=True)
    info = path.lstat()
    if path.is_symlink() or not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid():
        raise RuntimeError("ordinary launch lease directory is unsafe")
    if stat.S_IMODE(info.st_mode) != 0o700:
        os.chmod(path, 0o700)


def profile_hash(profile):
    return hashlib.sha256(os.fsencode(pathlib.Path(profile).expanduser().resolve())).hexdigest()


def write_launch_lease(url, request_id, deadline_ms, profile):
    root = lease_root()
    secure_directory(root.parent)
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
    final = root / f"{request_id}.json"
    temporary = root / f".{request_id}.{uuid.uuid4().hex}.tmp"
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
        if final.exists() or final.is_symlink():
            raise RuntimeError("ordinary launch lease already exists")
        os.link(temporary, final, follow_symlinks=False)
        temporary.unlink()
    except BaseException:
        temporary.unlink(missing_ok=True)
        raise
    return final


def remove_launch_lease(path):
    if path is None:
        return
    candidates = [path]
    if path.suffix == ".json":
        candidates.append(path.with_suffix(".claimed"))
    for candidate in candidates:
        try:
            info = candidate.lstat()
            if candidate.is_symlink() or not stat.S_ISREG(info.st_mode) or \
                    info.st_uid != os.getuid():
                raise RuntimeError("ordinary launch lease changed identity")
            candidate.unlink()
        except FileNotFoundError:
            pass


def socket_path():
    injected = os.environ.get("ABUSH_ORDINARY_OPEN_SOCKET")
    if injected:
        return pathlib.Path(injected)
    return runtime_root() / "ordinary-open.sock"


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
            info = path.lstat()
            if path.is_symlink() or not stat.S_ISSOCK(info.st_mode) or \
                    info.st_uid != os.getuid() or stat.S_IMODE(info.st_mode) != 0o600:
                raise RuntimeError("ordinary-open socket is unsafe")
            client.connect(str(path))
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
    if len(sys.argv) != 2:
        raise RuntimeError("usage: ordinary-open-client URL")
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
    request_id = os.environ.get("ABUSH_ORDINARY_OPEN_REQUEST_ID") or uuid.uuid4().hex
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
        candidate = pathlib.Path(inherited_lease)
        expected = lease_root() / f"{request_id}.json"
        if candidate != expected:
            raise RuntimeError("ABUSH_ORDINARY_OPEN_LEASE_PATH is invalid")
        lease = candidate
    if os.environ.get("ABUSH_ORDINARY_OPEN_WRITE_LEASE") == "1":
        profile = os.environ.get("ABUSH_CHROME_PROFILE")
        if not profile:
            raise RuntimeError("ABUSH_CHROME_PROFILE is unavailable")
        lease = write_launch_lease(url, request_id, deadline_ms, profile)
        if os.environ.get("ABUSH_ORDINARY_OPEN_LEASE_ONLY") == "1":
            info = {
                "request_id": request_id,
                "deadline_ms": deadline_ms,
                "lease_path": str(lease),
            }
            metadata_path = os.environ.get("ABUSH_ORDINARY_OPEN_LEASE_METADATA")
            if metadata_path:
                pathlib.Path(metadata_path).write_text(
                    json.dumps(info, separators=(",", ":")), encoding="utf-8")
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
    if set(completion) != expected or completion.get("version") != 1 or \
            completion.get("request_id") != request["request_id"] or \
            completion.get("operation") != "ordinary_open_completed" or \
            completion.get("completion") not in {"applied", "not_applied", "unknown"}:
        raise RuntimeError("invalid ordinary-open completion")
    if completion["completion"] != "applied":
        raise RuntimeError(f"ordinary-open completed as {completion['completion']}")


if __name__ == "__main__":
    try:
        main()
    except (EOFError, OSError, RuntimeError, TimeoutError, ValueError,
            json.JSONDecodeError) as error:
        print(f"ordinary-open: {error}", file=sys.stderr)
        raise SystemExit(1)
