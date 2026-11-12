#!/usr/bin/env python3

import json
import os
import pathlib
import socket
import stat
import struct
import sys
import time
import uuid

MAX_MESSAGE_SIZE = 64 * 1024


def socket_path():
    injected = os.environ.get("ABUSH_ORDINARY_OPEN_SOCKET")
    if injected:
        return pathlib.Path(injected)
    runtime = os.environ.get("XDG_RUNTIME_DIR")
    if not runtime:
        raise RuntimeError("XDG_RUNTIME_DIR is unavailable")
    return pathlib.Path(runtime) / "abush" / "ordinary-open.sock"


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
    request = {
        "version": 1,
        "request_id": uuid.uuid4().hex,
        "operation": "ordinary_open",
        "url": sys.argv[1],
        "focus": True,
    }
    timeout = float(os.environ.get("ABUSH_ORDINARY_OPEN_TIMEOUT", "10"))
    if timeout <= 0:
        raise RuntimeError("ABUSH_ORDINARY_OPEN_TIMEOUT must be positive")
    path = socket_path()
    with connect(path, timeout) as client:
        client.sendall(encode(request))
        completion = receive(client)
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
