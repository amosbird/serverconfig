#!/usr/bin/env python3

import fcntl
import json
import os
import pathlib
import selectors
import socket
import stat
import struct
import sys
import threading
import time

REMOTE_KEY = "r_local000"
CONNECTION_ID = "c_local000"
PATH_OPERATION_LOCK = threading.Lock()


def secure_directory(path, create=False):
    if create:
        try:
            path.mkdir(mode=0o700)
        except FileExistsError:
            pass
    info = path.lstat()
    if path.is_symlink() or not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or \
            stat.S_IMODE(info.st_mode) != 0o700:
        raise RuntimeError(f"unsafe runtime directory: {path}")


def bind_socket(sock, path):
    with PATH_OPERATION_LOCK:
        previous = os.open(".", os.O_RDONLY)
        try:
            os.chdir(path.parent)
            sock.bind(path.name)
        finally:
            os.fchdir(previous)
            os.close(previous)


def connect_socket(sock, path):
    with PATH_OPERATION_LOCK:
        previous = os.open(".", os.O_RDONLY)
        try:
            os.chdir(path.parent)
            sock.connect(path.name)
        finally:
            os.fchdir(previous)
            os.close(previous)


RELAY_BUFFER_LIMIT = 1024 * 1024


def relay(client, target_path):
    upstream = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    upstream.settimeout(2)
    connect_socket(upstream, target_path)
    upstream.setblocking(False)
    client.setblocking(False)
    selector = selectors.DefaultSelector()
    sockets = (client, upstream)
    peers = {client: upstream, upstream: client}
    buffers = {client: bytearray(), upstream: bytearray()}
    read_open = {client: True, upstream: True}
    write_open = {client: True, upstream: True}

    def update(sock):
        events = 0
        if read_open[sock] and len(buffers[peers[sock]]) < RELAY_BUFFER_LIMIT:
            events |= selectors.EVENT_READ
        if write_open[sock] and buffers[sock]:
            events |= selectors.EVENT_WRITE
        if not events:
            try:
                selector.unregister(sock)
            except KeyError:
                pass
            return
        try:
            selector.modify(sock, events)
        except KeyError:
            selector.register(sock, events)

    def finish_write(sock):
        if write_open[sock] and not read_open[peers[sock]] and not buffers[sock]:
            try:
                sock.shutdown(socket.SHUT_WR)
            except OSError:
                pass
            write_open[sock] = False

    try:
        for sock in sockets:
            update(sock)
        while any(read_open.values()) or any(buffers.values()):
            for key, events in selector.select(30):
                sock = key.fileobj
                peer = peers[sock]
                if events & selectors.EVENT_READ:
                    try:
                        data = sock.recv(min(65536, RELAY_BUFFER_LIMIT - len(buffers[peer])))
                    except BlockingIOError:
                        data = None
                    if data:
                        buffers[peer].extend(data)
                    elif data == b"":
                        read_open[sock] = False
                        finish_write(peer)
                if events & selectors.EVENT_WRITE and buffers[sock]:
                    try:
                        sent = sock.send(buffers[sock])
                    except BlockingIOError:
                        sent = 0
                    if sent:
                        del buffers[sock][:sent]
                        finish_write(sock)
                update(sock)
                update(peer)
    finally:
        selector.close()
        client.close()
        upstream.close()


def registration_paths(runtime):
    connection = runtime / "abush" / "browser-remotes" / REMOTE_KEY / CONNECTION_ID
    return connection / "browser-peer.sock", connection / "connection.json"


def registration_ready(runtime):
    published, metadata = registration_paths(runtime)
    try:
        socket_info = published.lstat()
        metadata_info = metadata.lstat()
        if published.is_symlink() or metadata.is_symlink() or \
                not stat.S_ISSOCK(socket_info.st_mode) or \
                not stat.S_ISREG(metadata_info.st_mode) or \
                socket_info.st_uid != os.getuid() or metadata_info.st_uid != os.getuid() or \
                stat.S_IMODE(socket_info.st_mode) != 0o600 or \
                stat.S_IMODE(metadata_info.st_mode) != 0o600:
            return False
        data = json.loads(metadata.read_text())
        owner_pid = data.get("owner_pid")
        if not isinstance(owner_pid, int) or owner_pid <= 0:
            return False
        os.kill(owner_pid, 0)
        return data.get("remote_key") == REMOTE_KEY and \
            data.get("connection_id") == CONNECTION_ID and \
            data.get("browser_socket") == str(published)
    except (OSError, ValueError, json.JSONDecodeError):
        return False


def wait_ready(runtime, timeout):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if registration_ready(runtime):
            return True
        time.sleep(0.02)
    return registration_ready(runtime)


def main():
    runtime_value = os.environ.get("XDG_RUNTIME_DIR", "")
    runtime = pathlib.Path(runtime_value)
    if not runtime.is_absolute() or runtime == pathlib.Path("/"):
        raise RuntimeError("XDG_RUNTIME_DIR is not a safe absolute path")
    current = pathlib.Path("/")
    for component in runtime.parts[1:]:
        current /= component
        if current.is_symlink():
            raise RuntimeError(f"symlink in XDG_RUNTIME_DIR: {current}")
    secure_directory(runtime)

    abush = runtime / "abush"
    root = abush / "browser-remotes"
    remote = root / REMOTE_KEY
    connection = remote / CONNECTION_ID
    for directory in (abush, root, remote, connection):
        secure_directory(directory, create=True)

    target = abush / "browser-peer.sock"
    published = connection / "browser-peer.sock"
    metadata = connection / "connection.json"
    lock_path = connection / "publisher.lock"
    flags = os.O_RDWR | os.O_CREAT | getattr(os, "O_NOFOLLOW", 0)
    lock_fd = os.open(lock_path, flags, 0o600)
    os.fchmod(lock_fd, 0o600)
    try:
        fcntl.flock(lock_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        os.close(lock_fd)
        return
    if published.exists() or published.is_symlink():
        info = published.lstat()
        if stat.S_ISSOCK(info.st_mode) and info.st_uid == os.getuid():
            published.unlink()
        else:
            os.close(lock_fd)
            raise RuntimeError(f"unsafe local browser relay path: {published}")

    server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    bind_socket(server, published)
    os.chmod(published, 0o600)
    server.listen(4)
    server.settimeout(1)
    data = {
        "schema_version": 1,
        "remote_key": REMOTE_KEY,
        "connection_id": CONNECTION_ID,
        "display_name": "local",
        "browser_socket": str(published),
        "remote_browser_socket": str(target),
        "owner_pid": os.getpid(),
    }
    temporary = metadata.with_suffix(".tmp")
    temporary_flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_NOFOLLOW", 0)
    try:
        fd = os.open(temporary, temporary_flags, 0o600)
    except FileExistsError:
        info = temporary.lstat()
        if temporary.is_symlink() or not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid():
            raise RuntimeError(f"unsafe local browser metadata temporary: {temporary}")
        temporary.unlink()
        fd = os.open(temporary, temporary_flags, 0o600)
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as output:
            json.dump(data, output, sort_keys=True)
            output.write("\n")
        os.replace(temporary, metadata)
    except BaseException:
        try:
            temporary.unlink()
        except FileNotFoundError:
            pass
        raise

    missing_since = None
    try:
        while True:
            try:
                client, _ = server.accept()
            except socket.timeout:
                if target.exists():
                    missing_since = None
                elif missing_since is None:
                    missing_since = time.monotonic()
                elif time.monotonic() - missing_since > 30:
                    return
                continue
            try:
                credentials = client.getsockopt(socket.SOL_SOCKET, socket.SO_PEERCRED,
                                                 struct.calcsize("3i"))
                _, client_uid, _ = struct.unpack("3i", credentials)
                if client_uid != os.getuid():
                    client.close()
                    continue
                info = target.lstat()
                if target.is_symlink() or not stat.S_ISSOCK(info.st_mode) or \
                        info.st_uid != os.getuid():
                    client.close()
                    continue
                threading.Thread(target=relay, args=(client, target), daemon=True).start()
            except OSError:
                client.close()
    finally:
        server.close()
        try:
            published.unlink()
        except FileNotFoundError:
            pass
        try:
            metadata.unlink()
        except FileNotFoundError:
            pass
        os.close(lock_fd)


if __name__ == "__main__":
    try:
        if len(sys.argv) == 3 and sys.argv[1] == "--wait-ready":
            runtime = pathlib.Path(os.environ.get("XDG_RUNTIME_DIR", ""))
            if not wait_ready(runtime, float(sys.argv[2])):
                raise RuntimeError("local browser relay did not become ready")
        elif len(sys.argv) == 1:
            main()
        else:
            raise RuntimeError("usage: abush-browser-local.py [--wait-ready SECONDS]")
    except (OSError, RuntimeError, ValueError) as error:
        print(f"abush-browser-local: {error}", file=sys.stderr)
        raise SystemExit(1)
