#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cat >"$WORK/socket.py" <<'PY'
import os

AF_INET = 2
SOCK_STREAM = 1
SOL_SOCKET = 1
SO_MARK = 36
SO_BINDTODEVICE = 25


def record(*parts):
    with open(os.environ["SOCKET_LOG"], "a", encoding="utf-8") as output:
        print(*parts, file=output)


def if_nametoindex(interface):
    record("if_nametoindex", interface)
    return 3


class socket:
    def __init__(self, family, kind):
        record("socket", family, kind)

    def __enter__(self):
        return self

    def __exit__(self, *_):
        record("close")

    def setsockopt(self, level, option, value):
        record("setsockopt", level, option, repr(value))

    def bind(self, address):
        record("bind", repr(address))

    def settimeout(self, timeout):
        record("settimeout", timeout)

    def connect(self, address):
        record("connect", repr(address))
PY

SOCKET_LOG="$WORK/socket.log" PYTHONPATH="$WORK" \
    "$ROOT/scripts/network-probe-tcp" \
    --target 216.239.32.117 --port 80 --source 10.36.55.23 \
    --interface wlan0 --mark 0x80000 --timeout 2

cat >"$WORK/expected" <<'EOF'
if_nametoindex wlan0
socket 2 1
setsockopt 1 36 524288
setsockopt 1 25 b'wlan0\x00'
bind ('10.36.55.23', 0)
settimeout 2.0
connect ('216.239.32.117', 80)
close
EOF

cmp "$WORK/expected" "$WORK/socket.log"
echo "OK   TCP probe fixes mark, device, and source before connect"
