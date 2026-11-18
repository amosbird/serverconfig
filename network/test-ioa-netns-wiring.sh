#!/usr/bin/env bash
# Prove the iOA-in-netns wiring with the real unit files.
#
#   - ioa-netns.service creates netns `ioa`; the ngnclient drop-in runs the daemon inside it.
#   - ioa-port@.socket binds a host loopback port; ioa-port@.service runs systemd-socket-proxyd in
#     the namespace, so 127.0.0.1:12639 (corkscrew, PAC, browsers) and :56100/:56101 (the GUI)
#     keep answering although iOA listens only inside `ioa`.
#
# Two layers:
#   1. The unit files are checked for every setting the wiring depends on.
#   2. The same templates are instantiated as transient units with `systemd-run`, against a
#      throwaway namespace and a high port, so the real systemd does the socket binding, the
#      namespace switch and the activation. Every transient unit has RuntimeMaxSec and is stopped
#      on exit, and the whole run has a hard time limit.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
LIMIT=${TEST_LIMIT:-60}
NETNS_UNIT=$ROOT/network/systemd/ioa-netns.service
SOCKET_UNIT=$ROOT/network/systemd/ioa-port@.socket
PORT_UNIT=$ROOT/network/systemd/ioa-port@.service
DROPIN=$ROOT/systemd/ngnclient.service.d/netns.conf

fail() { echo "FAIL $*" >&2; exit 1; }
ok() { echo "OK   $*"; }

# ---- 1. unit contract --------------------------------------------------------------------------
grep -Fxq 'NetworkNamespacePath=/run/netns/ioa' "$DROPIN" || fail 'ngnclient is not placed in netns ioa'
grep -Fxq 'BindsTo=ioa-netns.service' "$DROPIN" ||
    fail 'ngnclient keeps running in the host namespace if netns ioa goes away'
grep -Fxq 'After=ioa-netns.service' "$DROPIN" || fail 'ngnclient can start before netns ioa exists'
grep -Eq '^Before=.*\bngnclient\.service\b' "$NETNS_UNIT" || fail 'ioa-netns.service is not ordered first'
grep -Fxq 'RemainAfterExit=yes' "$NETNS_UNIT" ||
    fail 'ioa-netns.service would be inactive after up, and BindsTo would stop ngnclient'
grep -Eq '^ExecStop=.* down$' "$NETNS_UNIT" || fail 'stopping ioa-netns.service leaves the namespace'
grep -Fxq 'ListenStream=127.0.0.1:%i' "$SOCKET_UNIT" || fail 'the port socket does not bind host loopback'
grep -q 'NetworkNamespacePath' "$SOCKET_UNIT" &&
    fail 'the port socket must bind on the host; NetworkNamespacePath there would bind it in ioa'
grep -Fxq 'NetworkNamespacePath=/run/netns/ioa' "$PORT_UNIT" || fail 'the port proxy does not run in ioa'
grep -Fxq 'ExecStart=/usr/lib/systemd/systemd-socket-proxyd 127.0.0.1:%i' "$PORT_UNIT" ||
    fail 'the port proxy does not forward to the same loopback port inside ioa'
grep -Fxq 'Requires=ioa-netns.service' "$PORT_UNIT" || fail 'the port proxy does not require the namespace'
for port in 12639 12649 12679 12759 12769 12101 56100 56101; do
    grep -Eq "ioa-port@\{([0-9]+,)*$port(,[0-9]+)*\}\.socket" "$ROOT/restore.sh" ||
        fail "restore.sh does not enable the socket for host port $port"
done
ok 'units: ngnclient in netns ioa and bound to it; host sockets proxied into ioa for every iOA port'

# ---- 2. the templates, run by the real systemd -------------------------------------------------
TAG=ioatest$$
PORT=$((47000 + $$ % 1000))
NS=/run/netns/$TAG
cleanup() {
    sudo -n systemctl stop "$TAG-port.socket" "$TAG-port.service" "$TAG-agent.service" \
        >/dev/null 2>&1 || true
    sudo -n systemctl reset-failed "$TAG-port.socket" "$TAG-port.service" "$TAG-agent.service" \
        >/dev/null 2>&1 || true
    sudo -n ip netns del "$TAG" 2>/dev/null || true
}
trap cleanup EXIT
sudo -n ip netns add "$TAG"
sudo -n ip -n "$TAG" link set lo up

unit_value() { sed -n "s/^$2=//p" "$1" | sed "s/%i/$PORT/g; s#/run/netns/ioa#$NS#g"; }

# The fake iOA listener: replies only after it sees EOF, with the byte count it received.
sudo -n systemd-run --quiet --unit="$TAG-agent" -p RuntimeMaxSec="$LIMIT" \
    -p NetworkNamespacePath="$NS" python3 -c '
import socket, sys
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(("127.0.0.1", int(sys.argv[1]))); s.listen(64)
while True:
    c, _ = s.accept(); d = b""
    while (x := c.recv(65536)): d += x
    c.sendall(b"agent " + str(len(d)).encode()); c.close()' "$PORT"
for _ in $(seq 50); do
    sudo -n ip netns exec "$TAG" ss -Hltn "sport = :$PORT" | grep -q . && break
    sleep 0.1
done
sudo -n ip netns exec "$TAG" ss -Hltn "sport = :$PORT" | grep -q . || fail 'fixture: agent did not listen'

probe() { timeout 5 python3 -c '
import socket, sys
try:
    s = socket.create_connection(("127.0.0.1", int(sys.argv[1])), timeout=2)
    s.sendall(b"x" * 23); s.shutdown(socket.SHUT_WR)
    r = b""
    while (d := s.recv(4096)): r += d
    print(r.decode())
except OSError as e:
    print("ERR", e.__class__.__name__)' "$PORT" || echo TIMEOUT; }
[ "$(probe)" = "ERR ConnectionRefusedError" ] ||
    fail 'fixture: the host reaches the namespace loopback without the proxy'

sudo -n systemd-run --quiet --unit="$TAG-port" -p RuntimeMaxSec="$LIMIT" \
    --socket-property=ListenStream="$(unit_value "$SOCKET_UNIT" ListenStream)" \
    -p NetworkNamespacePath="$(unit_value "$PORT_UNIT" NetworkNamespacePath)" \
    $(unit_value "$PORT_UNIT" ExecStart)

out=$(probe)
[ "$out" = "agent 23" ] || fail "host 127.0.0.1:$PORT should reach the listener in the namespace, got '$out'"
ok 'the host port reaches the listener in the namespace, and a half-close is passed through'

pid=$(systemctl show -p MainPID --value "$TAG-port.service")
# readlink on a /run/netns handle returns nothing (it is a bind mount, not a link); compare inodes.
[ "$(sudo -n readlink /proc/"$pid"/ns/net)" = "net:[$(sudo -n stat -L -c %i "$NS")]" ] ||
    fail 'the activated proxy is not running in the namespace'
ok 'the activated proxy runs in the namespace'

got=$(timeout 20 python3 - "$PORT" <<'PY'
import socket, sys, threading
port = int(sys.argv[1]); ok = []
def one():
    s = socket.create_connection(("127.0.0.1", port), timeout=10)
    s.sendall(b"y" * 300000); s.shutdown(socket.SHUT_WR)
    r = b""
    while (d := s.recv(65536)): r += d
    ok.append(r == b"agent 300000")
ts = [threading.Thread(target=one) for _ in range(20)]
[t.start() for t in ts]; [t.join() for t in ts]
print(sum(ok))
PY
) || got=TIMEOUT
[ "$got" = 20 ] || fail "20 concurrent 300KB round trips: $got succeeded"
ok '20 concurrent 300KB round trips through the activated proxy'

sudo -n systemctl stop "$TAG-port.service" 2>/dev/null
[ "$(probe)" = "agent 23" ] || fail 'a stopped proxy was not re-activated by the next connection'
ok 'a stopped proxy is re-activated by the next connection'
