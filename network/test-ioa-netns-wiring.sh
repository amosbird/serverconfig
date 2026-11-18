#!/usr/bin/env bash
# Prove the iOA-in-netns wiring, in a sandbox that never touches the host:
#
#   - ioa-netns.service creates netns `ioa`; the ngnclient drop-in runs the daemon inside it.
#   - A fake daemon in ngnclient.service listens on 127.0.0.1:12639 and :56101 inside `ioa`, like
#     SmartGateAgent and iOA.bin do.
#   - ioa-netns-forward.service makes those answer on the host's 127.0.0.1, so corkscrew, the PAC
#     file and the GUI keep working unchanged.
#   - The fake daemon reaches the uplink through the namespace's default route.
#
# systemd itself cannot run in the sandbox, so the units are checked for exactly the settings the
# wiring depends on, and the behaviour those settings produce is exercised by hand in a sandbox
# (net, mount and PID namespaces; private /run/netns; hard time limit). When the sandbox exits, the
# kernel reaps every process in it, so nothing can hang the test.
#
# Usage: test-ioa-netns-wiring.sh [forwarder]   (the argument exists for mutation testing)
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
FORWARDER=$(realpath "${1:-$ROOT/network/ioa-netns-forward}")
LIMIT=${TEST_LIMIT:-60}

fail() { echo "FAIL $*" >&2; exit 1; }
ok() { echo "OK   $*"; }

if [ "${IOA_WIRING_SANDBOX:-}" != 1 ]; then
    # ---- static contract of the units, checked outside the sandbox ----------------------------
    netns_unit=$ROOT/network/systemd/ioa-netns.service
    fwd_unit=$ROOT/network/systemd/ioa-netns-forward.service
    dropin=$ROOT/systemd/ngnclient.service.d/netns.conf
    grep -Fxq 'NetworkNamespacePath=/run/netns/ioa' "$dropin" ||
        fail 'ngnclient is not placed in netns ioa'
    grep -Fxq 'BindsTo=ioa-netns.service' "$dropin" ||
        fail 'ngnclient keeps running in the host namespace if netns ioa goes away'
    grep -Fxq 'After=ioa-netns.service' "$dropin" || fail 'ngnclient can start before netns ioa exists'
    grep -Eq '^Before=.*\bngnclient\.service\b' "$netns_unit" || fail 'ioa-netns.service is not ordered first'
    grep -Fxq 'RemainAfterExit=yes' "$netns_unit" ||
        fail 'ioa-netns.service would be "dead" after up, and BindsTo would stop ngnclient'
    grep -Eq '^ExecStop=.* down$' "$netns_unit" || fail 'stopping ioa-netns.service leaves the namespace'
    for port in 12639 12649 12679 12759 12769 12101 56100 56101; do
        grep -q "127.0.0.1:$port\b" "$fwd_unit" || fail "host port $port is not forwarded into netns ioa"
    done
    grep -Fxq 'Requires=ioa-netns.service' "$fwd_unit" || fail 'the forwarder does not require the namespace'
    grep -Fxq 'Restart=always' "$fwd_unit" || fail 'a crashed forwarder is not restarted'
    ok 'units: ngnclient in netns ioa, ordered after it and bound to it; every iOA port forwarded'

    sudo -n timeout -k 5 "$LIMIT" \
        unshare --net --mount --pid --fork --kill-child --mount-proc --propagation private \
        env IOA_WIRING_SANDBOX=1 bash "$0" "$FORWARDER"
    [ -z "${1:-}" ] || exit 0
    mutate() {
        local work rc=0
        work=$(mktemp)
        sed "$2" "$FORWARDER" >"$work"
        if cmp -s "$FORWARDER" "$work"; then rm -f "$work"; fail "mutant $1 did not change the forwarder"; fi
        sudo -n timeout -k 5 "$LIMIT" \
            unshare --net --mount --pid --fork --kill-child --mount-proc --propagation private \
            env IOA_WIRING_SANDBOX=1 bash "$0" "$work" >/dev/null 2>&1 || rc=$?
        rm -f "$work"
        case $rc in
            0) fail "mutant $1 survived" ;;
            124 | 137) fail "mutant $1 hit the ${LIMIT}s limit instead of failing an assertion" ;;
        esac
    }
    mutate no-setns 's/os.setns(handle.fileno(), os.CLONE_NEWNET)/pass/'
    mutate setns-before-bind 's/^    listeners = \[\]$/    os.setns(open(args.netns, "rb").fileno(), os.CLONE_NEWNET); listeners = []/'
    mutate no-half-close 's/dst.shutdown(socket.SHUT_WR)/pass/'
    ok "mutation checks: every mutant fails an assertion"
    exit 0
fi

# ---- inside the sandbox -------------------------------------------------------------------------
mkdir -p /run/netns
mount -t tmpfs tmpfs /run/netns
ip link set lo up

# Host uplink to an "internet" namespace answering on 198.51.100.7:7000.
ip netns add up
ip link add wl0 type veth peer name up0 netns up
ip addr add 192.0.2.10/24 dev wl0
ip link set wl0 up
ip -n up link set lo up
ip -n up addr add 192.0.2.1/24 dev up0
ip -n up addr add 198.51.100.7/32 dev lo
ip -n up link set up0 up
ip -n up route add default via 192.0.2.10 dev up0
ip route add default via 192.0.2.1 dev wl0

IOA_NETNS=ioa bash "$ROOT/network/ioa-netns" up >/dev/null

py_listen() {  # py_listen <netns> <addr> <port> <reply>
    ip netns exec "$1" python3 -c '
import socket, sys
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind((sys.argv[1], int(sys.argv[2]))); s.listen(8)
while True:
    c, a = s.accept()
    data = b""
    while not data.endswith(b"\n"):
        chunk = c.recv(4096)
        if not chunk: break
        data += chunk
    c.sendall(sys.argv[3].encode() + b" " + a[0].encode() + b" " + data)
    c.shutdown(socket.SHUT_WR)
    while c.recv(4096): pass
    c.close()
' "$2" "$3" "$4" </dev/null >/dev/null 2>&1 &
}
wait_listen() {  # wait_listen <netns> <port>
    for _ in $(seq 50); do
        ip netns exec "$1" ss -Hltn "sport = :$2" | grep -q . && return 0
        sleep 0.1
    done
    fail "fixture: nothing listens on :$2 in $1"
}
# The fake iOA daemon: what SmartGateAgent and iOA.bin do on loopback inside the namespace.
py_listen ioa 127.0.0.1 12639 agent
py_listen ioa 127.0.0.1 56101 daemon
py_listen up 198.51.100.7 7000 internet
wait_listen ioa 12639
wait_listen ioa 56101
wait_listen up 7000

# Before the forwarder, the host's loopback must not reach iOA: that is the problem it solves.
host_call() {  # host_call <port> <payload>
    timeout 4 python3 -c '
import socket, sys
try:
    s = socket.create_connection(("127.0.0.1", int(sys.argv[1])), timeout=2)
    s.sendall(sys.argv[2].encode() + b"\n")
    s.shutdown(socket.SHUT_WR)
    out = b""
    while True:
        d = s.recv(4096)
        if not d: break
        out += d
    print(out.decode().strip())
except OSError as e:
    print("ERR", e.__class__.__name__)
' "$1" "$2" || echo TIMEOUT
}
out=$(host_call 12639 hello)
case $out in "ERR "*) ;; *) fail "fixture: the host already reaches the namespace's loopback: $out" ;; esac

python3 "$FORWARDER" --netns /run/netns/ioa 127.0.0.1:12639 127.0.0.1:56101 </dev/null >/dev/null 2>&1 &
for _ in $(seq 50); do ss -Hltn 'sport = :12639' | grep -q . && break; sleep 0.1; done

out=$(host_call 12639 proxy-request)
[ "$out" = "agent 127.0.0.1 proxy-request" ] ||
    fail "host 127.0.0.1:12639 should reach the agent inside netns ioa, got '$out'"
ok "host 127.0.0.1:12639 reaches SmartGateAgent's listener inside netns ioa"
out=$(host_call 56101 gui-hello)
[ "$out" = "daemon 127.0.0.1 gui-hello" ] ||
    fail "host 127.0.0.1:56101 should reach the daemon inside netns ioa, got '$out'"
ok "host 127.0.0.1:56101 reaches the daemon's GUI channel inside netns ioa"

# Many concurrent clients, and large payloads, through the forwarder.
got=$(timeout 10 python3 - <<'PY'
import socket, sys, threading
payload = "x" * 300000
results = []
def one(i):
    s = socket.create_connection(("127.0.0.1", 12639), timeout=5)
    s.sendall((f"{i}-" + payload + "\n").encode()); s.shutdown(socket.SHUT_WR)
    out = b""
    while (d := s.recv(65536)): out += d
    results.append(out.endswith((f"{i}-" + payload + "\n").encode()))
ts = [threading.Thread(target=one, args=(i,)) for i in range(20)]
[t.start() for t in ts]; [t.join() for t in ts]
print(sum(results))
PY
) || got=TIMEOUT
[ "$got" = 20 ] || fail "20 concurrent 300KB round trips through the forwarder: $got succeeded"
ok "20 concurrent 300KB round trips through the forwarder"

# A client that half-closes must still get the whole reply: the forwarder has to pass the EOF on
# instead of closing both directions. The fake agent here only answers after it sees EOF.
ip netns exec ioa python3 -c '
import socket
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(("127.0.0.1", 12649)); s.listen(8)
while True:
    c, _ = s.accept()
    data = b""
    while (d := c.recv(4096)): data += d
    c.sendall(b"after-eof " + data); c.close()
' </dev/null >/dev/null 2>&1 &
wait_listen ioa 12649
pkill -f "$FORWARDER" || true
python3 "$FORWARDER" --netns /run/netns/ioa 127.0.0.1:12639 127.0.0.1:12649 127.0.0.1:56101 \
    </dev/null >/dev/null 2>&1 &
for _ in $(seq 50); do ss -Hltn 'sport = :12649' | grep -q . && break; sleep 0.1; done
out=$(timeout 4 python3 -c '
import socket
s = socket.create_connection(("127.0.0.1", 12649), timeout=2)
s.sendall(b"request-without-newline"); s.shutdown(socket.SHUT_WR)
out = b""
while (d := s.recv(4096)): out += d
print(out.decode())' || echo TIMEOUT)
[ "$out" = "after-eof request-without-newline" ] ||
    fail "a half-closing client must still get the reply, got '$out'"
ok "a client's half-close is passed through, and the reply still arrives"

# The daemon itself reaches the uplink from inside the namespace.
out=$(ip netns exec ioa timeout 4 python3 -c '
import socket
s = socket.create_connection(("198.51.100.7", 7000), timeout=2)
s.sendall(b"heartbeat\n"); s.shutdown(socket.SHUT_WR)
print(s.recv(4096).decode().strip())' || echo TIMEOUT)
[ "$out" = "internet 192.0.2.10 heartbeat" ] ||
    fail "the daemon should reach the uplink masqueraded to 192.0.2.10, got '$out'"
ok "the daemon in netns ioa reaches the uplink, masqueraded"
