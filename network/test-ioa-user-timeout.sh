#!/usr/bin/env bash
# Prove the ngnclient sock_ops program bounds a stalled iOA connection, and only that.
#
# The real object is compiled (with a short timeout so the test is quick) and attached the way
# production attaches it: systemd's BPFProgram= on a unit. Clients run in that unit's cgroup inside a
# private network namespace wired by veth to a server namespace. Nothing touches the host network.
#
# Checks:
#   1. a non-loopback IPv4, IPv6 and v4-mapped connect gets TCP_USER_TIMEOUT;
#   2. a loopback connect (127/8, ::1, ::ffff:127/104) does not;
#   3. an established connection whose peer goes silent with data in flight fails with ETIMEDOUT
#      within the bound;
#   4. negative control: the same client in a unit *without* the program is still waiting well past
#      the bound, so check 3 cannot pass for some unrelated reason;
#   5. deployment: the pin path, the loader unit and the ngnclient drop-in agree, and the drop-in only
#      wants the loader, so a failed load can never take the tunnel down.
#   6. mutation: each deliberate break of the program is caught by checks 1-4.
# Usage: test-ioa-user-timeout.sh [bpf-source]   (the argument exists for mutation testing)
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SRC=$(realpath "${1:-$ROOT/network/ioa-sockops.bpf.c}")
LOADER="$ROOT/network/systemd/ioa-sockops.service"
DROPIN="$ROOT/systemd/ngnclient.service.d/user-timeout.conf"
TIMEOUT_MS=2000

[ "$(id -u)" -eq 0 ] || exec sudo -n bash "$0" "$SRC"

fail() { echo "FAIL $*" >&2; exit 1; }
ok() { echo "OK   $*"; }

# --- 5. static deployment contract ---------------------------------------------------------------
pin=$(sed -n 's/^BPFProgram=sock_ops://p' "$DROPIN")
[ "$pin" = /sys/fs/bpf/ioa-user-timeout ] || fail "drop-in attaches '$pin'"
grep -Fq " $pin type sockops" "$LOADER" || fail 'loader does not pin the path the drop-in attaches'
grep -Eq '^Wants=ioa-sockops.service$' "$DROPIN" || fail 'drop-in does not pull in the loader'
grep -Eq '^After=ioa-sockops.service$' "$DROPIN" || fail 'drop-in is not ordered after the loader'
grep -Eq '^(Requires|BindsTo|Requisite)=' "$DROPIN" && fail 'drop-in makes ngnclient depend hard on BPF'
grep -Eq '^Before=ngnclient.service$' "$LOADER" || fail 'loader is not ordered before ngnclient'
grep -Fq 'IOA_USER_TIMEOUT_MS 30000' "$ROOT/network/ioa-sockops.bpf.c" || fail 'production timeout is not 30s'
ok 'deployment: pin path, ordering and soft dependency agree'

# --- runtime ---------------------------------------------------------------------------------------

TAG=ioa-uto-$$
WORK=$(mktemp -d /run/$TAG.XXXX)
PIN=/sys/fs/bpf/$TAG
NS_C=$TAG-c
NS_S=$TAG-s
SERVER_PID=
LO_PID=
cleanup() {
    [ -z "$SERVER_PID" ] || kill "$SERVER_PID" 2>/dev/null || true
    [ -z "$LO_PID" ] || kill "$LO_PID" 2>/dev/null || true
    ip netns del "$NS_C" 2>/dev/null || true
    ip netns del "$NS_S" 2>/dev/null || true
    rm -f "$PIN"
    rm -rf "$WORK"
}
trap cleanup EXIT

clang -O2 -target bpf -Wall -Werror -DIOA_USER_TIMEOUT_MS=$TIMEOUT_MS -c "$SRC" -o "$WORK/prog.o"
bpftool prog load "$WORK/prog.o" "$PIN" type sockops

ip netns add "$NS_C"
ip netns add "$NS_S"
ip link add c0 netns "$NS_C" type veth peer name s0 netns "$NS_S"
for ns in "$NS_C" "$NS_S"; do ip -n "$ns" link set lo up; done
ip -n "$NS_C" addr add 192.0.2.1/24 dev c0
ip -n "$NS_S" addr add 192.0.2.2/24 dev s0
ip -n "$NS_C" addr add 2001:db8::1/64 dev c0 nodad
ip -n "$NS_S" addr add 2001:db8::2/64 dev s0 nodad
ip -n "$NS_C" link set c0 up
ip -n "$NS_S" link set s0 up
# Loopback listeners live in the client namespace.
ip -n "$NS_C" addr add 127.0.0.2/8 dev lo 2>/dev/null || true

cat >"$WORK/server.py" <<'EOF'
import socket, sys, threading
def serve(fam, addr):
    s = socket.socket(fam, socket.SOCK_STREAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    if fam == socket.AF_INET6:
        s.setsockopt(socket.IPPROTO_IPV6, socket.IPV6_V6ONLY, 0)
    s.bind((addr, 7000)); s.listen(16)
    while True:
        c, _ = s.accept()
        threading.Thread(target=lambda c=c: [c.sendall(d) for d in iter(lambda: c.recv(65536), b'')],
                         daemon=True).start()
for fam, addr in ((socket.AF_INET6, '::'),):
    threading.Thread(target=serve, args=(fam, addr), daemon=True).start()
print('ready', flush=True)
threading.Event().wait()
EOF
ip netns exec "$NS_S" python3 "$WORK/server.py" >"$WORK/server.out" 2>&1 &
SERVER_PID=$!
ip netns exec "$NS_C" python3 "$WORK/server.py" >"$WORK/lo.out" 2>&1 &
LO_PID=$!
for _ in $(seq 50); do
    grep -q ready "$WORK/server.out" 2>/dev/null && grep -q ready "$WORK/lo.out" 2>/dev/null && break
    sleep 0.1
done

# Prints "<target> <TCP_USER_TIMEOUT>" for each target.
cat >"$WORK/probe.py" <<'EOF'
import socket, sys
for t in sys.argv[1:]:
    fam = socket.AF_INET6 if ':' in t else socket.AF_INET
    s = socket.socket(fam, socket.SOCK_STREAM)
    s.settimeout(5)
    s.connect((t, 7000))
    print(t, s.getsockopt(socket.IPPROTO_TCP, socket.TCP_USER_TIMEOUT), flush=True)
    s.close()
EOF

# Connects, proves the path works, waits for the blackhole, then writes and measures the failure.
cat >"$WORK/stall.py" <<'EOF'
import errno, os, socket, sys, time
work, limit = sys.argv[1], float(sys.argv[2])
s = socket.create_connection(('192.0.2.2', 7000), timeout=5)
s.sendall(b'x'); assert s.recv(1) == b'x'
open(f'{work}/established', 'w').close()
while not os.path.exists(f'{work}/blackholed'):
    time.sleep(0.05)
s.settimeout(limit)
start = time.monotonic()
s.sendall(b'y' * 1024)
try:
    s.recv(1)
    print('unexpected-data')
except OSError as e:
    # socket.timeout is TimeoutError, which a kernel ETIMEDOUT also raises; only errno tells them
    # apart. Python's own deadline carries no errno.
    name = errno.errorcode.get(e.errno, 'alive') if e.errno else 'alive'
    print(f'{name} {time.monotonic() - start:.1f}')
EOF

in_unit() {  # in_unit <attach:yes|no> <cmd...>
    local attach=$1; shift
    local props=(-p "NetworkNamespacePath=/run/netns/$NS_C")
    [ "$attach" = no ] || props+=(-p "BPFProgram=sock_ops:$PIN")
    systemd-run --quiet --wait --pipe --collect "${props[@]}" "$@"
}

# --- 1/2. which connects get the option ------------------------------------------------------------
out=$(in_unit yes python3 "$WORK/probe.py" \
    192.0.2.2 2001:db8::2 ::ffff:192.0.2.2 127.0.0.1 127.0.0.2 ::1 ::ffff:127.0.0.1)
declare -A want=(
    [192.0.2.2]=$TIMEOUT_MS [2001:db8::2]=$TIMEOUT_MS [::ffff:192.0.2.2]=$TIMEOUT_MS
    [127.0.0.1]=0 [127.0.0.2]=0 [::1]=0 [::ffff:127.0.0.1]=0
)
for t in "${!want[@]}"; do
    got=$(awk -v t="$t" '$1 == t { print $2 }' <<<"$out")
    [ "$got" = "${want[$t]}" ] || fail "connect to $t: TCP_USER_TIMEOUT=${got:-missing}, want ${want[$t]}"
done
ok 'non-loopback IPv4/IPv6/v4-mapped connects get the timeout; loopback connects do not'

out=$(in_unit no python3 "$WORK/probe.py" 192.0.2.2)
[ "$out" = "192.0.2.2 0" ] || fail "unit without the program still got a timeout: $out"
ok 'a unit without BPFProgram= is unaffected'

# The loader failing must degrade to the old behaviour, never to a daemon that cannot start.
out=$(systemd-run --quiet --wait --pipe --collect -p "NetworkNamespacePath=/run/netns/$NS_C" \
    -p "BPFProgram=sock_ops:$PIN-missing" python3 "$WORK/probe.py" 192.0.2.2) ||
    fail 'a unit whose pinned program is missing failed to start'
[ "$out" = "192.0.2.2 0" ] || fail "missing pin produced: $out"
ok 'a missing pin is skipped and the unit still runs'

# --- 3/4. a silent peer with data in flight --------------------------------------------------------
blackhole() { ip netns exec "$NS_S" nft -f - <<'EOF'
table inet ioa_uto_test {
    chain inbound {
        type filter hook input priority 0; policy drop;
    }
}
EOF
}
unblackhole() { ip netns exec "$NS_S" nft delete table inet ioa_uto_test 2>/dev/null || true; }

run_stall() {  # run_stall <attach> <limit-seconds>
    rm -f "$WORK/established" "$WORK/blackholed"
    in_unit "$1" python3 "$WORK/stall.py" "$WORK" "$2" >"$WORK/stall.out" 2>&1 &
    local pid=$!
    for _ in $(seq 100); do [ -e "$WORK/established" ] && break; sleep 0.05; done
    [ -e "$WORK/established" ] || fail "stall client never established ($(cat "$WORK/stall.out"))"
    blackhole
    touch "$WORK/blackholed"
    wait "$pid" || true
    unblackhole
    cat "$WORK/stall.out"
}

read -r verdict secs < <(run_stall yes 15)
[ "$verdict" = ETIMEDOUT ] || fail "stalled connection ended with '$verdict $secs', want ETIMEDOUT"
awk -v s="$secs" -v t=$TIMEOUT_MS 'BEGIN { exit !(s * 1000 >= t * 0.9 && s * 1000 <= t + 3000) }' ||
    fail "stalled connection aborted after ${secs}s, want about $((TIMEOUT_MS / 1000))s"
ok "stalled connection aborts with ETIMEDOUT after ${secs}s"

read -r verdict secs < <(run_stall no 8)
[ "$verdict" = alive ] ||
    fail "negative control ended with '$verdict $secs'; the stall check proves nothing"
ok "negative control: without the program the same stall is still pending after ${secs}s"

# --- 6. mutation checks ---------------------------------------------------------------------------
# Each mutant must be rejected by the assertions above, or they prove less than they claim. Runs only
# for the real source, never recursively for a mutant.
[ "$SRC" = "$(realpath "$ROOT/network/ioa-sockops.bpf.c")" ] || exit 0
mutate() {  # mutate <name> <sed-expression>
    sed "$2" "$SRC" >"$WORK/mutant-$1.c"
    cmp -s "$SRC" "$WORK/mutant-$1.c" && fail "mutant $1 did not change the source"
    if bash "$0" "$WORK/mutant-$1.c" >/dev/null 2>&1; then
        fail "mutant $1 survived"
    fi
}
mutate no-loopback 's/ \&\& !is_loopback(skops)/ \&\& (is_loopback(skops) || 1)/'
mutate wrong-op 's/BPF_SOCK_OPS_TCP_CONNECT_CB/BPF_SOCK_OPS_PASSIVE_ESTABLISHED_CB/'
mutate only-127001 's/(bpf_ntohl(skops->remote_ip4) >> 24) == 127/skops->remote_ip4 == bpf_htonl(0x7f000001)/'
mutate v6-loopback 's|return 1;  // ::1|return 0;  // ::1|'
mutate mapped-loopback 's|>> 24) == 127;  // ::ffff|>> 24) == 10;  // ::ffff|'
mutate zero-timeout "s/int timeout = IOA_USER_TIMEOUT_MS;/int timeout = 0;/"
ok 'mutation checks: every mutant of the program is rejected'
