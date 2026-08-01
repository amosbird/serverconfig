#!/usr/bin/env bash
# Characterize nftables route-output policy routing without touching the live network stack.
set -euo pipefail

readonly PREFIX=nft-direct-routing-$$
readonly CLIENT=${PREFIX}-client
readonly PHYSICAL=${PREFIX}-physical
readonly EXIT=${PREFIX}-exit
readonly CN_IP=203.0.113.10
readonly OTHER_IP=198.51.100.10
WORK=$(mktemp -d)
readonly WORK
TCPDUMP_PIDS=()
SERVER_PIDS=()

fail() {
    printf 'FAIL %s\n' "$*" >&2
    exit 1
}

need() {
    command -v "$1" >/dev/null || fail "missing dependency: $1"
}

ns() {
    local namespace=$1
    shift
    case $namespace in
        "$CLIENT"|"$PHYSICAL"|"$EXIT") ;;
        *) fail "refusing command in unowned namespace: $namespace" ;;
    esac
    ip netns exec "$namespace" "$@"
}

cleanup() {
    local pid namespace
    for pid in "${TCPDUMP_PIDS[@]}" "${SERVER_PIDS[@]}"; do
        [ -n "$pid" ] && kill "$pid" 2>/dev/null || true
    done
    wait 2>/dev/null || true
    for namespace in "$CLIENT" "$PHYSICAL" "$EXIT"; do
        ip netns del "$namespace" 2>/dev/null || true
    done
    if [ "${KEEP_WORK:-0}" = 1 ]; then
        printf 'Artifacts retained at %s\n' "$WORK" >&2
    else
        rm -rf "$WORK"
    fi
}
trap cleanup EXIT INT TERM

assert_eq() {
    local description=$1 expected=$2 actual=$3
    if [ "$actual" != "$expected" ]; then
        fail "$description: expected '$expected', got '$actual'"
    fi
    printf 'OK   %s: %s\n' "$description" "$actual"
}

assert_contains() {
    local description=$1 haystack=$2 needle=$3
    [[ $haystack == *"$needle"* ]] || fail "$description: '$needle' absent from '$haystack'"
    printf 'OK   %s\n' "$description"
}

for dependency in ip nft python3 tcpdump ping conntrack awk sed grep pkill; do
    need "$dependency"
done
[ "${EUID:-$(id -u)}" -eq 0 ] || fail 'run with sudo/root; only temporary netns are changed'
for namespace in "$CLIENT" "$PHYSICAL" "$EXIT"; do
    ! ip netns list | awk '{print $1}' | grep -Fxq "$namespace" ||
        fail "namespace already exists: $namespace"
done

cat >"$WORK/server.py" <<'PY'
import selectors
import socket
import sys

identity, log_path = sys.argv[1:]
selector = selectors.DefaultSelector()
log = open(log_path, "a", buffering=1)

for kind in (socket.SOCK_STREAM, socket.SOCK_DGRAM):
    sock = socket.socket(socket.AF_INET, kind)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(("0.0.0.0", 4040))
    if kind == socket.SOCK_STREAM:
        sock.listen()
        selector.register(sock, selectors.EVENT_READ, "tcp-listener")
    else:
        selector.register(sock, selectors.EVENT_READ, "udp")

while True:
    for key, _ in selector.select():
        sock = key.fileobj
        if key.data == "tcp-listener":
            conn, address = sock.accept()
            payload = conn.recv(128)
            log.write(f"TCP {address[0]} {payload.decode(errors='replace')}\n")
            conn.sendall(identity.encode())
            conn.close()
        else:
            payload, address = sock.recvfrom(128)
            log.write(f"UDP {address[0]} {payload.decode(errors='replace')}\n")
            sock.sendto(identity.encode(), address)
PY

cat >"$WORK/client.py" <<'PY'
import socket
import sys

protocol, mode, destination, mark_text, payload = sys.argv[1:]
mark = int(mark_text, 0)
kind = socket.SOCK_STREAM if protocol == "tcp" else socket.SOCK_DGRAM
sock = socket.socket(socket.AF_INET, kind)
sock.settimeout(1)
if mark:
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_MARK, mark)
if protocol == "tcp":
    sock.connect((destination, 4040))
    sock.sendall(payload.encode())
    response = sock.recv(128)
elif mode == "connected":
    sock.connect((destination, 4040))
    sock.send(payload.encode())
    try:
        response = sock.recv(128)
    except TimeoutError:
        response = b"timeout"
else:
    sock.sendto(payload.encode(), (destination, 4040))
    try:
        response, _ = sock.recvfrom(128)
    except TimeoutError:
        response = b"timeout"
print(f"peer={response.decode()} local={sock.getsockname()[0]} mark={sock.getsockopt(socket.SOL_SOCKET, socket.SO_MARK):#x}")
PY

# The only host-level mutations are creation/removal of these three named namespaces. All links,
# addresses, routes, sysctls, nftables objects, and conntrack state are created inside them.
for namespace in "$CLIENT" "$PHYSICAL" "$EXIT"; do
    ip netns add "$namespace"
    ns "$namespace" ip link set lo up
done
ns "$CLIENT" ip link add physical0 type veth peer name client0 netns "$PHYSICAL"
ns "$CLIENT" ip link add exit0 type veth peer name client0 netns "$EXIT"

ns "$CLIENT" ip address add 192.0.2.2/24 dev physical0
ns "$CLIENT" ip address add 100.64.0.2/24 dev exit0
ns "$PHYSICAL" ip address add 192.0.2.1/24 dev client0
ns "$EXIT" ip address add 100.64.0.1/24 dev client0
for destination in "$CN_IP" "$OTHER_IP"; do
    # shortcut: duplicate loopback destinations make the selected peer directly observable without
    # a fourth router namespace; add routed server namespaces if hop behavior becomes relevant.
    ns "$PHYSICAL" ip address add "$destination/32" dev lo
    ns "$EXIT" ip address add "$destination/32" dev lo
done
ns "$CLIENT" ip link set physical0 up
ns "$CLIENT" ip link set exit0 mtu 1280 up
ns "$PHYSICAL" ip link set client0 up
ns "$EXIT" ip link set client0 mtu 1280 up

ns "$CLIENT" ip route add default via 192.0.2.1 dev physical0
ns "$CLIENT" ip route add default via 100.64.0.1 dev exit0 table 52
ns "$CLIENT" ip rule add priority 50 fwmark 0x2 lookup main
ns "$CLIENT" ip rule add priority 60 fwmark 0 lookup 52
ns "$PHYSICAL" ip route add 100.64.0.0/24 via 192.0.2.2
ns "$EXIT" ip route add 192.0.2.0/24 via 100.64.0.2

cat >"$WORK/route.nft" <<EOF
add table inet direct
add set inet direct cn_direct { type ipv4_addr; flags interval; }
add element inet direct cn_direct { $CN_IP }
add chain inet direct classify { type route hook output priority mangle; policy accept; }
add rule inet direct classify meta mark != 0 counter return
add rule inet direct classify ip daddr @cn_direct counter meta mark set 0x2
EOF
ns "$CLIENT" nft -f "$WORK/route.nft"

ns "$PHYSICAL" python3 "$WORK/server.py" physical "$WORK/physical.log" &
SERVER_PIDS+=("$!")
ns "$EXIT" python3 "$WORK/server.py" exit "$WORK/exit.log" &
SERVER_PIDS+=("$!")
sleep 0.2

start_captures() {
    : >"$WORK/physical.pcap.txt"
    : >"$WORK/exit.pcap.txt"
    ns "$CLIENT" tcpdump -l -nn -vvv -i physical0 "host $CN_IP or host $OTHER_IP" \
        >"$WORK/physical.pcap.txt" 2>"$WORK/physical.tcpdump.err" &
    TCPDUMP_PIDS+=("$!")
    ns "$CLIENT" tcpdump -l -nn -vvv -i exit0 "host $CN_IP or host $OTHER_IP" \
        >"$WORK/exit.pcap.txt" 2>"$WORK/exit.tcpdump.err" &
    TCPDUMP_PIDS+=("$!")
    sleep 0.2
}

stop_captures() {
    local pid
    ns "$CLIENT" pkill -INT tcpdump 2>/dev/null || true
    for pid in "${TCPDUMP_PIDS[@]}"; do
        kill -INT "$pid" 2>/dev/null || true
        wait "$pid" 2>/dev/null || true
    done
    TCPDUMP_PIDS=()
}

run_socket() {
    ns "$CLIENT" python3 "$WORK/client.py" "$@"
}

syn_mss() {
    local file=$1 destination=$2
    sed -n "/ > $destination\.4040: Flags \[S\]/s/.*mss \([0-9][0-9]*\).*/\1/p" "$file" | head -n1
}

packet_source() {
    local file=$1 destination=$2
    awk -v destination="$destination.4040" \
        '$0 ~ " > " destination ":" { source=$1; sub(/\.[0-9]+$/, "", source); print source; exit }' \
        "$file"
}

printf '\n== A: route-output without NAT ==\n'
start_captures
result=$(run_socket tcp connected "$CN_IP" 0 a-tcp)
assert_contains 'CN TCP uses physical peer' "$result" 'peer=physical'
assert_contains 'CN TCP socket retains initial table-52 source' "$result" 'local=100.64.0.2'
for mode in connected unconnected; do
    result=$(run_socket udp "$mode" "$CN_IP" 0 "a-udp-$mode")
    assert_contains "CN UDP $mode is sent through physical peer" \
        "$(cat "$WORK/physical.log" 2>/dev/null || true)" "UDP 100.64.0.2 a-udp-$mode"
    if [ "$mode" = connected ]; then
        assert_contains 'CN UDP connected socket retains table-52 source' "$result" 'local=100.64.0.2'
    else
        assert_contains 'unconnected UDP socket remains unnamed after sendto' "$result" 'local=0.0.0.0'
    fi
done
ns "$CLIENT" ping -q -c 1 -W 2 "$CN_IP" >/dev/null
result=$(run_socket tcp connected "$OTHER_IP" 0 a-other)
assert_contains 'ordinary destination uses exit peer' "$result" 'peer=exit'
for mark in 0x80000 0xa38; do
    result=$(run_socket udp connected "$CN_IP" "$mark" "owner-$mark")
    assert_contains "owner mark $mark is preserved" "$result" "mark=$mark"
    assert_contains "owner mark $mark remains on initial exit path" \
        "$(cat "$WORK/exit.log" 2>/dev/null || true)" "UDP 100.64.0.2 owner-$mark"
done
sleep 0.2
stop_captures
A_SOURCE=$(packet_source "$WORK/physical.pcap.txt" "$CN_IP")
A_MSS=$(syn_mss "$WORK/physical.pcap.txt" "$CN_IP")
assert_eq 'CN packet source without NAT' '100.64.0.2' "$A_SOURCE"
assert_eq 'CN SYN MSS keeps initial table-52 MTU' '1240' "$A_MSS"
assert_contains 'ICMP follows physical path' "$(cat "$WORK/physical.pcap.txt")" " > $CN_IP: ICMP echo request"
printf 'RESULT A_source=%s A_mss=%s A_conntrack=%s\n' "$A_SOURCE" "$A_MSS" \
    "$(ns "$CLIENT" conntrack -L 2>/dev/null | grep -c "dst=$CN_IP" || true)"

printf '\n== B: exact-mark postrouting masquerade ==\n'
cat >"$WORK/nat.nft" <<'EOF'
add chain inet direct direct_nat { type nat hook postrouting priority srcnat; policy accept; }
add rule inet direct direct_nat meta mark 0x2 counter masquerade
EOF
ns "$CLIENT" nft -f "$WORK/nat.nft"
ns "$CLIENT" conntrack -F >/dev/null 2>&1 || true
start_captures
result=$(run_socket tcp connected "$CN_IP" 0 b-tcp)
assert_contains 'NAT CN TCP uses physical peer' "$result" 'peer=physical'
assert_contains 'NAT keeps initial socket local address' "$result" 'local=100.64.0.2'
result=$(run_socket udp connected "$CN_IP" 0 b-udp)
assert_contains 'NAT CN UDP reaches physical server' \
    "$(cat "$WORK/physical.log" 2>/dev/null || true)" 'UDP 192.0.2.2 b-udp'
sleep 0.2
B_CONNTRACK=$(ns "$CLIENT" conntrack -L 2>/dev/null | grep "dst=$CN_IP" || true)
stop_captures
B_SOURCE=$(packet_source "$WORK/physical.pcap.txt" "$CN_IP")
B_MSS=$(syn_mss "$WORK/physical.pcap.txt" "$CN_IP")
assert_eq 'CN wire source with exact-mark masquerade' '192.0.2.2' "$B_SOURCE"
assert_eq 'CN SYN MSS remains initial-path MSS under NAT' '1240' "$B_MSS"
assert_contains 'conntrack records initial table-52 source' "$B_CONNTRACK" 'src=100.64.0.2'
assert_contains 'conntrack records masqueraded reply destination' "$B_CONNTRACK" 'dst=192.0.2.2'
printf 'RESULT B_source=%s B_mss=%s\n' "$B_SOURCE" "$B_MSS"
printf 'RESULT B_conntrack=%s\n' "$(tr '\n' ';' <<<"$B_CONNTRACK")"

printf '\n== C: atomic interval-set updates ==\n'
{
    echo 'flush set inet direct cn_direct'
    printf 'add element inet direct cn_direct { '
    for i in $(seq 0 1000); do
        printf '10.%d.%d.%d' "$((i / 65536))" "$(((i / 256) % 256))" "$((i % 256))"
        [ "$i" -eq 1000 ] || printf ', '
    done
    echo ' }'
} >"$WORK/set-update.nft"
ns "$CLIENT" nft -c -f "$WORK/set-update.nft"
ns "$CLIENT" conntrack -F >/dev/null 2>&1 || true
sleep 0.1
ns "$CLIENT" ip monitor route >"$WORK/route-monitor.log" 2>&1 &
monitor_pid=$!
sleep 0.1
ns "$CLIENT" nft -f "$WORK/set-update.nft"
sleep 0.2
kill "$monitor_pid" 2>/dev/null || true
wait "$monitor_pid" 2>/dev/null || true
SET_COUNT=$(ns "$CLIENT" nft -j list set inet direct cn_direct |
    python3 -c 'import json,sys; d=json.load(sys.stdin); print(sum(len(x["set"].get("elem", [])) for x in d["nftables"] if "set" in x))')
assert_eq 'transaction installs 1001 prefixes' '1001' "$SET_COUNT"
assert_eq 'set update emits no route events' '0' "$(wc -l <"$WORK/route-monitor.log")"
printf 'flush set inet direct cn_direct\nadd element inet direct cn_direct { broken }\n' \
    >"$WORK/bad-update.nft"
if ns "$CLIENT" nft -c -f "$WORK/bad-update.nft" >/dev/null 2>&1; then
    fail 'malformed set unexpectedly passed nft -c'
fi
if ns "$CLIENT" nft -f "$WORK/bad-update.nft" >/dev/null 2>&1; then
    fail 'malformed transaction unexpectedly applied'
fi
AFTER_BAD=$(ns "$CLIENT" nft -j list set inet direct cn_direct |
    python3 -c 'import json,sys; d=json.load(sys.stdin); print(sum(len(x["set"].get("elem", [])) for x in d["nftables"] if "set" in x))')
assert_eq 'malformed transaction leaves active set unchanged' '1001' "$AFTER_BAD"
printf 'RESULT C_elements=%s C_route_events=0 C_malformed_rollback=yes\n' "$SET_COUNT"

printf '\n== D: route hook versus filter hook ==\n'
cat >"$WORK/filter.nft" <<EOF
delete chain inet direct direct_nat
flush chain inet direct classify
delete chain inet direct classify
delete set inet direct cn_direct
add set inet direct cn_direct { type ipv4_addr; flags interval; }
add element inet direct cn_direct { $CN_IP }
add chain inet direct classify { type filter hook output priority mangle; policy accept; }
add rule inet direct classify meta mark != 0 return
add rule inet direct classify ip daddr @cn_direct meta mark set 0x2
EOF
ns "$CLIENT" nft -f "$WORK/filter.nft"
result=$(run_socket udp connected "$CN_IP" 0 d-filter)
assert_contains 'filter-output leaves packet on exit peer' \
    "$(cat "$WORK/exit.log" 2>/dev/null || true)" 'UDP 100.64.0.2 d-filter'
assert_contains 'filter-output socket source remains table-52 source' "$result" 'local=100.64.0.2'
printf 'RESULT D_filter_peer=exit D_route_peer=physical\n'

printf '\nAll nft direct-routing characterization checks passed.\n'
