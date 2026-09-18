#!/usr/bin/env bash
# Reproduce the 2026-09-18 CN source leak in an isolated network namespace.
#
# A marked CN socket initially gets tailscale0's source from the first route lookup, then OUTPUT
# reroutes it to the physical main table and MASQUERADE fixes the source. DHCP replacement removes
# the physical address and purges that NAT state. Without a stop rule, the next packet falls through
# an empty main table into table 52 and creates the no-NAT state that the real capture later leaked
# onto wlan0. The negative control must reproduce that wrong fallback or this test proves nothing.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT="$ROOT/scripts/network-reconfigure"

grep -Eq '^P_CN=1500$' "$SCRIPT"
grep -Eq '^P_CN_STOP=1501$' "$SCRIPT"
grep -Fq 'DESIRED_BANDS[$P_CN_STOP]="from all fwmark $CN_MARK prohibit"' "$SCRIPT"

sudo -n unshare -rn env \
    HOST_NETNS_LINK="$(readlink /proc/self/ns/net)" \
    HOST_NETNS_INODE="$(stat -Lc %i /proc/self/ns/net)" bash -s <<'INNER'
set -euo pipefail

current_link=$(readlink /proc/self/ns/net)
current_inode=$(stat -Lc %i /proc/self/ns/net)
if [ "$current_link" = "$HOST_NETNS_LINK" ] || [ "$current_inode" = "$HOST_NETNS_INODE" ]; then
    echo "REFUSE verified host network namespace" >&2
    exit 1
fi

WORK=$(mktemp -d)
REMOTE_PID=
SERVER_PID=
trap '[ -z "$SERVER_PID" ] || kill "$SERVER_PID" 2>/dev/null || true;
    [ -z "$REMOTE_PID" ] || kill "$REMOTE_PID" 2>/dev/null || true; rm -rf "$WORK"' EXIT

PHYSICAL=192.0.2.2
GATEWAY=192.0.2.1
TAILNET=100.88.203.53
TARGET=198.51.100.10

ip link add phys0 type veth peer name peer0
ip addr add "$PHYSICAL/24" dev phys0
ip link set phys0 up

# A real TCP peer is required. The incident was an ESTABLISHED flow whose MASQUERADE state vanished
# when DHCP removed the address; a dummy destination cannot reproduce TCP's FIN retransmission after
# the route returns.
unshare -n sleep 300 &
REMOTE_PID=$!
for _ in {1..100}; do
    [ "$(readlink "/proc/$REMOTE_PID/ns/net")" != "$(readlink /proc/self/ns/net)" ] && break
    sleep 0.01
done
ip link set peer0 netns "$REMOTE_PID"
nsenter -t "$REMOTE_PID" -n ip link set lo up
nsenter -t "$REMOTE_PID" -n ip addr add "$GATEWAY/24" dev peer0
nsenter -t "$REMOTE_PID" -n ip link set peer0 up
nsenter -t "$REMOTE_PID" -n ip addr add "$TARGET/32" dev lo
nsenter -t "$REMOTE_PID" -n python3 - "$TARGET" <<'PY' &
import socket
import sys
import threading

address = sys.argv[1]
def serve(port):
    listener = socket.socket()
    listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    listener.bind((address, port))
    listener.listen()
    connection, _ = listener.accept()
    while connection.recv(4096):
        pass

threads = [threading.Thread(target=serve, args=(port,)) for port in (43001, 43002, 43003)]
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
PY
SERVER_PID=$!
sleep 0.3

ip link add tailscale0 type dummy
ip addr add "$TAILNET/32" dev tailscale0
ip link set tailscale0 up
ip route add default dev tailscale0 table 52

ipset create cn_direct hash:ip
ipset add cn_direct "$TARGET"
iptables -t mangle -A OUTPUT -m set --match-set cn_direct dst \
    -j MARK --set-xmark 0x2/0xffffffff
iptables -t nat -A POSTROUTING -o phys0 -m mark --mark 0x2/0xffffffff -j MASQUERADE
ip rule add pref 1500 fwmark 0x2/0xffffffff lookup main
ip rule add pref 5270 lookup 52

restore_main() {
    ip addr replace "$PHYSICAL/24" dev phys0
    ip route replace default via "$GATEWAY" dev phys0
}

remove_main() {
    ip route del default 2>/dev/null || true
    ip addr flush dev phys0
}

capture_start() {
    local label=$1
    nsenter -t "$REMOTE_PID" -n tcpdump -p -i peer0 -n -s 96 \
        -w "$WORK/$label-physical.pcap" \
        "tcp and dst host $TARGET" >/dev/null 2>&1 &
    PHYS_RECORDER=$!
    tcpdump -p -i tailscale0 -n -s 96 -w "$WORK/$label-tail.pcap" \
        "tcp and dst host $TARGET" >/dev/null 2>&1 &
    TAIL_RECORDER=$!
    sleep 0.4
}

capture_stop() {
    sleep 0.4
    kill "$PHYS_RECORDER" "$TAIL_RECORDER" 2>/dev/null || true
    wait "$PHYS_RECORDER" "$TAIL_RECORDER" 2>/dev/null || true
}

sources() {
    tcpdump -n -r "$1" 2>/dev/null |
        sed -n 's/.* IP \([0-9.]*\)\.[0-9]* > .*/\1/p' | sort -u
}

exercise() {
    local mode=$1
    MODE="$mode" PHYSICAL="$PHYSICAL" GATEWAY="$GATEWAY" TARGET="$TARGET" python3 - <<'PY'
import os
import socket
import subprocess
import time

port = 43001 if os.environ["MODE"] == "leaky" else 43002
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect((os.environ["TARGET"], port))
sock.send(b"before DHCP replacement")
time.sleep(0.1)
subprocess.run("ip route del default 2>/dev/null || true; ip addr flush dev phys0",
               shell=True, check=True)
# MASQUERADE state is tied to the disappearing physical address. The production teardown purged it;
# delete this fixture's entry explicitly because a veth kept administratively UP does not emit the
# same notifier sequence as networkd's real DHCP address replacement.
subprocess.run(f"conntrack -D -p tcp --dport {port} >/dev/null 2>&1 || true",
               shell=True, check=True)
try:
    sock.shutdown(socket.SHUT_WR)
except OSError as error:
    if os.environ["MODE"] == "fixed" and error.errno != 13:
        raise
time.sleep(0.3)
subprocess.run(
    f"ip addr replace {os.environ['PHYSICAL']}/24 dev phys0; "
    f"ip route replace default via {os.environ['GATEWAY']} dev phys0",
    shell=True, check=True)
# Keep the fd alive for FIN retransmission after the route returns.
time.sleep(3)
PY
}

# Negative control: without pref 1501 the FIN must enter table 52 with the tailnet source. The real
# capture proves the second leg — its retransmission later left wlan0 with the same source — while
# whether this fixture's unacknowledged FIN is reclassified before NAT retries depends on TCP timing.
restore_main
capture_start leaky
exercise leaky
capture_stop
leaky_physical=$(sources "$WORK/leaky-physical.pcap")
leaky_tail=$(sources "$WORK/leaky-tail.pcap")
if ! grep -qx "$PHYSICAL" <<<"$leaky_physical" || ! grep -qx "$TAILNET" <<<"$leaky_tail"; then
    printf 'INCONCLUSIVE control did not reproduce the leak (physical=%s tail=%s)\n' \
        "${leaky_physical//$'\n'/,}" "${leaky_tail//$'\n'/,}" >&2
    exit 1
fi
echo "OK   control leaks $TAILNET into table 52 while main is absent"

# Fixed policy: main lookup and its stop rule are one band. No packet may reach table 52 while main
# is empty; after main returns the same socket must create fresh source NAT and use the physical IP.
conntrack -F >/dev/null 2>&1 || true
restore_main
ip rule add pref 1501 fwmark 0x2/0xffffffff prohibit
nft -f - <<'NFT'
add table ip source_guard
add chain ip source_guard postrouting { type filter hook postrouting priority 110; policy accept; }
add rule ip source_guard postrouting oifname "phys0" \
    ip saddr 100.64.0.0/10 counter drop
NFT
capture_start fixed
exercise fixed
capture_stop
fixed_physical=$(sources "$WORK/fixed-physical.pcap")
fixed_tail=$(sources "$WORK/fixed-tail.pcap")
if ! grep -qx "$PHYSICAL" <<<"$fixed_physical"; then
    printf 'FAIL fixed path never emitted the healthy physical source: %s\n' \
        "${fixed_physical//$'\n'/,}" >&2
    exit 1
fi
echo "OK   restored main path emits the healthy physical source $PHYSICAL"
if [ -n "$fixed_tail" ]; then
    printf 'FAIL marked CN traffic fell through to tailscale0: %s\n' \
        "${fixed_tail//$'\n'/,}" >&2
    exit 1
fi
echo "OK   pref 1501 keeps marked CN traffic out of table 52 while main is absent"

guarded=$(nft list chain ip source_guard postrouting |
    awk '/100\.64\.0\.0\/10/ {
        for (i = 1; i <= NF; i++) if ($i == "packets") {print $(i + 1); exit}
    }')
if [ "${guarded:-0}" -lt 1 ]; then
    echo "FAIL post-srcnat guard did not catch the stale no-NAT retransmission" >&2
    exit 1
fi
echo "OK   post-srcnat guard caught $guarded stale retransmission(s) before physical egress"

# Existing poisoned TCP state is allowed to fail; applications reconnect. A new connection after
# recovery must take fresh MASQUERADE state and work with the physical source.
python3 - "$TARGET" <<'PY'
import socket
import sys
import time
sock = socket.create_connection((sys.argv[1], 43003), timeout=2)
sock.send(b"new connection after recovery")
time.sleep(0.2)
PY
if ! conntrack -L -p tcp --dport 43003 -o extended 2>/dev/null |
    grep -q "src=$TARGET dst=$PHYSICAL"; then
    echo "FAIL a new post-recovery connection has no physical MASQUERADE mapping" >&2
    exit 1
fi
echo "OK   new connections acquire physical MASQUERADE state after recovery"
INNER
