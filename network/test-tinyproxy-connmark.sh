#!/usr/bin/env bash
# Test that owner-marked tinyproxy connections keep their IOA routing across socket close
# (FIN_WAIT_2 / TIME_WAIT / orphan RST) and never leak the ioa veth source address (198.18.170.1)
# onto a physical interface.
#
# Background: 2026-11-25 on XLSMART Public. tinyproxy (uid 186) opens connections to CN IPs that
# are not in ipset ioa, so `-m owner --uid-owner` is the only rule marking them 0x1 into table ioa
# (veth-ioa, masqueraded to 198.18.170.1). When tinyproxy closed a socket before the peer's FIN
# arrived, the kernel emitted the final ACK/RST from an orphan socket with no owning uid. That
# packet missed `-m owner`, matched cn_direct (mark 0x2) or main, left on wlan0 carrying the
# already-recorded veth-ioa SNAT source 198.18.170.1, and triggered a 180s WLC/FortiGate blackhole.
#
# This fixture proves:
#   1. Negative control (without connmark and without the 198.18.170.0/30 source guard): the
#      physical peer actually captures the leaked 198.18.170.1 RST on the wire.
#   2. With connmark (the fix in scripts/network-reconfigure): the final ACK stays on veth-ioa and
#      the physical peer captures zero packets.
#   3. With connmark mutated away (only the 198.18.170.0/30 source guard in place): the guard
#      catches and drops the leaked packet before it reaches the physical wire.
set -euo pipefail

if [ "${X_SANDBOX:-0}" != 1 ]; then
    exec sudo -n timeout -k 5 "${TEST_LIMIT:-30}" \
        unshare --net --mount --pid --fork --kill-child --mount-proc --propagation private \
        env X_SANDBOX=1 bash "$0" "$@"
fi

mkdir -p /run/netns
mount -t tmpfs tmpfs /run/netns

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

fail() { printf 'FAIL %s\n' "$*" >&2; exit 1; }
ok() { printf 'OK   %s\n' "$*"; }

# Resolve an unprivileged uid inside the sandbox (nobody = 65534).
OWNER_UID=$(id -u nobody)
[ -n "$OWNER_UID" ] || fail "cannot resolve nobody uid"

setup_topology() {
    ip link set lo up
    ip netns add lan
    ip netns add ioa

    # Physical NIC facing a simulated Cisco WLC / FortiGate gateway in `lan`.
    ip link add wlan0 type veth peer name phys1 netns lan
    ip addr add 10.36.54.190/20 dev wlan0
    ip link set wlan0 up
    ip route add default via 10.36.48.1 dev wlan0

    ip netns exec lan ip link set lo up
    ip netns exec lan ip addr add 10.36.48.1/20 dev phys1
    ip netns exec lan ip link set phys1 up

    # Prime ARP so the gateway is REACHABLE (an unresolved neighbour drops the leaked IP packet
    # behind an ARP request instead of putting it on the wire).
    ping -c 1 -W 1 10.36.48.1 >/dev/null

    # veth into the ioa namespace, matching network/ioa-netns.
    ip link add veth-ioa type veth peer name veth-ioa-ns netns ioa
    ip addr add 198.18.170.1/30 dev veth-ioa
    ip link set veth-ioa up
    ip route add default via 198.18.170.2 dev veth-ioa table 400

    ip netns exec ioa ip link set lo up
    ip netns exec ioa ip addr add 198.18.170.2/30 dev veth-ioa-ns
    ip netns exec ioa ip link set veth-ioa-ns up
    ip netns exec ioa ip addr add 163.177.30.56/32 dev lo
    ip netns exec ioa ip route add default via 198.18.170.1 dev veth-ioa-ns

    ip rule add pref 1150 fwmark 0x1 lookup 400
    ip rule add pref 1151 fwmark 0x1 prohibit
    ip rule add pref 1500 fwmark 0x2 lookup main
    ip rule add pref 1501 fwmark 0x2 prohibit

    nft -f - <<'NFT'
add table ip ioa_netns
add chain ip ioa_netns postrouting { type nat hook postrouting priority srcnat; policy accept; }
add rule ip ioa_netns postrouting oifname "veth-ioa" masquerade
NFT
}

install_policy() {
    local with_connmark=$1 with_guard=$2
    iptables -t mangle -F OUTPUT 2>/dev/null || true
    iptables -t mangle -F NETMODE_IOA 2>/dev/null || true
    iptables -t mangle -X NETMODE_IOA 2>/dev/null || true
    iptables -t mangle -N NETMODE_IOA
    iptables -t mangle -A OUTPUT -j NETMODE_IOA
    iptables -t mangle -A NETMODE_IOA -m mark ! --mark 0x0/0xffffffff -j RETURN
    if [ "$with_connmark" = 1 ]; then
        iptables -t mangle -A NETMODE_IOA \
            -m connmark --mark 0x1/0x1 -j MARK --set-xmark 0x1/0xffffffff
        iptables -t mangle -A NETMODE_IOA -m mark ! --mark 0x0/0xffffffff -j RETURN
    fi
    iptables -t mangle -A NETMODE_IOA \
        -d 100.64.0.0/10 -m owner --uid-owner "$OWNER_UID" -j RETURN
    iptables -t mangle -A NETMODE_IOA \
        -m owner --uid-owner "$OWNER_UID" -j MARK --set-xmark 0x1/0xffffffff
    if [ "$with_connmark" = 1 ]; then
        iptables -t mangle -A NETMODE_IOA \
            -m owner --uid-owner "$OWNER_UID" -j CONNMARK --set-xmark 0x1/0x1
    fi
    iptables -t mangle -A NETMODE_IOA -m mark ! --mark 0x0/0xffffffff -j RETURN
    iptables -t mangle -A NETMODE_IOA -d 163.177.30.56/32 -j MARK --set-xmark 0x2/0xffffffff

    nft delete table ip serverconfig_source_guard 2>/dev/null || true
    if [ "$with_guard" = 1 ]; then
        nft -f - <<'NFT'
add table ip serverconfig_source_guard
add set ip serverconfig_source_guard foreign_sources {
    type ipv4_addr
    flags interval
    elements = { 100.64.0.0/10, 198.18.170.0/30 }
}
add chain ip serverconfig_source_guard postrouting {
    type filter hook postrouting priority 110
    policy accept
}
add rule ip serverconfig_source_guard postrouting oifname "wlan*" ip saddr @foreign_sources counter drop
NFT
    fi
}

run_orphan_close() {
    local tag=$1 port=$2
    local lan_cap=$WORK/lan-$tag.out
    local ioa_cap=$WORK/ioa-$tag.out

    ip netns exec lan tcpdump -ni phys1 -l -nn "ip" >"$lan_cap" 2>&1 &
    local cap1=$!
    ip netns exec ioa tcpdump -ni veth-ioa-ns -l -nn "tcp and port $port" >"$ioa_cap" 2>&1 &
    local cap2=$!
    sleep 0.2

    ip netns exec ioa python3 -c "
import socket, time
s = socket.socket()
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(('163.177.30.56', $port))
s.listen(5)
c, _ = s.accept()
c.recv(1024)
# Wait for the client's FIN so the client process has closed and exited (FIN_WAIT_2 orphan).
assert c.recv(1024) == b''
time.sleep(0.2)
c.close()
time.sleep(0.4)
" &
    local srv=$!
    sleep 0.15

    su -s /bin/sh nobody -c "python3 -c \"
import socket
s = socket.create_connection(('163.177.30.56', $port), timeout=3)
s.sendall(b'hello')
s.close()
\""

    sleep 0.9
    kill "$cap1" "$cap2" 2>/dev/null || true
    wait "$cap1" "$cap2" "$srv" 2>/dev/null || true
}

setup_topology

# 1. Primary fix: connmark keeps the orphan final ACK on veth-ioa; zero packets reach wlan0 and
#    the peer sees the clean final ACK instead of repeated retransmitted FINs. Run before the
#    negative control so no retransmitted FIN from an un-acked negative-control socket is still in
#    flight during this phase.
install_policy 1 1
run_orphan_close fix 4432
! grep -q '198\.18\.170\.1' "$WORK/lan-fix.out" ||
    fail "with connmark, 198.18.170.1 still reached wlan0: $(cat "$WORK/lan-fix.out")"
grep -q '198\.18\.170\.1\..*> 163\.177\.30\.56\.4432: Flags \[\.\], ack 2' "$WORK/ioa-fix.out" ||
    fail "with connmark, final ACK did not reach veth-ioa-ns: $(cat "$WORK/ioa-fix.out")"
guard_drops=$(nft list chain ip serverconfig_source_guard postrouting |
    awk '/ip saddr @foreign_sources/ { for (i=1; i<=NF; i++) if ($i == "packets") print $(i+1) }')
[ "${guard_drops:-0}" -eq 0 ] ||
    fail "with connmark, source guard should not even need to fire, got drops=$guard_drops"
ok "connmark keeps orphan final ACK on veth-ioa with zero packets on wlan0"

# 2. Negative control: without connmark and without the 198.18.170.0/30 guard, closing the
#    owner-marked socket MUST leak 198.18.170.1 onto the physical LAN.
install_policy 0 0
run_orphan_close neg 4431
grep -q '198\.18\.170\.1\..*> 163\.177\.30\.56\.4431: Flags \[R\]' "$WORK/lan-neg.out" ||
    fail "negative control did not reproduce 198.18.170.1 leak on wlan0: $(cat "$WORK/lan-neg.out")"
ok "negative control reproduces 198.18.170.1 RST leak onto wlan0 without connmark"

# 3. Defense-in-depth mutant: connmark removed, source guard present — the guard drops the leaked
#    198.18.170.1 packets before wlan0 transmits them onto the wire.
install_policy 0 1
run_orphan_close guard 4433
! grep -q '198\.18\.170\.1' "$WORK/lan-guard.out" ||
    fail "source guard failed to block 198.18.170.1 from reaching wlan0: $(cat "$WORK/lan-guard.out")"
guard_drops=$(nft list chain ip serverconfig_source_guard postrouting |
    awk '/ip saddr @foreign_sources/ { for (i=1; i<=NF; i++) if ($i == "packets") print $(i+1) }')
[ "${guard_drops:-0}" -gt 0 ] ||
    fail "source guard did not record any drops for leaked 198.18.170.1 packets"
ok "post-srcnat source guard blocks 198.18.170.0/30 leak as backstop (dropped $guard_drops packets)"
