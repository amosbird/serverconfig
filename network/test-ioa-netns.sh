#!/usr/bin/env bash
# Prove network/ioa-netns builds an iOA namespace that:
#   1. reaches the physical uplink through the host, masqueraded to the uplink address, and gets
#      the replies back even though the host's catch-all would send them into the exit node;
#   2. never leaves through the Tailscale exit node, even though the host's catch-all prefers it;
#   3. fails closed immediately (EHOSTUNREACH, not a timeout) when the physical route is gone;
#   4. is idempotent (`up` twice) and removed completely by `down`.
#
# Every run happens in a sandbox: new network, mount and PID namespaces, with /run/netns on a
# private tmpfs. The sandbox's own network namespace plays the host, so the real host's rules,
# tables and links are unreachable. When the sandbox's first process exits, the kernel kills every
# process in it, so no listener can outlive the test or hold its output open. Each sandbox also has
# a hard time limit.
#
# Usage: test-ioa-netns.sh [ioa-netns-script]   (the argument exists for mutation testing)
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT=$(realpath "${1:-$ROOT/network/ioa-netns}")
LIMIT=${TEST_LIMIT:-60}

fail() { echo "FAIL $*" >&2; exit 1; }
ok() { echo "OK   $*"; }

sandbox() {
    sudo -n timeout -k 5 "$LIMIT" \
        unshare --net --mount --pid --fork --kill-child --mount-proc --propagation private \
        env IOA_NETNS_SANDBOX=1 bash "$0" "$1"
}

if [ "${IOA_NETNS_SANDBOX:-}" != 1 ]; then
    sandbox "$SCRIPT"
    [ -z "${1:-}" ] || exit 0
    mutate() {
        local work rc=0
        work=$(mktemp)
        sed "$2" "$SCRIPT" >"$work"
        if cmp -s "$SCRIPT" "$work"; then rm -f "$work"; fail "mutant $1 did not change the script"; fi
        sandbox "$work" >/dev/null 2>&1 || rc=$?
        rm -f "$work"
        case $rc in
            0) fail "mutant $1 survived" ;;
            124 | 137) fail "mutant $1 hit the ${LIMIT}s limit instead of failing an assertion" ;;
        esac
    }
    mutate no-main-rule 's/reconcile_rule "\$RULE_PREF" "from \$NET lookup main"/:/'
    mutate no-stop-rule 's/reconcile_rule "\$RULE_STOP_PREF" "from \$NET prohibit"/:/'
    mutate no-masquerade 's/ masquerade$/ accept/'
    mutate no-forwarding 's/net.ipv4.conf.\$HOST_IF.forwarding=1/net.ipv4.conf.$HOST_IF.forwarding=0/'
    mutate no-return-rule 's/reconcile_rule "\$RULE_RETURN_PREF" "to \$NET lookup main"/:/'
    mutate down-keeps-rules 's/while "\$IP" -4 rule del pref "\$pref"/while false/'
    mutate down-keeps-nat 's/"\$NFT" delete table ip "\$NFT_TABLE"/true/'
    ok "mutation checks: every mutant fails an assertion"
    exit 0
fi

# ---- inside the sandbox -------------------------------------------------------------------------
# The real host's /run/netns is a shared mount; without a private tmpfs on top, namespaces created
# here would appear in the host's `ip netns list`.
mkdir -p /run/netns
mount -t tmpfs tmpfs /run/netns
ip link set lo up

# The sandbox's own namespace is the host: an uplink to `up` (the internet) and a fake tailscale0
# to `ts` (the exit node). Both answer on 198.51.100.7:7000 and say which one they are.
ip netns add up
ip netns add ts
ip link add wl0 type veth peer name up0 netns up
ip link add tailscale0 type veth peer name ts0 netns ts
ip addr add 192.0.2.10/24 dev wl0
ip link set wl0 up
ip addr add 100.88.0.1/32 dev tailscale0
ip link set tailscale0 up
ip -n up link set lo up
ip -n up addr add 192.0.2.1/24 dev up0
ip -n up addr add 198.51.100.7/32 dev lo
ip -n up link set up0 up
ip -n up route add default via 192.0.2.10 dev up0
ip -n ts link set lo up
ip -n ts addr add 198.51.100.7/32 dev lo
ip -n ts link set ts0 up
ip -n ts route add default dev ts0
ip route add default via 192.0.2.1 dev wl0
# Tailscale's shape: table 52 carries everything and the catch-all rule prefers it to main.
ip route add default dev tailscale0 table 52
ip rule add pref 5270 lookup 52

listen() {
    ip netns exec "$1" python3 -c '
import socket, sys
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(("198.51.100.7", 7000)); s.listen(8)
while True:
    c, a = s.accept(); c.sendall((sys.argv[1] + " " + a[0] + "\n").encode()); c.close()
' "$1" </dev/null >/dev/null 2>&1 &
}
listen up
listen ts
for ns in up ts; do
    for _ in $(seq 50); do
        ip netns exec "$ns" ss -Hltn 'sport = :7000' | grep -q . && break
        sleep 0.1
    done
    ip netns exec "$ns" ss -Hltn 'sport = :7000' | grep -q . || fail "fixture: $ns listener never came up"
done

# Prints "<side> <peer>" on success, "ERR <errno name>" on failure, "TIMEOUT" if nothing answers.
connect_from() {
    "$@" timeout 4 python3 -c '
import errno, socket
try:
    s = socket.create_connection(("198.51.100.7", 7000), timeout=2)
    print(s.recv(64).decode().strip())
except socket.timeout:
    print("TIMEOUT")
except OSError as e:
    print("ERR", errno.errorcode.get(e.errno, e.errno))
' || echo TIMEOUT
}
from_host() { connect_from env; }
from_ioa() { connect_from ip netns exec ioa; }
netns_script() {
    IOA_NETNS=ioa IOA_NETNS_HOST_IF=vio IOA_NETNS_NS_IF=vio-ns bash "$SCRIPT" "$@" >/dev/null
}

out=$(from_host)
[ "$out" = "ts 100.88.0.1" ] || fail "fixture: the host's own traffic should take the exit node, got '$out'"

netns_script up
out=$(from_ioa)
[ "$out" = "up 192.0.2.10" ] ||
    fail "namespace traffic should leave through the uplink masqueraded to 192.0.2.10, got '$out'"
ok "namespace traffic leaves through the uplink, masqueraded, while the host prefers the exit node"

netns_script up
[ "$(ip -4 rule show pref 900 | wc -l)" -eq 2 ] && [ "$(ip -4 rule show pref 901 | wc -l)" -eq 1 ] ||
    fail "up twice left duplicate rules"
[ "$(from_ioa)" = "up 192.0.2.10" ] || fail "a second up broke the path"
ok "up is idempotent"

ip route del default via 192.0.2.1 dev wl0
out=$(from_ioa)
[ "$out" = "ERR EHOSTUNREACH" ] ||
    fail "with no physical route the namespace must be refused at once (EHOSTUNREACH), got '$out'"
ok "with no physical route the namespace fails closed at once"
ip route add default via 192.0.2.1 dev wl0
[ "$(from_ioa)" = "up 192.0.2.10" ] || fail "the path did not return with the physical route"
ok "the path returns with the physical route without re-running up"

netns_script down
left=$(
    ip netns list | awk '$1 == "ioa"'
    ip -o link show vio 2>/dev/null || true
    ip -4 rule show pref 900
    ip -4 rule show pref 901
    nft list tables | grep -w ioa_netns || true
)
[ -z "$left" ] || fail "down left state behind: $left"
netns_script down || fail "a second down failed"
ok "down removes everything and is idempotent"
