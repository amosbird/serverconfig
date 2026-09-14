#!/usr/bin/env bash
# Static IOA 10/8 selection must never capture the current physical LAN.
#
# The priority-1000 pins that keep the LAN direct are a snapshot written by
# network-reconfigure, while the priority-2500 `to 10.0.0.0/8 lookup ioa` rule
# and table `ioa` outlive any single run. Roaming onto a different 10/8 subnet
# leaves the pins naming the previous LAN, so the window until the next run
# finishes is exactly when the new gateway can be swallowed into tun0.
#
# Both checks run against deliberately stale pins. The kernel-derived
# suppress_prefixlength rule must keep the live gateway and LAN direct, and the
# same topology without that rule must fail, so the check cannot pass for the
# wrong reason.

set -Eeuo pipefail

NS=ioa-lan-overlap-$$
STALE_LAN=10.36.99.0/20
LIVE_LAN=10.36.48.0/20
LIVE_ADDR=10.36.50.129/20
GATEWAY=10.36.48.1
LAN_PEER=10.36.60.7
SUPPRESS_RULE=(from all lookup main suppress_prefixlength 0)

pass=0
fail=0
ok() { printf 'ok   %s\n' "$*"; pass=$((pass + 1)); }
bad() { printf 'FAIL %s\n' "$*"; fail=$((fail + 1)); }

if [ "$(id -u)" -ne 0 ]; then
    echo "must run as root (needs network namespaces)" >&2
    exit 2
fi

cleanup() { ip netns del "$NS" 2>/dev/null || true; }
trap cleanup EXIT

nsx() { ip netns exec "$NS" "$@"; }

# phys0 stands in for wlan0 and tun0 for the SmartGateAgent tunnel. A dummy
# device carries a real connected route, which is what the suppressed lookup
# must find.
ip netns add "$NS"
nsx ip link add phys0 type dummy
nsx ip link add tun0 type dummy
nsx ip link set phys0 up
nsx ip link set tun0 up
nsx ip addr add "$LIVE_ADDR" dev phys0
nsx ip route add default via "$GATEWAY" dev phys0
nsx ip route add default dev tun0 table 400 metric 101

# Stale snapshot: the pins still name the LAN from before the roam.
nsx ip rule add pref 1000 to "$STALE_LAN" lookup main
nsx ip rule add pref 2500 to 10.0.0.0/8 lookup 400

route_dev() {
    nsx ip route get "$1" 2>/dev/null | awk '{for (i=1;i<NF;i++) if ($i=="dev") {print $(i+1); exit}}'
}

# Control: the hazard must be reproducible, or the check below proves nothing.
for dest in "$GATEWAY" "$LAN_PEER"; do
    dev=$(route_dev "$dest")
    if [ "$dev" = tun0 ]; then
        ok "control: stale pins send $dest into tun0"
    else
        bad "control: expected $dest via tun0 with stale pins, got ${dev:-none}"
    fi
done

# The fix: main minus its default route is the live on-link set.
nsx ip rule add pref 1000 "${SUPPRESS_RULE[@]}"

for dest in "$GATEWAY" "$LAN_PEER"; do
    dev=$(route_dev "$dest")
    if [ "$dev" = phys0 ]; then
        ok "$dest stays on phys0 despite stale pins"
    else
        bad "expected $dest via phys0, got ${dev:-none}"
    fi
done

# The suppressed lookup must not leak the default route, or every destination
# would bypass IOA selection and the exit node.
dev=$(route_dev 10.99.99.99)
if [ "$dev" = tun0 ]; then
    ok "unrelated 10/8 destination still selects IOA"
else
    bad "expected 10.99.99.99 via tun0, got ${dev:-none}"
fi

dev=$(route_dev 8.8.8.8)
if [ "$dev" = phys0 ]; then
    ok "public destination unaffected"
else
    bad "expected 8.8.8.8 via phys0, got ${dev:-none}"
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
