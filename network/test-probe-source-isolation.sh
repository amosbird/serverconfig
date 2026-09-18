#!/usr/bin/env bash
# A diagnostic probe must never emit a source address that does not belong to the interface it
# forces traffic onto. Violating that looks like a spoofed packet to upstream security gear, and on
# 2026-09-15 it made an incident capture cause the very outage it was recording.
#
# The check is only meaningful if it can detect the defect, so it runs a deliberately wrong
# implementation first and requires that one to leak.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)

# unshare -r maps only the caller's uid, so inside the namespace every path owned by amos belongs
# to an unmapped owner and namespace root gets no CAP_DAC_OVERRIDE over it. It can neither traverse
# a 0710 home directory nor write into a 0700 temporary directory. So: stage a world-readable copy
# of the probe outside $HOME, feed the body in on stdin rather than by path, and let the namespace
# create its own scratch directory.
STAGE=$(mktemp -d)
trap 'rm -rf "$STAGE"' EXIT
chmod 0755 "$STAGE"
install -m 0755 "$ROOT/scripts/network-probe-tcp" "$STAGE/network-probe-tcp"

sudo -n unshare -rn env \
    HOST_NETNS_LINK="$(readlink /proc/self/ns/net)" \
    HOST_NETNS_INODE="$(stat -Lc %i /proc/self/ns/net)" \
    STAGE="$STAGE" bash -s <<'INNER'
set -euo pipefail

current_link=$(readlink /proc/self/ns/net)
current_inode=$(stat -Lc %i /proc/self/ns/net)
if [ "$current_link" = "$HOST_NETNS_LINK" ] || [ "$current_inode" = "$HOST_NETNS_INODE" ]; then
    echo "REFUSE verified host network namespace" >&2
    exit 1
fi

PROBE="$STAGE/network-probe-tcp"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

PHYSICAL=10.36.55.23
TAILNET=100.88.203.53
GATEWAY=10.36.48.1
TARGET=216.239.32.117
MARK=0x80000

# phys0 stands in for wlan0 and net0 for tailscale0. Permanent neighbours make the dummy devices
# actually transmit, so tcpdump observes the source address the kernel chose.
ip link add phys0 type dummy
ip link set phys0 up
ip addr add "$PHYSICAL/20" dev phys0
ip neigh add "$GATEWAY" lladdr 02:00:00:00:00:01 dev phys0 nud permanent
ip route add default via "$GATEWAY" dev phys0

ip link add net0 type dummy
ip link set net0 up
ip addr add "$TAILNET/32" dev net0
ip neigh add "$TARGET" lladdr 02:00:00:00:00:02 dev net0 nud permanent
ip route add default dev net0 table 52

# Reproduce the deployed rule order: a marked socket escapes to main, everything else is captured
# by the exit-node table and therefore sources from the tailnet address.
ip rule add pref 500 fwmark "$MARK/0xff0000" lookup main
ip rule add pref 5270 lookup 52

capture() {
    local label=$1
    shift
    tcpdump -p -i phys0 -n -s 64 -w "$WORK/$label.pcap" "tcp and dst host $TARGET" \
        >/dev/null 2>&1 &
    local recorder=$!
    sleep 0.5
    "$@" >"$WORK/$label.out" 2>&1 || true
    sleep 0.5
    kill "$recorder" 2>/dev/null || true
    wait "$recorder" 2>/dev/null || true
}

sources() {
    tcpdump -n -r "$WORK/$1.pcap" 2>/dev/null |
        sed -n 's/.* IP \([0-9.]*\)\.[0-9]* > .*/\1/p' | sort -u
}

# Known-defective implementation: connect first, then apply the mark. The kernel drops the cached
# route and reroutes retransmissions onto phys0 while the socket keeps the tailnet source.
cat >"$WORK/leaky-probe" <<PY
import socket, time
connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
connection.setblocking(False)
connection.connect_ex(("$TARGET", 80))
connection.setsockopt(socket.SOL_SOCKET, socket.SO_MARK, $MARK)
time.sleep(4)
PY

capture leaky python3 "$WORK/leaky-probe"
leaked=$(sources leaky)
if ! grep -qx "$TAILNET" <<<"$leaked"; then
    printf 'INCONCLUSIVE control did not reproduce the leak; sources on phys0: %s\n' \
        "${leaked:-none}" >&2
    exit 1
fi
echo "OK   control: marking a connected socket leaks $TAILNET onto phys0"

capture fixed "$PROBE" --target "$TARGET" --port 80 --source "$PHYSICAL" \
    --interface phys0 --mark "$MARK" --timeout 2
observed=$(sources fixed)
if [ "$observed" != "$PHYSICAL" ]; then
    printf 'FAIL probe emitted unexpected sources on phys0: %s (%s)\n' \
        "${observed:-none}" "$(tr '\n' ' ' <"$WORK/fixed.out")" >&2
    exit 1
fi
echo "OK   network-probe-tcp emits only $PHYSICAL on the interface it forces"

# A source that is not assigned locally must be reported as an unplaceable probe, never as a path
# failure, so a mid-roam address change cannot be misread as an outage.
set +e
"$PROBE" --target "$TARGET" --port 80 --source 10.36.55.99 \
    --interface phys0 --mark "$MARK" --timeout 2 >/dev/null 2>&1
unplaceable_rc=$?
"$PROBE" --target "$TARGET" --port 80 --source "$PHYSICAL" \
    --interface phys0 --mark "$MARK" --timeout 2 >/dev/null 2>&1
path_rc=$?
set -e
[ "$unplaceable_rc" -eq 2 ] || {
    echo "FAIL unusable source must exit 2, got $unplaceable_rc" >&2
    exit 1
}
echo "OK   unusable source reports an unplaceable probe (2), not a path failure"
[ "$path_rc" -eq 1 ] || {
    echo "FAIL silently dropped path must exit 1, got $path_rc" >&2
    exit 1
}
echo "OK   silently dropped path reports a path failure (1)"
INNER
