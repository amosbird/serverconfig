#!/usr/bin/env bash
# Behavioural fixture for the per-association lease reassertion.
#
# Two failure shapes were recorded on XLSMART and they need different repairs, which is why the stubs
# below take the repair that works as a parameter:
#
#   - A roam leaves the station associated but unforwarded. The lease is correct and the DHCP server
#     answers immediately, so DHCP cannot help; only rebuilding the association does. This is the
#     common case while walking, so it has to be tried first.
#   - A reassociation inside the carrier grace keeps a lease from another VLAN (the 2026-09-18 resume).
#     Reassociating does not replace the lease, so a full DHCP is still needed.
#
# The third property is structural: acting is recorded before it happens, because every repair changes
# link state and retriggers the hook.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT="$ROOT/scripts/network-dhcp-refresh"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

fail=0
check() {
    local description=$1 expected=$2 actual=$3
    if [ "$expected" = "$actual" ]; then
        printf 'OK   %s\n' "$description"
    else
        printf 'FAIL %s\n     expected: %s\n     got:      %s\n' \
            "$description" "$expected" "$actual" >&2
        fail=1
    fi
}

mkdir -p "$WORK/bin"
cat >"$WORK/bin/networkctl" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >>"$ACTION_LOG"
case "$1" in
    renew) [ -z "$RENEW_FAILS" ] || exit 1 ;;
    reconfigure)
        [ -z "$RECONFIGURE_FAILS" ] || exit 1
        [ "$FIXED_BY" != reconfigure ] || { printf 1 >"$ARP_ANSWERS"; printf 1 >"$FORWARDING"; }
        ;;
esac
SH
cat >"$WORK/bin/iwctl" <<'SH'
#!/bin/sh
printf 'iwctl %s\n' "$*" >>"$ACTION_LOG"
[ -z "$IWCTL_FAILS" ] || exit 1
[ "$3" != connect ] || [ "$FIXED_BY" != reassociate ] || { printf 1 >"$ARP_ANSWERS"; printf 1 >"$FORWARDING"; }
SH
cat >"$WORK/bin/iw" <<'SH'
#!/bin/sh
case "$*" in
    *"station dump"*)
        [ -r "$STATION_FILE" ] || exit 1
        cat "$STATION_FILE"
        ;;
    *link*) printf 'Connected to aa:bb:cc:dd:ee:01 (on wlan0)\n\tSSID: %s\n' "$LINK_SSID" ;;
esac
SH
cat >"$WORK/bin/ip" <<'SH'
#!/bin/sh
case "$*" in
    *"route show"*) printf 'default via 10.36.48.1 dev wlan0 proto dhcp src 10.36.55.44 metric 600\n' ;;
    *) printf '%s' "$ADDR_OUTPUT" ;;
esac
SH
cat >"$WORK/bin/arping" <<'SH'
#!/bin/sh
[ "$(cat "$ARP_ANSWERS")" = 1 ]
SH
# The probe is the only thing that can tell forwarding from a gateway that merely answers ARP.
cat >"$WORK/bin/probe" <<'SH'
#!/bin/sh
printf 'probe %s\n' "$*" >>"$PROBE_LOG"
[ -z "$PROBE_UNPLACEABLE" ] || exit 2
[ "$(cat "$FORWARDING")" = 1 ] || exit 1
SH
# Fixture associations must not reach the system journal, where they would look like real incidents.
printf '#!/bin/sh\nexit 0\n' >"$WORK/bin/logger"
chmod 755 "$WORK/bin"/*

ADDRESS='3: wlan0    inet 10.36.55.44/20 scope global wlan0'
RENEW='renew wlan0'
REASSOCIATE='iwctl station wlan0 disconnect,iwctl station wlan0 connect XLSMART Public'
RECONFIGURE='reconfigure wlan0'

associate() {
    printf 'Station %s (on wlan0)\n\tinactive time:\t0 ms\n\tconnected time:\t%s seconds\n' \
        "$1" "$2" >"$WORK/station"
}
disassociate() { rm -f "$WORK/station"; }
uptime_at() { printf '%s.42 %s.00\n' "$1" "$1" >"$WORK/uptime"; }
segment() { printf '%s' "$1" >"$WORK/arp"; printf '%s' "$2" >"$WORK/fwd"; }

run() {
    local rc=0
    IW="$WORK/bin/iw" IP="$WORK/bin/ip" NETWORKCTL="$WORK/bin/networkctl" \
        ARPING="$WORK/bin/arping" IWCTL="$WORK/bin/iwctl" STATION_FILE="$WORK/station" \
        ADDR_OUTPUT="${ADDR_OUTPUT-$ADDRESS}" ACTION_LOG="$WORK/actions" \
        ARP_ANSWERS="$WORK/arp" FORWARDING="$WORK/fwd" PROBE="$WORK/bin/probe" \
        PROBE_LOG="$WORK/probes" PROBE_UNPLACEABLE="${PROBE_UNPLACEABLE-}" \
        FIXED_BY="${FIXED_BY-none}" \
        LINK_SSID="${LINK_SSID-XLSMART Public}" \
        RENEW_FAILS="${RENEW_FAILS-}" RECONFIGURE_FAILS="${RECONFIGURE_FAILS-}" \
        IWCTL_FAILS="${IWCTL_FAILS-}" REASSOCIATE_COOLDOWN="${REASSOCIATE_COOLDOWN-20}" \
        RECONFIGURE_LOCK="${RECONFIGURE_LOCK-$WORK/reconfigure.lock}" \
        RENEW_SETTLE=1 REASSOCIATE_SETTLE=1 RECONFIGURE_SETTLE=1 PATH="$WORK/bin:$PATH" \
        UPTIME_FILE_OVERRIDE="$WORK/uptime" STATE_DIR_OVERRIDE="$WORK/state" \
        "$SCRIPT" wlan0 >/dev/null 2>&1 || rc=$?
    printf '%s' "$rc"
}
actions() {
    [ -r "$WORK/actions" ] || { printf ''; return; }
    paste -sd, <"$WORK/actions"
}
reset_actions() { rm -f "$WORK/actions"; }

# A lease that works on this segment needs the renew and nothing else.
segment 1 1
uptime_at 1000
associate aa:bb:cc:dd:ee:01 100
check 'working lease reasserted' 0 "$(run)"
check 'working lease needs only a renew' "$RENEW" "$(actions)"

# The same association must not be touched twice, including across the one-second jitter that
# `connected time` resolution introduces into the derived association start.
reset_actions
uptime_at 1005
associate aa:bb:cc:dd:ee:01 104
check 'unchanged association is a no-op' 0 "$(run)"
check 'unchanged association acts on nothing' '' "$(actions)"

# The roam shape: forwarding is withheld, and only rebuilding the association restores it. It must be
# tried before DHCP, because walking produced a roam every ten to twenty-five seconds and a repair
# that spends twelve seconds on DHCP first never lands.
reset_actions
segment 0 0
uptime_at 1200
associate aa:bb:cc:dd:ee:02 2
check 'withheld forwarding is repaired' 0 "$(FIXED_BY=reassociate run)"
check 'reassociation is tried before DHCP' "$RENEW,$REASSOCIATE" "$(actions)"

# Reassociating creates a new association, which retriggers this hook. The cooldown is the only thing
# between that and a reassociation loop, so an attempt inside it must fall through to DHCP.
reset_actions
segment 0 0
uptime_at 1205
associate aa:bb:cc:dd:ee:05 3
check 'a second reassociation inside the cooldown is refused' 0 "$(FIXED_BY=reconfigure run)"
check 'cooldown skips straight to DHCP' "$RENEW,$RECONFIGURE" "$(actions)"

# The resume shape: the lease belongs to another VLAN, so reassociating cannot help and the ladder has
# to carry on to a full DHCP.
reset_actions
segment 0 0
uptime_at 1400
associate aa:bb:cc:dd:ee:03 2
check 'lease from another segment is repaired' 0 "$(FIXED_BY=reconfigure run)"
check 'ladder continues to a full DHCP' "$RENEW,$REASSOCIATE,$RECONFIGURE" "$(actions)"

# A reassociation to the same BSSID resets connected time. The controller dropped the client context,
# so this is a new association even though the BSSID did not change.
reset_actions
segment 1 1
uptime_at 1600
associate aa:bb:cc:dd:ee:03 2
check 'reassociation to the same BSSID reasserts' 0 "$(run)"
check 'reassociation to the same BSSID renews' "$RENEW" "$(actions)"

# Nothing to do while disassociated, and the record must be dropped so reconnecting to the same BSSID
# at a similar uptime still counts as new.
reset_actions
disassociate
check 'disassociated is a no-op' 0 "$(run)"
check 'disassociated acts on nothing' '' "$(actions)"
if [ -e "$WORK/state/wlan0" ]; then
    echo 'FAIL disassociating leaves a stale association record' >&2
    fail=1
else
    echo 'OK   disassociating forgets the association'
fi

reset_actions
uptime_at 1700
associate aa:bb:cc:dd:ee:04 4
check 'associated without an address is a no-op' 0 "$(ADDR_OUTPUT= run)"
check 'no address means no action' '' "$(actions)"

# Past the cooldown the reassociation is available again.
reset_actions
segment 0 0
uptime_at 1800
associate aa:bb:cc:dd:ee:06 3
check 'reassociation is available past the cooldown' 0 "$(FIXED_BY=reassociate run)"
check 'ladder reassociates again past the cooldown' "$RENEW,$REASSOCIATE" "$(actions)"
# One attempt per association: retrying here would loop, because every step changes link state.
reset_actions
uptime_at 1802
associate aa:bb:cc:dd:ee:06 5
check 'a handled association is not retried' 0 "$(run)"
check 'no repair loop' '' "$(actions)"

# The cooldown is measured against /proc/uptime, which restarts at zero, so a stamp left by a longer
# previous boot sits in the future and never expires. While this state lived under /var/lib that
# refused every reassociation: in the 2026-09-18 19:10 blackhole the repair fired on each association
# and did nothing for three minutes, until the access point deauthenticated the client on its own.
reset_actions
segment 0 0
uptime_at 1900
printf '99999\n' >"$WORK/state/reassociated"
associate aa:bb:cc:dd:ee:0e 3
check 'a stamp from a previous boot is not a cooldown' 0 "$(FIXED_BY=reassociate run)"
check 'stale stamp still reassociates' "$RENEW,$REASSOCIATE" "$(actions)"

# When nothing recovers the segment, that must be reported rather than reported as success.
reset_actions
segment 0 0
uptime_at 2000
associate aa:bb:cc:dd:ee:07 3
check 'unrecoverable segment reports failure' 1 "$(run)"
check 'unrecoverable segment tried the whole ladder' "$RENEW,$REASSOCIATE,$RECONFIGURE" "$(actions)"

# iwd being undrivable must not stop the ladder; DHCP is the only step left.
reset_actions
segment 0 0
uptime_at 2100
associate aa:bb:cc:dd:ee:08 3
check 'unusable iwd still reaches DHCP' 0 "$(IWCTL_FAILS=1 FIXED_BY=reconfigure run)"
check 'ladder skips a dead iwd' \
    "$RENEW,iwctl station wlan0 disconnect,$RECONFIGURE" "$(actions)"

# A renew that cannot be delivered must not stop the escalation either.
reset_actions
segment 0 0
uptime_at 2200
associate aa:bb:cc:dd:ee:09 3
check 'undeliverable renew still escalates' 0 "$(RENEW_FAILS=1 FIXED_BY=reassociate run)"
check 'escalation follows a failed renew' "$RENEW,$REASSOCIATE" "$(actions)"

# The shape that made this hook report success in the middle of the 2026-09-18 13:20:17 outage: the
# controller answers ARP at layer two while refusing to route, so a gateway that replies proves only
# that the lease names the right segment. Escalation has to happen on the probe, not on the ARP reply.
reset_actions
segment 1 0
uptime_at 2400
associate aa:bb:cc:dd:ee:0b 3
check 'an answering gateway that does not forward is repaired' 0 "$(FIXED_BY=reassociate run)"
check 'forwarding failure escalates despite ARP' "$RENEW,$REASSOCIATE" "$(actions)"

# The probe must actually be consulted, otherwise the above passes for the wrong reason.
if [ -s "$WORK/probes" ]; then
    printf 'OK   forwarding is confirmed by a probe (%s calls)\n' "$(wc -l <"$WORK/probes")"
else
    echo 'FAIL forwarding was never probed; ARP alone decided the verdict' >&2
    fail=1
fi
# It has to be pinned to the physical path, or it can succeed through the tunnel it is meant to test.
if grep -q -- "--interface wlan0" "$WORK/probes" && grep -q -- '--mark 0x80000' "$WORK/probes" &&
    grep -q -- '--source 10.36.55.44' "$WORK/probes"; then
    echo 'OK   probe is pinned to the physical interface, source and mark'
else
    echo 'FAIL probe is not pinned to the physical path' >&2
    sed -n 1p "$WORK/probes" >&2
    fail=1
fi

# A probe that cannot be placed measured nothing. Reading that as failure would reassociate on every
# association whenever the probe itself is broken, so it must not be a verdict.
reset_actions
segment 1 0
uptime_at 2500
associate aa:bb:cc:dd:ee:0c 3
check 'an unplaceable probe does not trigger a repair' 0 "$(PROBE_UNPLACEABLE=1 run)"
check 'unplaceable probe acts only on the renew' "$RENEW" "$(actions)"

# The repair reconnects to the SSID and lets iwd pick the BSS. Aiming it at a chosen BSSID needs
# iwd's StationDebug interface, which only exists in developer mode; enabling that replaced iwd's
# ExecStart and left the machine with no wireless at all, so the repair stays band-agnostic.
reset_actions
segment 1 0
uptime_at 2600
associate aa:bb:cc:dd:ee:0d 3
check 'a blackholed association is repaired by reassociating' 0 "$(FIXED_BY=reassociate run)"
check 'repair never addresses a BSSID directly' "$RENEW,$REASSOCIATE" "$(actions)"

# A repair must not interleave with a policy reconciliation. Both are started by the same change to
# /run/systemd/netif/links, and reassociating here tears down the link that reconciliation is building
# policy against, so the two have to share network-reconfigure's lock. Holding that lock elsewhere must
# therefore block the repair rather than let it proceed in parallel.
reset_actions
segment 1 1
uptime_at 2300
associate aa:bb:cc:dd:ee:0a 3
held=$WORK/held.lock
: >"$held"
flock "$held" sleep 3 &
holder=$!
sleep 0.3
before=$SECONDS
blocked=$(RECONFIGURE_LOCK="$held" run)
waited=$((SECONDS - before))
wait "$holder" 2>/dev/null || true
check 'repair waits for a held reconciliation lock' 0 "$blocked"
# Timing is the only thing that distinguishes taking the lock from ignoring it: without the flock this
# returns immediately instead of waiting out the two and a half seconds left on the holder.
if [ "$waited" -ge 2 ]; then
    printf 'OK   repair blocked for %ss rather than running in parallel\n' "$waited"
else
    printf 'FAIL repair did not wait for the reconciliation lock (returned in %ss)\n' "$waited" >&2
    fail=1
fi
check 'repair still acts once the lock is free' "$RENEW" "$(actions)"

exit "$fail"
