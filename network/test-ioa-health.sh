#!/usr/bin/env bash
# Behavioural fixture for scripts/ioa-health.
#
# The decision is "is the GUI holding a connection to its local agent", and the risk of the fix is
# the mirror image: every restart costs a re-login, so a false positive is worse than the stall it
# repairs. Both directions are pinned here, including the two cases that made an earlier version of
# this script wrong and are kept as regressions:
#
#   - a refused ticket is not a dead tunnel (2026-10-24: four `get ticket result:-2` lines while
#     the agent was forwarding intranet traffic through the tunnel normally),
#   - a fresh association must not be mistaken for a stuck window.
set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT="$ROOT/scripts/ioa-health"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
fail=0

check() {
    local description=$1 expected=$2 actual=$3
    if [ "$expected" != "$actual" ]; then
        printf 'FAIL %s\n  expected: %s\n  actual:   %s\n' \
            "$description" "$expected" "$actual" >&2
        fail=1
    fi
}

cat >"$WORK/systemctl" <<'SH'
#!/bin/sh
case "$*" in
    "--system is-active ngnclient.service") cat "$DAEMON_STATE" ;;
    "--user is-active ioagui.service") cat "$GUI_STATE" ;;
    "--user restart ioagui.service") printf 'restart\n' >>"$RESTART_LOG" ;;
    *) echo "unexpected: $*" >&2; exit 2 ;;
esac
SH
chmod 755 "$WORK/systemctl"

# `ss` prints a header-less connection table. $LINK decides whether the session channel is in it;
# the command channel (:56100) is always present because a login handshake leaves it behind, and it
# must not be mistaken for a working session.
cat >"$WORK/ss" <<'SH'
#!/bin/sh
printf '0 0 127.0.0.1:54596 127.0.0.1:56100\n'
[ "$(cat "$LINK")" = yes ] && printf '0 0 127.0.0.1:35456 127.0.0.1:56101\n'
exit 0
SH
chmod 755 "$WORK/ss"

run() {
    local rc=0
    SS_OVERRIDE="$WORK/ss" SYSTEMCTL_OVERRIDE="$WORK/systemctl" \
        STATE_DIR_OVERRIDE="$WORK/state" NOW_EPOCH_OVERRIDE="${NOW_EPOCH-1000}" \
        GRACE_OVERRIDE="${GRACE-120}" COOLDOWN_OVERRIDE="${COOLDOWN-900}" \
        GIVE_UP_OVERRIDE="${GIVE_UP-3}" \
        DAEMON_STATE="$WORK/daemon" GUI_STATE="$WORK/gui-state" RESTART_LOG="$WORK/restarts" \
        LINK="$WORK/link" bash "$SCRIPT" >"$WORK/out" 2>&1 || rc=$?
    printf 'rc=%s' "$rc"
}

restarts() { [ -r "$WORK/restarts" ] && wc -l <"$WORK/restarts" | tr -d ' ' || echo 0; }

reset() {
    rm -rf "$WORK/state" "$WORK/restarts"
    printf 'active\n' >"$WORK/daemon"
    printf 'active\n' >"$WORK/gui-state"
    printf 'yes\n' >"$WORK/link"
}

# A working GUI holds the socket, and the machine spends its life in this state. Nothing may happen.
reset
check 'a working GUI is left alone' 'rc=0' "$(NOW_EPOCH=1000 run)"
check 'a working GUI is not restarted' 0 "$(restarts)"
[ -d "$WORK/state" ] && check 'a working GUI leaves no state' 'absent' 'present' ||
    check 'a working GUI leaves no state' 'absent' 'absent'

# The stall: the socket is gone and stays gone. The first sighting only starts the clock.
reset; printf 'no\n' >"$WORK/link"
check 'the first sighting only starts the clock' 'rc=0' "$(NOW_EPOCH=1000 run)"
check 'the first sighting does not restart' 0 "$(restarts)"

# Still inside GRACE: a GUI mid-reconnect after a roam looks exactly like this for a few seconds.
check 'a short gap is tolerated' 'rc=0' "$(NOW_EPOCH=1060 run)"
check 'a short gap does not restart' 0 "$(restarts)"

# Past GRACE: the retry ladder is over and the window is stuck on the reconnect icon.
check 'a held gap is repaired' 'rc=0' "$(NOW_EPOCH=1125 run)"
check 'a held gap is repaired once' 1 "$(restarts)"

# The timer fires every minute; the same stall may not restart the GUI twice.
check 'a tick inside the cooldown is a no-op' 'rc=0' "$(NOW_EPOCH=1200 run)"
check 'the cooldown holds' 1 "$(restarts)"

# Recovery — the socket came back — clears the allowance so a later incident gets a full one.
printf 'yes\n' >"$WORK/link"
NOW_EPOCH=1300 run >/dev/null
[ -d "$WORK/state" ] && check 'recovery clears the allowance' 'gone' 'present' ||
    check 'recovery clears the allowance' 'gone' 'gone'

# A later incident is separate from the first and earns its own restart after its own GRACE.
printf 'no\n' >"$WORK/link"
NOW_EPOCH=2000 run >/dev/null
NOW_EPOCH=2130 run >/dev/null
check 'a later incident earns a second restart' 2 "$(restarts)"

# The GUI being stopped is the user's decision, and starting it is not this script's.
reset; printf 'no\n' >"$WORK/link"; printf 'inactive\n' >"$WORK/gui-state"
NOW_EPOCH=3000 run >/dev/null
NOW_EPOCH=3130 run >/dev/null
check 'a stopped GUI is left stopped' 0 "$(restarts)"

# The daemon being down is systemd's recovery; restarting the GUI then would only start a second
# stuck ladder against a dead agent.
reset; printf 'no\n' >"$WORK/link"; printf 'failed\n' >"$WORK/daemon"
NOW_EPOCH=4000 run >/dev/null
NOW_EPOCH=4130 run >/dev/null
check 'a dead daemon is not this scripts repair' 0 "$(restarts)"

# A GUI that stays broken across restarts is not repaired by restarting, so the allowance runs out
# and the machine stops bouncing. This is the loop guard that matters most.
reset; printf 'no\n' >"$WORK/link"
for t in 5000 5130 6000 6130 7000 7130; do
    NOW_EPOCH=$t run >/dev/null
done
check 'the allowance stops further bounces' 3 "$(restarts)"

# Loopback may be resolved as ::1, and the port is what identifies the agent either way.
reset
cat >"$WORK/ss" <<'SH'
#!/bin/sh
printf '0 0 [::1]:54596 [::1]:56100\n0 0 [::1]:35456 [::1]:56101\n'
SH
chmod 755 "$WORK/ss"
check 'a v6 loopback link counts as connected' 'rc=0' "$(NOW_EPOCH=1000 run)"
check 'a v6 loopback link is not restarted' 0 "$(restarts)"

# The command channel alone is not a working session. This is the shape that made an earlier
# version of this script blind: it accepted either port, so a GUI whose session channel had gone
# still looked connected because the handshake socket was still in the table.
reset
cat >"$WORK/ss" <<'SH'
#!/bin/sh
printf '0 0 127.0.0.1:54596 127.0.0.1:56100\n'
SH
chmod 755 "$WORK/ss"
NOW_EPOCH=8000 run >/dev/null
NOW_EPOCH=8130 run >/dev/null
check 'the command channel alone is not a session' 1 "$(restarts)"

# A connection table with no agent entry at all, which is what a refused socket looks like.
reset
cat >"$WORK/ss" <<'SH'
#!/bin/sh
printf '0 0 10.36.48.168:35644 199.165.136.100:443\n'
SH
chmod 755 "$WORK/ss"
NOW_EPOCH=9000 run >/dev/null
NOW_EPOCH=9130 run >/dev/null
check 'an unrelated connection is not the agent' 1 "$(restarts)"

if [ "$fail" -ne 0 ]; then
    echo 'FAIL ioa-health' >&2
    exit 1
fi
echo 'PASS ioa-health'
