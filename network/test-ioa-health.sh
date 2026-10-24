#!/usr/bin/env bash
# Behavioural fixture for scripts/ioa-health.
#
# The decision is "is the GUI holding its session channel to the local agent", and the risk of the
# fix is the mirror image: every restart costs a re-login, so a false positive is worse than the
# stall it repairs. Both directions are pinned here.
#
# The thresholds are NOT overridden, so this fails if a default is changed without the reasoning
# behind it being revisited. The timestamps below are therefore scaled to the real GRACE (600),
# COOLDOWN (1800), GIVE_UP (2) and FORGIVE (3600).
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

# `ss` prints a header-less connection table. $LINK decides whether the session channel is present;
# the command channel (:56100) is always printed because a login handshake leaves it behind, and it
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
        STATE_DIR_OVERRIDE="$WORK/state" NOW_EPOCH_OVERRIDE="${CLOCK-${NOW_EPOCH-1000}}" \
        DAEMON_STATE="$WORK/daemon" GUI_STATE="$WORK/gui-state" RESTART_LOG="$WORK/restarts" \
        LINK="$WORK/link" bash "$SCRIPT" >"$WORK/out" 2>&1 || rc=$?
    printf 'rc=%s' "$rc"
}

restarts() { [ -r "$WORK/restarts" ] && wc -l <"$WORK/restarts" | tr -d ' ' || echo 0; }
counted() { cat "$WORK/state/restarts" 2>/dev/null || echo none; }

reset() {
    rm -rf "$WORK/state" "$WORK/restarts"
    printf 'active\n' >"$WORK/daemon"
    printf 'active\n' >"$WORK/gui-state"
    printf 'yes\n' >"$WORK/link"
    cat >"$WORK/ss" <<'SH'
#!/bin/sh
printf '0 0 127.0.0.1:54596 127.0.0.1:56100\n'
[ "$(cat "$LINK")" = yes ] && printf '0 0 127.0.0.1:35456 127.0.0.1:56101\n'
exit 0
SH
    chmod 755 "$WORK/ss"
}

# --- healthy -----------------------------------------------------------------------------
# The machine spends its life here. Nothing may happen, and the only state written is the health
# clock that lets a later incident earn a fresh allowance.
reset
check 'a working GUI is left alone' 'rc=0' "$(NOW_EPOCH=1000 run)"
check 'a working GUI is not restarted' 0 "$(restarts)"
[ -r "$WORK/state/healthy-since" ] && check 'health is recorded' yes yes ||
    check 'health is recorded' yes no
[ -r "$WORK/state/restarts" ] && check 'a healthy run counts no restarts' gone present ||
    check 'a healthy run counts no restarts' gone gone

# --- the stall ---------------------------------------------------------------------------
# The channel is gone and stays gone. The first sighting only starts the clock.
reset; printf 'no\n' >"$WORK/link"
check 'the first sighting only starts the clock' 'rc=0' "$(NOW_EPOCH=1000 run)"
check 'the first sighting does not restart' 0 "$(restarts)"

# Still inside GRACE: every roam takes this channel away for a few seconds, which is why the
# threshold exists at all.
check 'a short gap is tolerated' 'rc=0' "$(NOW_EPOCH=1500 run)"
check 'a short gap does not restart' 0 "$(restarts)"

# Past GRACE: the retry ladder ended long ago and the window is stuck on the reconnect icon.
check 'a held gap is repaired' 'rc=0' "$(NOW_EPOCH=1700 run)"
check 'a held gap is repaired once' 1 "$(restarts)"
check 'the repair is counted' 1 "$(counted)"

# The timer fires every minute. A GUI that is slow to open its channel after a restart must not be
# restarted again: the clock is reset at the restart, so the replacement gets a full GRACE.
#
# Asserted on the stamp rather than through behaviour, because COOLDOWN outlasts GRACE and would
# mask a missing reset at every timestamp a behavioural test could reach.
check 'the restart resets the clock' 1700 "$(cat "$WORK/state/held-since" 2>/dev/null)"
check 'the replacement GUI gets a full grace' 'rc=0' "$(NOW_EPOCH=1800 run)"
check 'the replacement GUI is not restarted' 1 "$(restarts)"

# Inside COOLDOWN, even though the channel has been gone longer than GRACE again.
check 'the cooldown holds' 'rc=0' "$(NOW_EPOCH=2400 run)"
check 'no restart inside the cooldown' 1 "$(restarts)"

# --- the allowance -----------------------------------------------------------------------
# A GUI that stays broken is not repaired by restarting, so the allowance runs out and the machine
# stops bouncing. This is the guard that matters most: a permanent loop would cost a login a minute.
check 'a second attempt is allowed after the cooldown' 'rc=0' "$(NOW_EPOCH=3600 run)"
check 'the second attempt happened' 2 "$(restarts)"
check 'the allowance is spent' 'rc=0' "$(NOW_EPOCH=5500 run)"
check 'the allowance stops further bounces' 2 "$(restarts)"

# --- forgiveness -------------------------------------------------------------------------
# Health must last FORGIVE before the allowance is forgiven, so one healthy sample between two
# failures is not mistaken for a repair.
printf 'yes\n' >"$WORK/link"
run_at() { NOW_EPOCH=$1 run >/dev/null; }
run_at 5600
check 'a brief recovery keeps the allowance' 2 "$(counted)"
run_at 8000
check 'a brief recovery still keeps it' 2 "$(counted)"
run_at 9200
check 'an hour of health forgives it' none "$(counted)"

# With the allowance restored, the next incident gets a full one.
printf 'no\n' >"$WORK/link"
run_at 9300
run_at 10000
check 'the next incident is repaired' 3 "$(restarts)"

# --- the two components the GUI cannot work without -------------------------------------
# The GUI being stopped is the user's decision; starting it is not this script's.
reset; printf 'no\n' >"$WORK/link"; printf 'inactive\n' >"$WORK/gui-state"
run_at 20000; run_at 20700
check 'a stopped GUI is left stopped' 0 "$(restarts)"

# With the daemon down there is nothing to connect to, and systemd already owns that recovery;
# restarting the GUI would only begin a second ladder against a dead agent.
#
# The clock is also cleared while it is down. Leaving it to run would mean the first tick after the
# daemon returns sees a mark that aged through the whole outage and restarts a GUI that has had no
# chance to reconnect yet.
reset; printf 'no\n' >"$WORK/link"
run_at 30000
check 'the clock runs while both are up' 30000 "$(cat "$WORK/state/held-since" 2>/dev/null)"
printf 'failed\n' >"$WORK/daemon"
run_at 30700
check 'a dead daemon is not this scripts repair' 0 "$(restarts)"
[ -r "$WORK/state/held-since" ] && check 'the clock is cleared while it is down' cleared kept ||
    check 'the clock is cleared while it is down' cleared cleared
printf 'active\n' >"$WORK/daemon"
check 'the returned daemon gets a fresh grace' 'rc=0' "$(NOW_EPOCH=31000 run)"
check 'a fresh clock is started' 31000 "$(cat "$WORK/state/held-since" 2>/dev/null)"
check 'the returned daemon is not restarted at once' 0 "$(restarts)"
run_at 40000
check 'a full grace after the return does restart it' 1 "$(restarts)"

# --- what the channel test must not accept -----------------------------------------------
# The command channel alone is not a session. An earlier version accepted either port and was
# blind to exactly the case this exists for.
reset
cat >"$WORK/ss" <<'SH'
#!/bin/sh
printf '0 0 127.0.0.1:54596 127.0.0.1:56100\n'
SH
chmod 755 "$WORK/ss"
run_at 40000; run_at 40700
check 'the command channel alone is not a session' 1 "$(restarts)"

# A table with no agent entry at all, which is what a refused socket looks like.
reset
cat >"$WORK/ss" <<'SH'
#!/bin/sh
printf '0 0 10.36.48.168:35644 199.165.136.100:443\n'
SH
chmod 755 "$WORK/ss"
run_at 50000; run_at 50700
check 'an unrelated connection is not the agent' 1 "$(restarts)"

# Loopback may be resolved as ::1; the port identifies the agent either way.
reset
cat >"$WORK/ss" <<'SH'
#!/bin/sh
printf '0 0 [::1]:54596 [::1]:56100\n0 0 [::1]:35456 [::1]:56101\n'
SH
chmod 755 "$WORK/ss"
check 'a v6 loopback link counts as connected' 'rc=0' "$(NOW_EPOCH=60000 run)"
check 'a v6 loopback link is not restarted' 0 "$(restarts)"

# A restart that the unit manager refuses must not be retried every tick: the cooldown is written
# before the command runs, precisely so a failing manager cannot turn this into a loop.
reset; printf 'no\n' >"$WORK/link"
cat >"$WORK/systemctl" <<'SH'
#!/bin/sh
case "$*" in
    "--system is-active ngnclient.service") cat "$DAEMON_STATE" ;;
    "--user is-active ioagui.service") cat "$GUI_STATE" ;;
    "--user restart ioagui.service") printf 'attempt\n' >>"$RESTART_LOG"; exit 1 ;;
    *) echo "unexpected: $*" >&2; exit 2 ;;
esac
SH
chmod 755 "$WORK/systemctl"
run_at 60000
check 'a refused restart is reported' 'rc=1' "$(NOW_EPOCH=61000 run)"
check 'a refused restart is not counted' none "$(counted)"
check 'the failed attempt is on record' 1 "$(restarts)"
check 'a refused restart is not retried' 'rc=0' "$(NOW_EPOCH=61500 run)"
run_at 62500
check 'nor on the ticks after it' 1 "$(restarts)"

# --- a broken environment is not a verdict either -----------------------------------------
# No clock means the thresholds cannot be evaluated at all. Failing is right; what must not happen
# is arithmetic on an empty string, which under `set -u` aborts with a message about an unbound
# variable rather than about the real fault.
reset
check 'an unusable clock is reported' 'rc=1' "$(CLOCK=not-a-clock run)"

# --- measuring nothing is not a verdict --------------------------------------------------
# If `ss` cannot run, the script must not conclude that the channel is gone: that would restart the
# GUI every minute for the duration of an unrelated fault.
reset
cat >"$WORK/ss" <<'SH'
#!/bin/sh
exit 1
SH
chmod 755 "$WORK/ss"
check 'an unreadable table is reported' 'rc=1' "$(NOW_EPOCH=70000 run)"
run_at 71000
check 'an unreadable table never restarts' 0 "$(restarts)"

# A state directory that cannot be created must fail loudly. The alternative — swallowing the write
# and comparing against a clock that resets every tick — is a unit that runs, reports success, and
# never fires. That was the first version of this script.
#
# A path under an ordinary file rather than a mode-500 directory: the permission bit does not stop
# root, so a fixture using it would pass when run by hand and fail under `sudo`, which is exactly
# how that bug survived the first time.
reset
cat >"$WORK/ss" <<'SH'
#!/bin/sh
printf '0 0 127.0.0.1:54596 127.0.0.1:56100\n'
exit 0
SH
chmod 755 "$WORK/ss"
: >"$WORK/not-a-directory"
check 'an unusable state directory fails' 'rc=1' "$(
    NOW_EPOCH=80000 SS_OVERRIDE="$WORK/ss" SYSTEMCTL_OVERRIDE="$WORK/systemctl" \
        STATE_DIR_OVERRIDE="$WORK/not-a-directory/state" RESTART_LOG="$WORK/restarts" \
        DAEMON_STATE="$WORK/daemon" GUI_STATE="$WORK/gui-state" LINK="$WORK/link" \
        bash "$SCRIPT" >/dev/null 2>&1; echo "rc=$?"
)"
check 'an unusable state directory never restarts' 0 "$(restarts)"

if [ "$fail" -ne 0 ]; then
    echo 'FAIL ioa-health' >&2
    exit 1
fi
echo 'PASS ioa-health'
