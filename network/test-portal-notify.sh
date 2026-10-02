#!/usr/bin/env bash
# Behavioural fixture for scripts/portal-notify.
#
# The script exists because root cannot reach a user's dunst — the session bus authenticates by uid —
# so the notification has to be sent with the session's own credentials. What is asserted here is
# that the privilege drop carries everything the notification and the browser need, and that a
# dismissed notice is not retried.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT="$ROOT/scripts/portal-notify"
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
cat >"$WORK/bin/setpriv" <<'SH'
#!/bin/sh
# Record everything the session is handed, then do what the real setpriv would: run the command.
# The notification call is the one whose command is the notifier, and it answers with the action the
# test chose; the opener is executed for real, which is how the fixture sees the URL.
printf '%s\n' "$*" >>"WORKDIR/setpriv-calls"
while [ $# -gt 0 ]; do
    case $1 in
        --reuid=*|--regid=*|--init-groups) shift ;;
        env) shift; break ;;
        *) break ;;
    esac
done
# `env` assignments follow until the command.
while [ $# -gt 0 ]; do
    case $1 in
        *=*) shift ;;
        *) break ;;
    esac
done
[ $# -gt 0 ] || exit 0
case $1 in
    *notify*|*dunstify*) cat WORKDIR/action 2>/dev/null || printf 'dismiss' ;;
    *) "$@" ;;
esac
SH
cat >"$WORK/bin/stat" <<'SH'
#!/bin/sh
printf '1000\n'
SH
cat >"$WORK/bin/xdg-open" <<'SH'
#!/bin/sh
printf '%s\n' "$1" >>"WORKDIR/opened"
SH
chmod 755 "$WORK/bin"/*
sed -i "s|WORKDIR|$WORK|g" "$WORK/bin"/*

printf 'https://wifiportal.yaduo.com/web/mobile.html?gx_token=abc\n' >"$WORK/url"
printf 'dunstify\n' >"$WORK/notify"   # any executable path will do; the stub setpriv is what runs
chmod 755 "$WORK/notify"

run() {
    local rc=0
    PATH="$WORK/bin:$PATH" PORTAL_URL_STATE_OVERRIDE="$WORK/url" \
        NOTIFY="${NOTIFY:-$WORK/notify}" OPEN="$WORK/bin/xdg-open" \
        "$SCRIPT" "$@" >"$WORK/out" 2>"$WORK/err" || rc=$?
    printf '%s' "$rc"
}

# Clicking the notice opens the recorded URL — token and all. Opening the bare hostname instead is
# what sent the operator to a page answering `1` on this network.
printf 'open' >"$WORK/action"
rm -f "$WORK/opened"
check 'acting on the notice succeeds' 0 "$(run)"
check 'the recorded URL is what opens' \
    'https://wifiportal.yaduo.com/web/mobile.html?gx_token=abc' "$(cat "$WORK/opened")"

# The privilege drop has to carry HOME as well as the display: without it xdg-open writes its MIME
# cache under /root and fails before reaching a browser.
if grep -q 'HOME=/home/' "$WORK/setpriv-calls" && grep -q 'DISPLAY=' "$WORK/setpriv-calls"; then
    echo 'OK   the session environment is passed to the opener'
else
    printf 'FAIL the opener is missing HOME or DISPLAY: %s\n' "$(cat "$WORK/setpriv-calls")" >&2
    fail=1
fi
if grep -q 'DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus' "$WORK/setpriv-calls"; then
    echo 'OK   the notification goes to the session bus'
else
    printf 'FAIL the session bus is not named: %s\n' "$(cat "$WORK/setpriv-calls")" >&2
    fail=1
fi

# Dismissing is a decision, not a failure. Nothing opens, and the exit stays clean so the caller
# does not treat it as a fault.
printf 'dismiss' >"$WORK/action"
rm -f "$WORK/opened"
check 'dismissing the notice is not an error' 0 "$(run)"
check 'dismissing opens nothing' '' "$(cat "$WORK/opened" 2>/dev/null)"

# No recorded portal and no --force: nothing to say.
rm -f "$WORK/url"
check 'a link with no portal is silent' 0 "$(run)"
check 'a link with no portal opens nothing' '' "$(cat "$WORK/opened" 2>/dev/null)"

# While the notice is up the link is probed again. A confirmed open is withdrawn; that is not a
# dismissal, and it must not open the browser on the way out.
printf 'https://wifiportal.yaduo.com/web/mobile.html?gx_token=abc\n' >"$WORK/url"
printf 'wlan0|||10.0.0.1\n' >"$WORK/identity"
cat >"$WORK/bin/portal-login" <<'SH'
#!/bin/sh
printf 'open\n'
SH
cat >"$WORK/bin/reconfigure" <<'SH'
#!/bin/sh
rm -f WORKDIR/url
SH
cat >"$WORK/bin/dunstctl" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >>WORKDIR/closed
touch WORKDIR/stop
SH
# Not named notify: the setpriv stub answers those itself, and this one has to block until closed.
cat >"$WORK/bin/waiter" <<'SH'
#!/bin/sh
while [ ! -f WORKDIR/stop ]; do sleep 0.05; done
printf 'dismiss\n'
SH
chmod 755 "$WORK/bin/portal-login" "$WORK/bin/reconfigure" "$WORK/bin/dunstctl" "$WORK/bin/waiter"
sed -i "s|WORKDIR|$WORK|g" "$WORK/bin/reconfigure" "$WORK/bin/dunstctl" "$WORK/bin/waiter"
rm -f "$WORK/opened" "$WORK/closed" "$WORK/stop"
check 'an open recheck withdraws the notice' 0 \
    "$(PORTAL_RECHECK=1 PORTAL_RECHECK_INTERVAL=0 \
        NOTIFY="$WORK/bin/waiter" \
        PORTAL_LOGIN="$WORK/bin/portal-login" RECONFIGURE="$WORK/bin/reconfigure" \
        DUNSTCTL="$WORK/bin/dunstctl" \
        PORTAL_IDENTITY_STATE_OVERRIDE="$WORK/identity" \
        run)"
check 'an open recheck does not open the browser' '' "$(cat "$WORK/opened" 2>/dev/null)"
if grep -q 'close 99114' "$WORK/closed"; then
    echo 'OK   an open recheck closes the notification'
else
    printf 'FAIL the notification was not closed: %s\n' "$(cat "$WORK/closed" 2>/dev/null)" >&2
    fail=1
fi
if [ ! -e "$WORK/url" ]; then
    echo 'OK   an open recheck asks the reconciler to drop the login URL'
else
    echo 'FAIL the login URL survived an open recheck' >&2
    fail=1
fi

exit "$fail"
