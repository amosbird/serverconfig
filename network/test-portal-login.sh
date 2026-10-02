#!/usr/bin/env bash
# Behavioural fixture for scripts/portal-login.
#
# The script exists because the portal hostname alone is not the page that can authenticate a
# station: `https://wifiportal.yaduo.com/` answers `1` (a health check) while the page that works
# carries a per-association token in its query. What is asserted here is that the script prefers the
# recorded tokenised URL, falls back to probing the link when there is no record, and refuses to
# invent a portal that is not there.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT="$ROOT/scripts/portal-login"
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
cat >"$WORK/bin/ip" <<'SH'
#!/bin/sh
printf 'default via 10.36.48.1 dev wlan0 proto dhcp src 10.36.55.44 metric 600\n'
SH
# The canary probe. `portal-landing` holds what the link redirects to; without it the link answers as
# itself, which is how an open network looks.
cat >"$WORK/bin/curl" <<'SH'
#!/bin/sh
landing=''
if [ -r WORKDIR/portal-landing ]; then
    landing=$(cat WORKDIR/portal-landing)
else
    for arg in "$@"; do
        case $arg in http*) landing=$arg ;; esac
    done
fi
printf '200 %s' "$landing"
SH
printf '#!/bin/sh\nprintf "opened %%s\\n" "$1"\n' >"$WORK/bin/open"
chmod 755 "$WORK/bin"/*
sed -i "s|WORKDIR|$WORK|g" "$WORK/bin/curl"

# STATE defaults to the recorded-URL fixture; pass a path that does not exist to exercise the probe.
run() {
    local rc=0
    PATH="$WORK/bin:$PATH" PORTAL_URL_STATE_OVERRIDE="${STATE-$WORK/state}" \
        OPEN="$WORK/bin/open" CURL_OVERRIDE="$WORK/bin/curl" \
        "$SCRIPT" "$@" >"$WORK/out" 2>"$WORK/err" || rc=$?
    printf '%s' "$rc"
}

# With a recorded URL, the recorded URL is what gets opened — token and all. Probing instead would
# land on the health check again, which is the whole failure this script removes.
printf 'https://wifiportal.yaduo.com/web/mobile.html?gx_token=e774c9d4667c0f80\n' >"$WORK/state"
check 'a recorded portal URL is opened' 0 "$(run)"
check 'the recorded URL is the one opened, token included' \
    'opened https://wifiportal.yaduo.com/web/mobile.html?gx_token=e774c9d4667c0f80' \
    "$(tail -1 "$WORK/out")"
# The URL is printed before the browser starts, so a failure to launch still leaves the operator with
# something to paste. That is the difference between a tool and a black box.
if [ "$(grep -c 'web/mobile.html' "$WORK/out")" -ge 2 ]; then
    echo 'OK   the URL is echoed before opening it'
else
    printf 'FAIL the URL is not echoed: %s\n' "$(cat "$WORK/out")" >&2
    fail=1
fi

# --print is for scripts and for reading aloud: it must not launch anything.
mv "$WORK/bin/open" "$WORK/bin/open.hidden"
check '--print does not open anything' \
    'https://wifiportal.yaduo.com/web/mobile.html?gx_token=e774c9d4667c0f80' \
    "$(PATH="$WORK/bin:$PATH" PORTAL_URL_STATE_OVERRIDE="$WORK/state" "$SCRIPT" --print)"
mv "$WORK/bin/open.hidden" "$WORK/bin/open"

# No record: probe the link. A gated link answers somewhere else, and that is the URL to open.
printf 'https://wifiportal.other.example/login?t=9\n' >"$WORK/portal-landing"
rm -f "$WORK/state"
check 'with no record the link is probed' 0 "$(run)"
check 'the probed landing URL is opened' \
    'opened https://wifiportal.other.example/login?t=9' "$(tail -1 "$WORK/out")"

# A link that answers as itself is not gated, and there is nothing to open.
rm -f "$WORK/portal-landing"
check 'an open link reports no portal' 1 "$(run)"
if grep -q 'not behind a captive portal' "$WORK/err"; then
    echo 'OK   an open link says so rather than opening something'
else
    printf 'FAIL an open link did not explain itself: %s\n' "$(cat "$WORK/err")" >&2
    fail=1
fi

# A canary that never connected is no evidence. The status is what separates it from one the link
# answered as itself, and reading it as "open" is the bug this fallback was written around.
cat >"$WORK/bin/curl" <<'SH'
#!/bin/sh
# The first canary never connects; the rest answer on the URL the link redirects to. That is the
# shape a portal produces when it blocks what it does not recognise, and reporting `000` for all of
# them as "not gated" is the bug this case exists to catch.
landing=''
[ ! -r WORKDIR/portal-landing ] || landing=$(cat WORKDIR/portal-landing)
case "$*" in
    *hicloud*) printf '000 %s' "${1##* }" ;;
    *) printf '200 %s' "$landing" ;;
esac
SH
sed -i "s|WORKDIR|$WORK|g" "$WORK/bin/curl"
printf 'https://wifiportal.other.example/login?t=9\n' >"$WORK/portal-landing"
check 'an unreachable canary is skipped, not read as open' \
    'https://wifiportal.other.example/login?t=9' \
    "$(PATH="$WORK/bin:$PATH" PORTAL_URL_STATE_OVERRIDE="$WORK/absent" "$SCRIPT" --print)"

# --verdict is the live probe the notification rechecks. open, captive and unknown are different
# answers: a timeout is not an open link, and a redirect is not "nothing to open".
cat >"$WORK/bin/curl" <<'SH'
#!/bin/sh
url=
for arg in "$@"; do
    case $arg in http*) url=$arg ;; esac
done
if [ -r WORKDIR/portal-landing ]; then
    printf '200 %s' "$(cat WORKDIR/portal-landing)"
    exit 0
fi
case $url in
    */generate_204) printf '204 %s' "$url" ;;
    *) printf '200 %s' "$url" ;;
esac
SH
sed -i "s|WORKDIR|$WORK|g" "$WORK/bin/curl"
rm -f "$WORK/portal-landing"
check '--verdict on an open link is open' open \
    "$(PATH="$WORK/bin:$PATH" CURL_OVERRIDE="$WORK/bin/curl" "$SCRIPT" --verdict)"
printf 'https://wifiportal.other.example/login?t=9\n' >"$WORK/portal-landing"
check '--verdict names the redirect' 'captive https://wifiportal.other.example/login?t=9' \
    "$(PATH="$WORK/bin:$PATH" CURL_OVERRIDE="$WORK/bin/curl" "$SCRIPT" --verdict)"
cat >"$WORK/bin/curl" <<'SH'
#!/bin/sh
printf '000 http://connectivitycheck.platform.hicloud.com/generate_204'
SH
check '--verdict on a dead link is unknown' unknown \
    "$(PATH="$WORK/bin:$PATH" CURL_OVERRIDE="$WORK/bin/curl" "$SCRIPT" --verdict)"

# --status is read-only and never opens.
mv "$WORK/bin/curl" "$WORK/bin/curl.hidden"
check '--status reports without opening' 0 "$(STATE=$WORK/state run --status)"
grep -q 'captive portal:' "$WORK/out" \
    && echo 'OK   --status names the recorded portal' \
    || { echo 'FAIL --status did not report' >&2; fail=1; }

exit "$fail"
