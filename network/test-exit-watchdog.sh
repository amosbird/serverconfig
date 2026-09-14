#!/usr/bin/env bash
# Behavioural tests for scripts/network-exit-watchdog.
#
#     bash network/test-exit-watchdog.sh
#
# Nothing here touches the real network or the real tailscaled: ip, iw, ping, curl, tailscale,
# systemctl, sleep and date are stubbed and every action the script takes is recorded. `jq` is the
# real one, so the exit-node detection filter is exercised as written.
set -uo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT="$ROOT/scripts/network-exit-watchdog"
pass=0; fail=0

ok()    { printf '  \033[32mOK\033[0m   %s\n' "$*"; pass=$((pass+1)); }
bad()   { printf '  \033[31mFAIL\033[0m %s\n' "$*"; fail=$((fail+1)); }
head_() { printf '\n\033[1m%s\033[0m\n' "$*"; }

SANDBOX=$(mktemp -d)
trap 'rm -rf "$SANDBOX"' EXIT
BIN="$SANDBOX/bin"
mkdir -p "$BIN"
STATE="$SANDBOX/state"
ACTIONS="$SANDBOX/actions"
ADDRS="$SANDBOX/addrs"

DEFAULT_ROUTE='default via 10.36.48.1 dev wlan0 proto dhcp src 10.36.50.129 metric 600'
WLAN_ADDR='3: wlan0    inet 10.36.50.129/20 brd 10.36.63.255 scope global wlan0'
TUN_ADDR='11: tun0    inet 192.168.255.10/24 scope global tun0'

cat >"$BIN/ip" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    *"-o addr show scope global"*)
        cat "$SANDBOX/addrs" 2>/dev/null
        ;;
    *"route show table main default"*)
        [ "${FAKE_HAS_DEFAULT:-1}" = 1 ] || exit 0
        printf '%s\n' "$FAKE_DEFAULT_ROUTE"
        ;;
    *"neigh show"*)
        [ "${FAKE_GATEWAY_GONE:-0}" = 1 ] && exit 0
        echo "172.20.32.1 dev wlan0 lladdr d4:b4:c0:5d:8f:99 REACHABLE"
        ;;
    *"route get"*)
        if [ "${FAKE_ROUTE_TUNNEL:-1}" = 1 ]; then
            echo "216.239.32.117 dev tailscale0 src 100.88.203.53 uid 0"
        else
            echo "216.239.32.117 via 10.36.48.1 dev wlan0 src 10.36.50.129 uid 0"
        fi
        ;;
esac
EOF

cat >"$BIN/iw" <<'EOF'
#!/usr/bin/env bash
[ -n "${FAKE_BSSID:-}" ] || exit 0
printf 'Connected to %s (on wlan0)\n\tSSID: XLSMART Public\n' "$FAKE_BSSID"
EOF

cat >"$BIN/ping" <<'EOF'
#!/usr/bin/env bash
exit "${FAKE_GATEWAY_DOWN:-0}"
EOF

# Probe results are a queue: one word per attempt, the last value repeats.
cat >"$BIN/curl" <<'EOF'
#!/usr/bin/env bash
read -r -a codes <<<"${FAKE_PROBES:-204}"
n=$(cat "$SANDBOX/probe-count" 2>/dev/null || echo 0)
printf '%s\n' "$((n + 1))" >"$SANDBOX/probe-count"
idx=$n
[ "$idx" -lt "${#codes[@]}" ] || idx=$(( ${#codes[@]} - 1 ))
printf '%s' "${codes[$idx]}"
EOF

cat >"$BIN/tailscale" <<'EOF'
#!/usr/bin/env bash
printf 'tailscale %s\n' "$*" >>"$ACTIONS"
case "$*" in
    "status --json")
        if [ "${FAKE_EXIT_NODE:-1}" = 1 ]; then
            printf '{"ExitNodeStatus":{"ID":"nXPA5k1Aw811CNTRL","Online":true}}\n'
        else
            printf '{"ExitNodeStatus":null}\n'
        fi
        ;;
esac
EOF

cat >"$BIN/systemctl" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    "is-active --quiet tailscaled") exit "${FAKE_TS_INACTIVE:-0}" ;;
    "show -P ActiveEnterTimestampMonotonic tailscaled")
        # /proc/uptime is real, so anchor the fake start relative to it.
        awk -v age="${FAKE_TS_UPTIME:-3600}" \
            '{printf "%d\n", ($1 - age) * 1000000}' /proc/uptime
        ;;
    *) printf 'systemctl %s\n' "$*" >>"$ACTIONS" ;;
esac
EOF

cat >"$BIN/logger" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF

# A fake clock keeps the bounded windows instant *and* deterministic: sleeping advances time, so
# each window yields a fixed number of probes instead of spinning against the real clock.
cat >"$BIN/sleep" <<'EOF'
#!/usr/bin/env bash
now=$(cat "$SANDBOX/clock" 2>/dev/null || echo 0)
printf '%s\n' "$((now + ${1%.*}))" >"$SANDBOX/clock"
EOF

cat >"$BIN/date" <<'EOF'
#!/usr/bin/env bash
[ "${1:-}" = '+%s' ] || exec /usr/bin/date "$@"
cat "$SANDBOX/clock" 2>/dev/null || echo 0
EOF

chmod +x "$BIN"/*

run() {
    : >"$ACTIONS"
    rm -f "$SANDBOX/probe-count"
    printf '0\n' >"$SANDBOX/clock"
    env PATH="$BIN:$PATH" ACTIONS="$ACTIONS" SANDBOX="$SANDBOX" \
        NETWORK_EXIT_WATCHDOG_LOCKED=1 \
        STATE_DIR_OVERRIDE="$STATE" \
        LOCK_OVERRIDE="$SANDBOX/lock" \
        FAKE_BSSID="${FAKE_BSSID:-bb:bb:bb:bb:bb:bb}" \
        FAKE_DEFAULT_ROUTE="$DEFAULT_ROUTE" \
        "$@" bash "$SCRIPT" >/dev/null 2>&1
}

status() { cat "$STATE/status" 2>/dev/null; }
recovery_actions() { grep -E 'debug rebind|restart tailscaled' "$ACTIONS" || true; }

# Record a fingerprint for BSSID aa:… with wlan0 only, so a default run reads as a link change.
reset_state() {
    rm -rf "$STATE"; mkdir -p "$STATE"
    printf '%s\n' "$WLAN_ADDR" >"$ADDRS"
    {
        printf 'bssid %s\n' "${1:-aa:aa:aa:aa:aa:aa}"
        printf 'default %s\n' "$DEFAULT_ROUTE"
        printf 'addr wlan0 10.36.50.129/20\n'
    } >"$STATE/linkstate"
}

head_ "only a real link-state change does anything"
reset_state bb:bb:bb:bb:bb:bb
run
if [ -z "$(recovery_actions)" ] && [ -z "$(status)" ]; then
    ok "an unchanged fingerprint is a no-op"
else
    bad "acted without a change: $(recovery_actions) / $(status)"
fi
reset_state
run FAKE_HAS_DEFAULT=0
[ -z "$(recovery_actions)" ] && ok "no physical default route means nothing to check" \
                             || bad "acted without a physical default route"

head_ "every input tailscaled keys on is a trigger"
reset_state
run
[ "$(recovery_actions)" = 'tailscale debug rebind' ] && ok "a BSSID change (roam) rebinds" \
                                                     || bad "roam did not rebind"
# Regression for 2026-09-14 18:53: restarting the iOA GUI removed tun0 with no roam at all, and
# the exit-node path wedged. A BSSID-only trigger missed it entirely.
reset_state bb:bb:bb:bb:bb:bb
printf '%s\n%s\n' "$WLAN_ADDR" "$TUN_ADDR" >"$ADDRS"
run
[ "$(recovery_actions)" = 'tailscale debug rebind' ] \
    && ok "tun0 appearing rebinds even though the BSSID is unchanged" \
    || bad "a tun0 flap with no roam was missed: $(recovery_actions)"
reset_state bb:bb:bb:bb:bb:bb
printf '%s\n' "$WLAN_ADDR" >"$ADDRS"
{ printf 'bssid bb:bb:bb:bb:bb:bb\n'; printf 'default %s\n' "$DEFAULT_ROUTE"
  printf 'addr tun0 192.168.255.10/24\naddr wlan0 10.36.50.129/20\n'; } >"$STATE/linkstate"
run
[ "$(recovery_actions)" = 'tailscale debug rebind' ] \
    && ok "tun0 disappearing rebinds even though the BSSID is unchanged" \
    || bad "tun0 removal was missed: $(recovery_actions)"
reset_state bb:bb:bb:bb:bb:bb
DEFAULT_ROUTE='default via 10.36.48.1 dev wlan0 proto dhcp src 10.36.50.129 metric 1024'
run
[ "$(recovery_actions)" = 'tailscale debug rebind' ] \
    && ok "a changed physical default route rebinds" || bad "default route change was missed"
DEFAULT_ROUTE='default via 10.36.48.1 dev wlan0 proto dhcp src 10.36.50.129 metric 600'

head_ "a link change tells tailscaled to rebind, then stops"
reset_state
run
[ "$(status)" = 'recovered after link change: rebind' ] \
    && ok "a healthy tunnel gets a rebind and nothing more" || bad "status is: $(status)"
reset_state
run FAKE_TS_INACTIVE=1
[ -z "$(recovery_actions)" ] && ok "inactive tailscaled is left alone" \
                             || bad "acted while tailscaled was inactive"
reset_state
run FAKE_EXIT_NODE=0 FAKE_PROBES=000
[ "$(recovery_actions)" = 'tailscale debug rebind' ] \
    && ok "with no exit node engaged it rebinds but never escalates" \
    || bad "escalated with no exit node engaged: $(recovery_actions)"

head_ "faults that are not the tunnel never restart it"
reset_state
run FAKE_GATEWAY_DOWN=1 FAKE_GATEWAY_GONE=1 FAKE_PROBES=000
if ! grep -Fq 'restart tailscaled' "$ACTIONS" && [[ "$(status)" == *"physical fault"* ]]; then
    ok "an unresolvable gateway is reported as a physical fault"
else
    bad "blamed the tunnel for a LAN fault: $(status) / $(recovery_actions)"
fi
# This network's gateway drops ICMP echo while forwarding normally; ping alone would suppress
# every escalation here, so ARP evidence has to override it.
reset_state
run FAKE_GATEWAY_DOWN=1 FAKE_PROBES=000
if grep -Fq 'restart tailscaled' "$ACTIONS" && [[ "$(status)" != *"physical fault"* ]]; then
    ok "a gateway that drops ICMP but answers ARP still allows escalation"
else
    bad "an ICMP-dropping gateway suppressed escalation: $(status)"
fi
reset_state
run FAKE_ROUTE_TUNNEL=0 FAKE_PROBES=000
! grep -Fq 'restart tailscaled' "$ACTIONS" \
    && ok "a probe routed off-tunnel never restarts tailscaled" \
    || bad "restarted although the probe bypassed the tunnel"

head_ "bounded escalation ends in a single restart"
reset_state
run FAKE_PROBES='000 000 000 204'
grep -Fq 'systemctl restart tailscaled' "$ACTIONS" \
    && ok "a path still dead after rebind restarts tailscaled" \
    || bad "expected a restart, got: $(recovery_actions)"
[ "$(status)" = 'recovered after link change: tailscaled restart' ] \
    && ok "status records recovery by restart" || bad "status is: $(status)"
reset_state
run FAKE_PROBES=000
if [ "$(grep -Fc 'systemctl restart tailscaled' "$ACTIONS")" -eq 1 ] &&
   [[ "$(status)" == *"failing closed"* ]]; then
    ok "an unrecoverable path restarts once, then fails closed"
else
    bad "expected one restart then fail-closed: $(status) / $(recovery_actions)"
fi
reset_state
run FAKE_PROBES=000 FAKE_TS_UPTIME=10
if ! grep -Fq 'restart tailscaled' "$ACTIONS" && [[ "$(status)" == *"restart withheld"* ]]; then
    ok "systemd's own start timestamp withholds a repeat restart"
else
    bad "restarted a tailscaled that just started: $(status) / $(recovery_actions)"
fi

head_ "no action ever installs a bypass"
if grep -E 'ip (rule|route) (add|replace|del)' "$SCRIPT" >/dev/null; then
    bad "the watchdog mutates routing state"
else
    ok "the watchdog never mutates routes or rules"
fi

printf '\n\033[1m%d passed, %d failed\033[0m\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
