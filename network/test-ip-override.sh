#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
LOG="$WORK/ip.log"
FAKE="$WORK/ip"

cat > "$FAKE" <<'FAKE'
#!/usr/bin/env bash
if [ "$*" = "-4 route show table 19 default" ]; then
    if [ "${TENCENT_TEST_UP:-0}" = 1 ]; then
        printf '%s\n' 'default via 10.76.76.193 dev enp9s0u2u1u2'
    fi
    exit 0
fi
if [ "$*" = "-4 route show table main default" ]; then
    printf '%s\n' \
        'default via 192.0.2.1 dev enp1s0 proto dhcp src 192.0.2.20 metric 100' \
        'default via 198.51.100.1 dev wlan0 proto dhcp src 198.51.100.20 metric 600'
    exit 0
fi
printf '%q ' "$@" >> "$IP_LOG"
printf '\n' >> "$IP_LOG"
FAKE
chmod +x "$FAKE"

run() {
    : > "$LOG"
    IP_REAL="$FAKE" IP_LOG="$LOG" "$ROOT/scripts/overrides/ip" "$@"
    cat "$LOG"
}

expect() {
    local name="$1" expected="$2"
    shift 2
    local actual
    actual=$(run "$@")
    if [ "$actual" != "$expected" ]; then
        printf 'FAIL %s\nexpected: %s\nactual:   %s\n' "$name" "$expected" "$actual" >&2
        return 1
    fi
    printf 'OK   %s\n' "$name"
}

expect_tencent() {
    local name="$1" expected="$2"
    shift 2
    local actual
    : > "$LOG"
    actual=$(TENCENT_TEST_UP=1 IP_REAL="$FAKE" IP_LOG="$LOG" \
        "$ROOT/scripts/overrides/ip" "$@"; cat "$LOG")
    if [ "$actual" != "$expected" ]; then
        printf 'FAIL %s\nexpected: %s\nactual:   %s\n' "$name" "$expected" "$actual" >&2
        return 1
    fi
    printf 'OK   %s\n' "$name"
}

run_status() {
    : > "$LOG"
    IP_REAL="$FAKE" IP_LOG="$LOG" "$ROOT/scripts/overrides/ip" "$@"
}

expect_failure() {
    local name="$1"
    shift
    if run_status "$@"; then
        printf 'FAIL %s\n' "$name" >&2
        return 1
    fi
    printf 'OK   %s\n' "$name"
}

expect 'tun default is isolated and has no gateway' \
    'route add default dev tun0 table ioa metric 101 ' \
    route add default via 192.168.255.1 dev tun0
expect 'table 20 default is bound to the current physical route' \
    'route add default via 192.0.2.1 dev enp1s0 table 20 ' \
    route add default via 192.0.2.1 table 20
expect 'table 230 default follows the requested active Wi-Fi route without Tencent Ethernet' \
    'route add default via 198.51.100.1 dev wlan0 table 230 ' \
    route add default via 198.51.100.1 table 230
expect_tencent 'Tencent Ethernet overrides SmartGate requested Wi-Fi underlay' \
    'route add default via 10.76.76.193 dev enp9s0u2u1u2 table 20 ' \
    route add default via 198.51.100.1 table 20
expect_failure 'non-default gateway fails instead of choosing a tunnel device' \
    route add default via 203.0.113.1 table 20
expect 'SmartGateAgent mark rule gets an explicit safe priority' \
    'rule add priority 1100 fwmark 2616 table 20 ' \
    rule add fwmark 2616 table 20
expect 'SmartGateAgent mark delete passes through' \
    'rule del fwmark 2616 table 20 ' \
    rule del fwmark 2616 table 20
expect 'SmartGateAgent source rule gets an explicit safe priority' \
    'rule add priority 1200 from 192.0.2.10 table 230 ' \
    rule add from 192.0.2.10 table 230
expect 'SmartGateAgent source delete passes through' \
    'rule del from 192.0.2.10 table 230 ' \
    rule del from 192.0.2.10 table 230
expect 'unknown rule is not swallowed' \
    'rule add to 198.51.100.0/24 table 77 ' \
    rule add to 198.51.100.0/24 table 77
expect 'table flush passes through' \
    'route flush table 230 ' \
    route flush table 230
