#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
LOG="$WORK/ip.log"
FAKE="$WORK/ip"

cat > "$FAKE" <<'FAKE'
#!/usr/bin/env bash
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

expect 'tun default is isolated and has no gateway' \
    'route add default dev tun0 table ioa metric 101 ' \
    route add default via 192.168.255.1 dev tun0
expect 'SmartGateAgent mark rule passes through' \
    'rule add fwmark 2616 table 20 ' \
    rule add fwmark 2616 table 20
expect 'SmartGateAgent mark delete passes through' \
    'rule del fwmark 2616 table 20 ' \
    rule del fwmark 2616 table 20
expect 'SmartGateAgent source rule passes through' \
    'rule add from 192.0.2.10 table 230 ' \
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
