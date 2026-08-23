#!/usr/bin/env bash
set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WRAPPER="$ROOT/network/iOA"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cat >"$WORK/ip" <<'EOF'
#!/usr/bin/env bash
count=$(cat "$TEST_WORK/count" 2>/dev/null || printf 0)
printf '%s' "$((count + 1))" >"$TEST_WORK/count"
if [ "$count" -gt 0 ]; then
    echo 'default via 192.168.2.1 dev wlan0 proto dhcp src 192.168.2.161 metric 600'
fi
EOF
cat >"$WORK/reconfigure" <<'EOF'
#!/usr/bin/env bash
printf 'reconfigure\n' >>"$TEST_WORK/order"
EOF
cat >"$WORK/client" <<'EOF'
#!/usr/bin/env bash
printf 'client\n' >>"$TEST_WORK/order"
printf '%s\n' "$@" >"$TEST_WORK/args"
EOF
chmod +x "$WORK/ip" "$WORK/reconfigure" "$WORK/client"

TEST_WORK="$WORK" IOA_IP_BIN="$WORK/ip" IOA_RECONFIGURE="$WORK/reconfigure" \
    IOA_CLIENT_BIN="$WORK/client" IOA_RETRY_SECONDS=0 "$WRAPPER" -flag value

[ "$(cat "$WORK/count")" -eq 2 ] || {
    echo 'FAIL iOA wrapper did not wait for a physical main-table default route' >&2
    exit 1
}
[ "$(cat "$WORK/order")" = $'reconfigure\nclient' ] || {
    echo 'FAIL iOA wrapper did not reconcile policy before starting the client' >&2
    exit 1
}
mapfile -t args <"$WORK/args"
[ "${args[*]}" = '-flag value' ] || {
    printf 'FAIL iOA wrapper changed client arguments: %s\n' "${args[*]}" >&2
    exit 1
}

echo 'OK   iOA waits for physical networking and reconciles policy before starting'

cat >"$WORK/reconfigure-fails" <<'EOF'
#!/usr/bin/env bash
exit 1
EOF
chmod +x "$WORK/reconfigure-fails"
rm -f "$WORK/order"
if TEST_WORK="$WORK" IOA_IP_BIN="$WORK/ip" IOA_RECONFIGURE="$WORK/reconfigure-fails" \
    IOA_CLIENT_BIN="$WORK/client" IOA_RETRY_SECONDS=0 "$WRAPPER"; then
    echo 'FAIL iOA wrapper succeeded after network reconfiguration failed' >&2
    exit 1
fi
[ ! -e "$WORK/order" ] || {
    echo 'FAIL iOA wrapper started the client after network reconfiguration failed' >&2
    exit 1
}

echo 'OK   iOA stays stopped when network reconfiguration fails'

grep -Fq 'network/iOA' "$ROOT/restore.sh" || {
    echo 'FAIL restore does not install the iOA wrapper' >&2
    exit 1
}
! grep -Fq 'network/SmartGateAgent' "$ROOT/restore.sh" || {
    echo 'FAIL restore still installs the ineffective SmartGateAgent wrapper' >&2
    exit 1
}

echo 'OK   restore installs only the iOA entrypoint wrapper'
