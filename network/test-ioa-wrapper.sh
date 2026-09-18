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

# Exercise the root-only daemon path as a function so this fixture itself never needs root.
TEST_WORK="$WORK" IOA_IP_BIN="$WORK/ip" IOA_RECONFIGURE="$WORK/reconfigure" \
    IOA_CLIENT_BIN="$WORK/client" IOA_RETRY_SECONDS=0 \
    bash -c 'source "$1"; shift; run_daemon "$@"' _ "$WRAPPER" -flag value

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
    IOA_CLIENT_BIN="$WORK/client" IOA_RETRY_SECONDS=0 \
    bash -c 'source "$1"; run_daemon' _ "$WRAPPER"; then
    echo 'FAIL iOA wrapper succeeded after network reconfiguration failed' >&2
    exit 1
fi
[ ! -e "$WORK/order" ] || {
    echo 'FAIL iOA wrapper started the client after network reconfiguration failed' >&2
    exit 1
}

echo 'OK   iOA stays stopped when network reconfiguration fails'

# The GUI calls this same entrypoint as the desktop user when the daemon is absent. That path may
# inspect daemon state but must never invoke the root-only reconciler or start another daemon.
rm -f "$WORK/order"
TEST_WORK="$WORK" bash -c '
    source "$1"
    daemon_active() { return 0; }
    run_daemon() { printf "invalid\n" >>"$TEST_WORK/order"; }
    main
' _ "$WRAPPER"
[ ! -e "$WORK/order" ] || {
    echo 'FAIL unprivileged iOA invocation entered the root daemon path' >&2
    exit 1
}
echo 'OK   desktop iOA invocation never enters root network reconciliation'

grep -Fxq \
    'ExecCondition=/usr/bin/systemctl is-active --quiet ngnclient.service' \
    "$ROOT/systemd/ioagui.service" || {
    echo 'FAIL ioagui can start before the root-owned daemon is active' >&2
    exit 1
}
echo 'OK   iOA GUI waits for the system daemon without starting it'

grep -Fq 'network/iOA' "$ROOT/restore.sh" || {
    echo 'FAIL restore does not install the iOA wrapper' >&2
    exit 1
}
grep -Fq 'network/SmartGateAgent' "$ROOT/restore.sh" || {
    echo 'FAIL restore does not install the SmartGateAgent underlay wrapper' >&2
    exit 1
}

echo 'OK   restore installs both iOA entrypoint wrappers'
