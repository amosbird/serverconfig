#!/usr/bin/env bash
set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WRAPPER="$ROOT/network/iOA"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cat >"$WORK/client" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" >"$TEST_WORK/args"
EOF
chmod +x "$WORK/client"

# Exercise the root-only daemon path as a function so this fixture itself never needs root.
TEST_WORK="$WORK" IOA_CLIENT_BIN="$WORK/client" \
    bash -c 'source "$1"; shift; run_daemon "$@"' _ "$WRAPPER" -flag value

mapfile -t args <"$WORK/args"
[ "${args[*]}" = '-flag value' ] || {
    printf 'FAIL iOA wrapper changed client arguments: %s\n' "${args[*]}" >&2
    exit 1
}
grep -Fxq '    exec -a firefox "$client" "$@"' "$WRAPPER" || {
    echo 'FAIL iOA wrapper no longer execs the client as firefox' >&2
    exit 1
}
echo 'OK   iOA starts the client with its arguments unchanged'

# The GUI calls this same entrypoint as the desktop user when the daemon is absent. That path may
# inspect daemon state but must never start another daemon.
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
echo 'OK   desktop iOA invocation never starts the root daemon'

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
    echo 'FAIL restore does not install the SmartGateAgent wrapper' >&2
    exit 1
}

echo 'OK   restore installs both iOA entrypoint wrappers'
