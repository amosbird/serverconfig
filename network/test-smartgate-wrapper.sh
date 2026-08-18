#!/usr/bin/env bash
set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WRAPPER="$ROOT/network/SmartGateAgent"
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
cat >"$WORK/ip-never" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
cat >"$WORK/agent" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" >"$TEST_WORK/args"
EOF
chmod +x "$WORK/ip" "$WORK/ip-never" "$WORK/agent"

TEST_WORK="$WORK" SMARTGATE_IP_BIN="$WORK/ip" \
    SMARTGATE_AGENT_BIN="$WORK/agent" SMARTGATE_RETRY_SECONDS=0 \
    SMARTGATE_LOCK_FILE="$WORK/initial.lock" \
    "$WRAPPER" -logpath=/tmp/log

[ "$(cat "$WORK/count")" -eq 2 ] || {
    echo 'FAIL wrapper did not wait for a physical main-table default route' >&2
    exit 1
}
mapfile -t args <"$WORK/args"
[ "${args[0]}" = -logpath=/tmp/log ] && [ "${#args[@]}" -eq 1 ] || {
    printf 'FAIL wrapper changed SmartGateAgent arguments: %s\n' "${args[*]}" >&2
    exit 1
}

echo 'OK   wrapper waits for the physical main-table default without changing arguments'

lock="$WORK/wrapper.lock"
TEST_WORK="$WORK" SMARTGATE_IP_BIN="$WORK/ip-never" \
    SMARTGATE_AGENT_BIN="$WORK/agent" SMARTGATE_RETRY_SECONDS=1 \
    SMARTGATE_LOCK_FILE="$lock" "$WRAPPER" &
waiter=$!
for _ in {1..50}; do
    [ -e "$lock" ] && break
    sleep 0.01
done
TEST_WORK="$WORK" SMARTGATE_IP_BIN="$WORK/ip-never" \
    SMARTGATE_AGENT_BIN="$WORK/agent" SMARTGATE_RETRY_SECONDS=0 \
    SMARTGATE_LOCK_FILE="$lock" "$WRAPPER"
second_rc=$?
kill "$waiter"
wait "$waiter" 2>/dev/null || true
for _ in {1..50}; do
    flock -n "$lock" true 2>/dev/null && break
    sleep 0.01
done
[ "$second_rc" -eq 0 ] || {
    echo "FAIL duplicate wrapper exited $second_rc" >&2
    exit 1
}

echo 'OK   duplicate wrapper exits while one instance waits for the network'

rm -f "$WORK/args"
TEST_WORK="$WORK" SMARTGATE_IP_BIN="$WORK/ip" \
    SMARTGATE_AGENT_BIN="$WORK/agent" SMARTGATE_RETRY_SECONDS=0 \
    SMARTGATE_LOCK_FILE="$lock" "$WRAPPER" -logpath=/tmp/restart
mapfile -t args <"$WORK/args"
[ "${args[0]}" = -logpath=/tmp/restart ] || {
    echo 'FAIL wrapper lock was not released after the waiting process exited' >&2
    exit 1
}

echo 'OK   wrapper lock is released for a later start'

grep -Fq 'network/SmartGateAgent' "$ROOT/restore.sh" || {
    echo 'FAIL restore does not install the SmartGateAgent wrapper' >&2
    exit 1
}

echo 'OK   restore installs the SmartGateAgent wrapper'
