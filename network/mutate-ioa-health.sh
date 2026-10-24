#!/usr/bin/env bash
# Mutation harness for scripts/ioa-health: flip one decision at a time and require the fixture to
# notice. A suite that never fails is not evidence, and reading the code is not verification.
set -u
cd "$(dirname "$0")/.." || exit 1

base=$(mktemp)
cp scripts/ioa-health "$base"
trap 'cp "$base" scripts/ioa-health; rm -f "$base"' EXIT

caught=0
escaped=0
skipped=0

flip() {
    local name=$1 old=$2 new=$3
    cp "$base" scripts/ioa-health
    if ! python3 - "$old" "$new" <<'PY'
import sys
p = 'scripts/ioa-health'
s = open(p).read()
old, new = sys.argv[1], sys.argv[2]
if old not in s:
    raise SystemExit(1)
open(p, 'w').write(s.replace(old, new, 1))
PY
    then
        printf '  SKIP    %s (pattern not found)\n' "$name"
        skipped=$((skipped + 1))
        return
    fi
    if bash network/test-ioa-health.sh >/dev/null 2>&1; then
        printf '  ESCAPED %s\n' "$name"
        escaped=$((escaped + 1))
    else
        printf '  caught  %s\n' "$name"
        caught=$((caught + 1))
    fi
}

echo 'thresholds'
flip 'GRACE 600 -> 60'           'GRACE=${GRACE_OVERRIDE:-600}'        'GRACE=${GRACE_OVERRIDE:-60}'
flip 'GIVE_UP 2 -> 99'           'GIVE_UP=${GIVE_UP_OVERRIDE:-2}'      'GIVE_UP=${GIVE_UP_OVERRIDE:-99}'
flip 'FORGIVE 3600 -> 0'         'FORGIVE=${FORGIVE_OVERRIDE:-3600}'   'FORGIVE=${FORGIVE_OVERRIDE:-0}'
flip 'COOLDOWN 1800 -> 0'        'COOLDOWN=${COOLDOWN_OVERRIDE:-1800}' 'COOLDOWN=${COOLDOWN_OVERRIDE:-0}'
flip 'agent port 56101 -> 56100' 'AGENT_PORT=${AGENT_PORT_OVERRIDE:-56101}' 'AGENT_PORT=${AGENT_PORT_OVERRIDE:-56100}'

echo 'the channel test'
flip 'peer field 4 -> 3'          '$4 ~ pattern' '$3 ~ pattern'

echo 'the guards'
flip 'clock not cleared when down'   'if ! both_up; then' 'if ! true; then'
flip 'clock cleared unconditionally' 'if ! both_up; then' 'if true; then'
flip 'daemon not required'           'both_up() { gui_up && daemon_up; }' 'both_up() { gui_up; }'
flip 'gui not required'              'both_up() { gui_up && daemon_up; }' 'both_up() { daemon_up; }'
flip 'write guard removed'           'stamp "$held_stamp" || { log "cannot record $held_stamp"; exit 1; }' 'stamp "$held_stamp" || true'

echo 'the cooldown ordering'
flip 'clock guard removed' '[[ "$NOW_EPOCH" =~ ^[0-9]+$ ]] || { echo "ioa-health: no usable clock" >&2; exit 1; }' ': # no guard'
flip 'cooldown written only on success' 'stamp "$last_stamp" || true
if "$SYSTEMCTL" --user restart ioagui.service; then
    stamp "$held_stamp" || true' 'if "$SYSTEMCTL" --user restart ioagui.service; then
    stamp "$held_stamp" || true
    stamp "$last_stamp" || true'

printf '\ncaught %d  escaped %d  skipped %d\n' "$caught" "$escaped" "$skipped"
[ "$escaped" -eq 0 ]
