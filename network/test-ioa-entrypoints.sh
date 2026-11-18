#!/usr/bin/env bash
# The vendor entry points are plain symlinks to the versioned ELFs (restore.sh). Two iOA processes
# look for SmartGateAgent in two different ways, and a symlink has to satisfy both:
#
#   iOA.bin uses `killall SmartGateAgent`, which matches /proc/<pid>/comm: the basename of the path
#     that was executed, not of the file it resolves to.
#   iOALinux scans /proc/*/cmdline and only counts the agent when argv[0] is its full launch path,
#     /usr/lib/iOA/bin/SmartGateAgent. Otherwise it logs `No SmartGateAgent Runing` every 5s and
#     re-sends the agent mode, which makes the agent rebuild its routes each time.
#
# The test launches a copy of `sleep` through the same link layout, the way iOA.bin does, and reads
# what /proc shows. It also checks that restore.sh installs exactly that layout.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
pid=
cleanup() { [ -z "$pid" ] || kill "$pid" 2>/dev/null || true; rm -rf "$WORK"; }
trap cleanup EXIT

fail() { echo "FAIL $*" >&2; exit 1; }
ok() { echo "OK   $*"; }

grep -Fxq '    sudo ln -sfn iOA.bin /usr/lib/iOA/bin/iOA' "$ROOT/restore.sh" ||
    fail 'restore.sh does not point /usr/lib/iOA/bin/iOA at iOA.bin'
grep -Fxq '    sudo ln -sfn SmartGateAgent.bin /usr/lib/iOA/bin/SmartGateAgent' "$ROOT/restore.sh" ||
    fail 'restore.sh does not point /usr/lib/iOA/bin/SmartGateAgent at SmartGateAgent.bin'
ok 'restore.sh links the vendor names to the versioned ELFs'

mkdir -p "$WORK/bin"
cp "$(readlink -f "$(command -v sleep)")" "$WORK/bin/SmartGateAgent.bin.9.9.9.9"
ln -s SmartGateAgent.bin.9.9.9.9 "$WORK/bin/SmartGateAgent.bin"
ln -s SmartGateAgent.bin "$WORK/bin/SmartGateAgent"

# iOA.bin: `/usr/lib/iOA/bin/SmartGateAgent -logpath=... &`, i.e. the full path as argv[0].
"$WORK/bin/SmartGateAgent" 30 &
pid=$!
for _ in $(seq 50); do
    [ "$(cat /proc/$pid/comm 2>/dev/null)" = SmartGateAgent ] && break
    sleep 0.1
done

[ "$(cat /proc/$pid/comm)" = SmartGateAgent ] ||
    fail "comm is '$(cat /proc/$pid/comm)'; killall SmartGateAgent would not find it"
ok 'comm is SmartGateAgent, so iOA.bin finds it with killall'
mapfile -d '' argv </proc/$pid/cmdline
[ "${argv[0]}" = "$WORK/bin/SmartGateAgent" ] ||
    fail "argv[0] is '${argv[0]}'; iOALinux only accepts the launch path"
ok 'argv[0] is the full launch path, so iOALinux finds it in /proc/*/cmdline'
