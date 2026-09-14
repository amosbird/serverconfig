#!/usr/bin/env bash
# Fixture test for the package-authoritative iOA installer.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
PKG="$WORK/pkg"
SRC="$PKG/usr/lib/iOA/bin"
LIVE="$WORK/live"
OPT="$WORK/opt/ioa"
mkdir -p "$SRC" "$LIVE/.real" "$OPT/bin"

for file in conf.yaml ifconfig2 iOA iOADiagnosticTool iOALinux iOAStandard iOA_upgrade \
    SmartGateAgent wpa.sh; do
    printf 'package-62:%s\n' "$file" >"$SRC/$file"
done
printf old >"$LIVE/iOA.bin.40"
printf old >"$LIVE/SmartGateAgent.bin.39"
printf shim >"$LIVE/iOALinux.bin"
printf duplicate >"$OPT/bin/iOALinux"

IOA_PACKAGE_ROOT="$PKG" IOA_LIVE_DIR_OVERRIDE="$LIVE" IOA_OPT_ROOT_OVERRIDE="$OPT" \
    "$ROOT/network/install-ioa-62" >/dev/null

cmp -s "$SRC/iOA" "$LIVE/iOA.bin.62"
cmp -s "$SRC/SmartGateAgent" "$LIVE/SmartGateAgent.bin.62"
cmp -s "$SRC/iOALinux" "$LIVE/iOALinux"
cmp -s "$SRC/iOA_upgrade" "$LIVE/iOA_upgrade"
[ "$(readlink "$LIVE/iOA.bin")" = iOA.bin.62 ]
[ "$(readlink "$LIVE/SmartGateAgent.bin")" = SmartGateAgent.bin.62 ]
[ ! -e "$LIVE/iOA.bin.40" ]
[ ! -e "$LIVE/SmartGateAgent.bin.39" ]
[ ! -e "$LIVE/iOALinux.bin" ]
[ ! -e "$OPT/bin" ]

# A second run must be a true content no-op.
inode=$(stat -c %i "$LIVE/iOALinux")
IOA_PACKAGE_ROOT="$PKG" IOA_LIVE_DIR_OVERRIDE="$LIVE" IOA_OPT_ROOT_OVERRIDE="$OPT" \
    "$ROOT/network/install-ioa-62" >/dev/null
[ "$(stat -c %i "$LIVE/iOALinux")" = "$inode" ]

echo "iOA 1.0.3.62 installer fixture: PASS"
