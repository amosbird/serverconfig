#!/usr/bin/env bash
# The SmartGateAgent wrapper must leave the agent looking exactly as the vendor launches it, because
# two different iOA processes look for it in two different ways:
#
#   iOA.bin (the daemon) uses `killall SmartGateAgent`, which matches /proc/<pid>/comm. comm is the
#     basename of the executed file, so the ELF must be executed through a file named exactly
#     `SmartGateAgent`.
#   iOALinux (the 1.0.3.88 GUI) scans every /proc/<pid>/cmdline and only counts the agent as running
#     when argv[0] is the full path it launches, /usr/lib/iOA/bin/SmartGateAgent. If argv[0] is the
#     bare name, it logs `No SmartGateAgent Runing` every 5s. Each time it re-sends the agent3 mode
#     to the agent, which tears down and rebuilds tables 20/230 and the tun0 default.
#
# The test runs the real wrapper with a fake agent ELF, a copy of `sleep`, and reads what /proc shows.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WRAPPER=${1:-$ROOT/network/SmartGateAgent}
WORK=$(mktemp -d)
pid=
cleanup() { [ -z "$pid" ] || kill "$pid" 2>/dev/null || true; rm -rf "$WORK"; }
trap cleanup EXIT

fail() { echo "FAIL $*" >&2; exit 1; }

mkdir -p "$WORK/bin"
cp "$(readlink -f "$(command -v sleep)")" "$WORK/bin/SmartGateAgent.bin.9.9.9.9"
ln -s SmartGateAgent.bin.9.9.9.9 "$WORK/bin/SmartGateAgent.bin"
cat >"$WORK/ip" <<'EOF'
#!/usr/bin/env bash
echo 'default via 192.0.2.1 dev wlan0 proto dhcp src 192.0.2.2 metric 600'
EOF
chmod +x "$WORK/ip"

SMARTGATE_IP_BIN="$WORK/ip" SMARTGATE_BINDIR="$WORK/bin" \
    SMARTGATE_AGENT_BIN="$WORK/bin/SmartGateAgent.bin" SMARTGATE_LOCK_FILE="$WORK/lock" \
    SMARTGATE_ARGV0="$WORK/bin/SmartGateAgent" \
    bash "$WRAPPER" 30 &
pid=$!
for _ in $(seq 50); do
    [ "$(cat /proc/$pid/comm 2>/dev/null)" = SmartGateAgent ] && break
    sleep 0.1
done

comm=$(cat /proc/$pid/comm)
mapfile -d '' argv </proc/$pid/cmdline
cwd=$(readlink /proc/$pid/cwd)

[ "$comm" = SmartGateAgent ] || fail "comm is '$comm'; killall SmartGateAgent would not find it"
echo 'OK   comm is SmartGateAgent, so iOA.bin finds it with killall'
[ "${argv[0]}" = "$WORK/bin/SmartGateAgent" ] ||
    fail "argv[0] is '${argv[0]}'; iOALinux only accepts the launch path"
echo 'OK   argv[0] is the full launch path, so iOALinux finds it in /proc/*/cmdline'
[ "${argv[*]:1}" = 30 ] || fail "arguments changed: ${argv[*]:1}"
echo 'OK   arguments are passed through unchanged'
[ "$cwd" = "$WORK/bin" ] || fail "cwd is '$cwd'; conf.yaml and assets resolve from it"
echo 'OK   cwd is the bin directory'

# The production default must be the real launch path, not a test override.
grep -Fxq 'argv0=${SMARTGATE_ARGV0:-/usr/lib/iOA/bin/SmartGateAgent}' "$WRAPPER" ||
    fail 'production argv[0] is not /usr/lib/iOA/bin/SmartGateAgent'
echo 'OK   production argv[0] is /usr/lib/iOA/bin/SmartGateAgent'
