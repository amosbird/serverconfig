#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SERVICE="$ROOT/network/systemd/network-debug-pcap.service"
SCRIPT="$ROOT/scripts/network-debug-capture"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

fail() {
    printf 'FAIL %s\n' "$*" >&2
    exit 1
}

assert_contains() {
    local file=$1 text=$2
    grep -Fq -- "$text" "$file" || fail "$file does not contain: $text"
}

assert_not_contains() {
    local file=$1 pattern=$2
    ! grep -Eq -- "$pattern" "$file" || fail "$file contains forbidden pattern: $pattern"
}

assert_no_mutations() {
    local log=$1
    if grep -Ev '^(systemctl (is-active --quiet|stop|start) network-debug-pcap\.service|tcpdump )' "$log" |
            grep -Eq '(systemctl|^ip |^iptables|^nft|^tailscale (down|up|set))'; then
        return 1
    fi
}

service_contract() {
    [ -f "$SERVICE" ] || fail "missing service: $SERVICE"
    assert_contains "$SERVICE" 'User=root'
    assert_contains "$SERVICE" 'ExecStartPre=/usr/bin/install -d -o root -g root -m 0700 /var/log/network-debug/ring'
    assert_contains "$SERVICE" 'ExecStart=/usr/bin/tcpdump -i wlan0 -s 96 -n -U -Z root -w /var/log/network-debug/ring/trace.pcap -C 8 -W 8 udp port 3478 or udp port 41641'
    assert_contains "$SERVICE" 'CapabilityBoundingSet=CAP_NET_RAW CAP_NET_ADMIN'
    assert_contains "$SERVICE" 'AmbientCapabilities=CAP_NET_RAW CAP_NET_ADMIN'
    assert_contains "$SERVICE" 'ProtectSystem=strict'
    assert_contains "$SERVICE" 'ReadWritePaths=/var/log/network-debug'
    assert_contains "$SERVICE" 'ProtectHome=true'
    assert_contains "$SERVICE" 'PrivateTmp=true'
    assert_contains "$SERVICE" 'UMask=0077'
    assert_contains "$SERVICE" 'Restart=always'
    assert_not_contains "$SERVICE" 'Exec(Start|StartPre)=.*/(sh|bash)( |$)'
}

static_script_contract() {
    [ -x "$SCRIPT" ] || fail "missing executable script: $SCRIPT"
    # shellcheck disable=SC2016 # assert literal source, not this test's positional parameters
    assert_contains "$SCRIPT" 'exec sudo -n -- "$0" "$@"'
    assert_contains "$SCRIPT" 'capture_main /var/log/network-debug/incidents network-debug-pcap.service 30'
    assert_not_contains "$SCRIPT" '^[[:space:]]*(/usr/bin/)?tailscale (down|up|set)( |$)'
    assert_not_contains "$SCRIPT" 'systemctl (restart|stop|start) (tailscaled|systemd-networkd|network-reconfigure|smartdns|ngnclient)'
    assert_contains "$ROOT/restore.sh" 'network-debug-pcap.service /etc/systemd/system/'
    assert_contains "$ROOT/restore.sh" '[ -x /usr/bin/tcpdump ] || {'
    assert_contains "$ROOT/restore.sh" 'sudo systemctl enable --now network-debug-pcap.service'
}

runtime_contract() (
    set -euo pipefail
    local base="$WORK/incidents" ring="$WORK/ring" fakebin="$WORK/bin" log="$WORK/calls"
    mkdir -p "$base" "$ring" "$fakebin"
    printf 'ring-zero\n' >"$ring/trace.pcap0"
    printf 'ring-one\n' >"$ring/trace.pcap1"
    : >"$log"

    cat >"$fakebin/systemctl" <<'EOF'
#!/usr/bin/env bash
printf 'systemctl %s\n' "$*" >>"$NETWORK_DEBUG_TEST_LOG"
case "${1-}" in
    is-active) exit 0 ;;
    stop|start) exit 0 ;;
    *) exit 90 ;;
esac
EOF
    cat >"$fakebin/timeout" <<'EOF'
#!/usr/bin/env bash
while [[ ${1-} == --* ]]; do shift; done
shift
exec "$@"
EOF
    cat >"$fakebin/tcpdump" <<'EOF'
#!/usr/bin/env bash
printf 'tcpdump %s\n' "$*" >>"$NETWORK_DEBUG_TEST_LOG"
out=
while [ "$#" -gt 0 ]; do
    if [ "$1" = -w ]; then out=$2; shift 2; else shift; fi
done
printf 'pcap\n' >"$out"
EOF
    cat >"$fakebin/diag" <<'EOF'
#!/usr/bin/env bash
printf 'diag %s\n' "$*" >>"$NETWORK_DEBUG_TEST_LOG"
[ "${1-}" != fail ]
EOF
    chmod +x "$fakebin"/*

    # shellcheck source=scripts/network-debug-capture
    source "$SCRIPT"
    SYSTEMCTL="$fakebin/systemctl"
    TIMEOUT="$fakebin/timeout"
    TCPDUMP="$fakebin/tcpdump"
    RING_DIR="$ring"
    NETWORK_DEBUG_TEST_LOG="$log"
    export NETWORK_DEBUG_TEST_LOG
    diagnostic_commands=(
        "working command|5|$fakebin/diag|ok"
        "failing command|5|$fakebin/diag|fail"
    )

    capture_main "$base" network-debug-pcap.service 0 false $'safe note\nwith\tcontrols\033[31m'

    mapfile -t incidents < <(find "$base" -mindepth 1 -maxdepth 1 -type d -printf '%f\n')
    [ "${#incidents[@]}" -eq 1 ] || fail 'capture did not create exactly one incident'
    local incident="$base/${incidents[0]}"
    [ -f "$incident/ring/trace.pcap0" ] || fail 'ring file was not copied'
    [ -f "$incident/wlan0.pcap" ] || fail 'wlan capture missing'
    [ -f "$incident/tailscale0.pcap" ] || fail 'tailscale capture missing'
    assert_contains "$incident/manifest.tsv" $'0\tworking command'
    assert_contains "$incident/manifest.tsv" $'1\tfailing command'
    assert_not_contains "$incident/note.txt" '[[:cntrl:]]'
    assert_contains "$log" 'systemctl is-active --quiet network-debug-pcap.service'
    assert_contains "$log" 'systemctl stop network-debug-pcap.service'
    assert_contains "$log" 'systemctl start network-debug-pcap.service'
    assert_contains "$log" 'tcpdump -i wlan0 -s 96 -n -w'
    assert_contains "$log" 'udp port 3478 or udp port 41641'
    assert_contains "$log" 'tcpdump -i tailscale0 -s 96 -n -w'

    # A second capture in the same second must not collide.
    capture_main "$base" network-debug-pcap.service 0 false second
    [ "$(find "$base" -mindepth 1 -maxdepth 1 -type d | wc -l)" -eq 2 ] ||
        fail 'same-second captures collided'

    # Retention is bounded to five directories and ignores non-directories.
    touch "$base/keep-file"
    for n in 1 2 3 4 5 6; do
        mkdir "$base/20000101T00000${n}Z.test"
        touch -d "2000-01-0$n 00:00:00" "$base/20000101T00000${n}Z.test"
    done
    prune_incidents "$base"
    [ "$(find "$base" -mindepth 1 -maxdepth 1 -type d | wc -l)" -eq 5 ] ||
        fail 'retention did not keep five incidents'
    [ -f "$base/keep-file" ] || fail 'retention deleted a non-directory'

    local modes
    modes=$(stat -c '%a' "$incident" "$incident/manifest.tsv" 2>/dev/null || true)
    assert_contains <(printf '%s\n' "$modes") '700'
    assert_contains <(printf '%s\n' "$modes") '600'

    # Only the recorder service and tcpdump may perform mutations.
    assert_no_mutations "$log" || fail 'unexpected network mutation logged'
    printf 'systemctl restart tailscaled\n' >>"$log"
    if assert_no_mutations "$log"; then
        fail 'mutation self-test did not reject a tailscaled restart'
    fi
)

service_contract
static_script_contract
runtime_contract
printf 'OK   network debug recorder contract\n'
