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

assert_capture_success() {
    local manifest=$1
    assert_contains "$manifest" $'0\ttcpdump wlan0 (duration complete)\tcapture'
    assert_contains "$manifest" $'0\ttcpdump tailscale0 (duration complete)\tcapture'
}

capture_manifest_mutation_contract() (
    local manifest="$WORK/capture-manifest-mutated.tsv"
    printf '124\ttcpdump wlan0\tcapture\n124\ttcpdump tailscale0\tcapture\n' >"$manifest"
    if (assert_capture_success "$manifest") >/dev/null 2>&1; then
        fail 'capture contract accepted timeout rc 124 as a failure'
    fi
)

assert_no_mutations() {
    local log=$1
    if grep -Ev '^(systemctl (is-active( --quiet)?|stop|start) network-debug-pcap\.service|tcpdump(-start|-done)? )' "$log" |
            grep -Eq '(systemctl|^ip |^iptables|^nft|^tailscale (down|up|set))'; then
        return 1
    fi
}

recorder_directory_contract() {
    local service=$1
    assert_contains "$service" 'LogsDirectory=network-debug'
    assert_contains "$service" 'LogsDirectoryMode=0700'
    assert_contains "$service" 'ReadWritePaths=/var/log/network-debug'
}

recorder_identity_contract() {
    local service=$1
    assert_contains "$service" 'User=root'
    # tcpdump already runs as root; -Z triggers a redundant libcap-ng identity change that is
    # incompatible with the unit's capability bounding and NoNewPrivileges restrictions.
    assert_not_contains "$service" '(^|[[:space:]])-Z([[:space:]]|$)'
}

service_contract() {
    [ -f "$SERVICE" ] || fail "missing service: $SERVICE"
    recorder_identity_contract "$SERVICE"
    assert_contains "$SERVICE" 'ExecStartPre=/usr/bin/install -d -o root -g root -m 0700 /var/log/network-debug/ring'
    assert_contains "$SERVICE" 'ExecStart=/usr/bin/tcpdump -p -i wlan0 -s 96 -n -U -w /var/log/network-debug/ring/trace.pcap -C 8 -W 8 udp port 3478 or udp port 41641'
    assert_contains "$SERVICE" 'CapabilityBoundingSet=CAP_NET_RAW'
    assert_contains "$SERVICE" 'AmbientCapabilities=CAP_NET_RAW'
    assert_not_contains "$SERVICE" 'CAP_NET_ADMIN'
    assert_contains "$SERVICE" 'RestrictAddressFamilies=AF_PACKET AF_INET AF_INET6 AF_NETLINK'
    assert_contains "$SERVICE" 'ProtectSystem=strict'
    recorder_directory_contract "$SERVICE"
    assert_contains "$SERVICE" 'ProtectHome=true'
    assert_contains "$SERVICE" 'PrivateTmp=true'
    assert_not_contains "$SERVICE" 'PrivateDevices=false'
    assert_contains "$SERVICE" 'UMask=0077'
    assert_contains "$SERVICE" 'Restart=always'
    assert_not_contains "$SERVICE" 'Exec(Start|StartPre)=.*/(sh|bash)( |$)'
}

service_identity_mutation_contract() (
    local mutated="$WORK/network-debug-pcap-identity-mutated.service"
    cp "$SERVICE" "$mutated"
    sed -i 's/ -w / -Z root -w /' "$mutated"
    # shortcut: static mutation reproduces the forbidden invocation without touching the live unit.
    if (recorder_identity_contract "$mutated") >/dev/null 2>&1; then
        fail 'service contract accepted tcpdump -Z identity change'
    fi
)

service_directory_mutation_contract() (
    local mutated="$WORK/network-debug-pcap-mutated.service"
    cp "$SERVICE" "$mutated"
    sed -i '/^LogsDirectory=/d' "$mutated"
    # shortcut: static mutation covers ordering without a systemd-capable test environment.
    if (recorder_directory_contract "$mutated") >/dev/null 2>&1; then
        fail 'service contract accepted missing LogsDirectory'
    fi
)

static_script_contract() {
    [ -x "$SCRIPT" ] || fail "missing executable script: $SCRIPT"
    # shellcheck disable=SC2016 # assert literal source, not this test's positional parameters
    assert_contains "$SCRIPT" 'exec sudo -n -- "$0" "$@"'
    # shellcheck disable=SC2016 # Assert literal shell source.
    assert_contains "$SCRIPT" 'capture_main /var/log/network-debug/incidents network-debug-pcap.service 30 "$bugreport"'
    # shellcheck disable=SC2016 # Assert literal shell source.
    assert_contains "$SCRIPT" '/usr/bin/journalctl -b -u "$entry" --since=-15min -n 5000 --no-pager'
    assert_not_contains "$SCRIPT" '^[[:space:]]*(/usr/bin/)?tailscale (down|up|set)( |$)'
    assert_not_contains "$SCRIPT" 'systemctl (restart|stop|start) (tailscaled|systemd-networkd|network-reconfigure|smartdns|ngnclient)'

    local check copy reload enable restart active
    check=$(grep -nF '[ -x /usr/bin/tcpdump ] || {' "$ROOT/restore.sh" | cut -d: -f1)
    copy=$(grep -nF 'network-debug-pcap.service /etc/systemd/system/' "$ROOT/restore.sh" | cut -d: -f1)
    reload=$(awk '/network-debug-pcap.service \/etc\/systemd\/system\// { seen=1 } seen && /systemctl daemon-reload/ { print NR; exit }' "$ROOT/restore.sh")
    enable=$(grep -nF 'sudo systemctl enable network-debug-pcap.service' "$ROOT/restore.sh" | cut -d: -f1)
    restart=$(grep -nF 'sudo systemctl restart network-debug-pcap.service' "$ROOT/restore.sh" | cut -d: -f1)
    active=$(grep -nF 'sudo systemctl is-active --quiet network-debug-pcap.service' "$ROOT/restore.sh" | cut -d: -f1)
    [ -n "$check" ] && [ "$check" -lt "$copy" ] || fail 'restore checks tcpdump after unit copy'
    [ "$copy" -lt "$reload" ] && [ "$reload" -lt "$enable" ] &&
        [ "$enable" -lt "$restart" ] && [ "$restart" -lt "$active" ] ||
        fail 'restore recorder deployment order is unsafe'
}

make_fakes() {
    local dir=$1
    mkdir -p "$dir"
    cat >"$dir/systemctl" <<'EOF'
#!/usr/bin/env bash
printf 'systemctl %s\n' "$*" >>"$NETWORK_DEBUG_TEST_LOG"
state=$(cat "$NETWORK_DEBUG_TEST_STATE")
case "${1-}" in
    is-active)
        [ "${NETWORK_DEBUG_ACTIVE_FAIL:-0}" = 0 ] || exit 2
        if [ -n "${NETWORK_DEBUG_STATE_SEQUENCE:-}" ] && [ -s "$NETWORK_DEBUG_STATE_SEQUENCE" ]; then
            state=$(head -1 "$NETWORK_DEBUG_STATE_SEQUENCE")
            sed -i '1d' "$NETWORK_DEBUG_STATE_SEQUENCE"
        fi
        printf '%s\n' "$state"
        [ "$state" = active ]
        ;;
    stop)
        [ "${NETWORK_DEBUG_STOP_FAIL:-0}" = 0 ] || exit 1
        printf 'inactive\n' >"$NETWORK_DEBUG_TEST_STATE"
        ;;
    start)
        [ "${NETWORK_DEBUG_START_FAIL:-0}" = 0 ] || exit 1
        printf 'active\n' >"$NETWORK_DEBUG_TEST_STATE"
        ;;
    *) exit 90 ;;
esac
EOF
    cat >"$dir/timeout" <<'EOF'
#!/usr/bin/env bash
while [[ ${1-} == --* ]]; do shift; done
shift
"$@"
rc=$?
if [ "${NETWORK_DEBUG_TIMEOUT_TCPDUMP_RC:-0}" -ne 0 ] && [ "${1##*/}" = tcpdump ]; then
    exit "$NETWORK_DEBUG_TIMEOUT_TCPDUMP_RC"
fi
exit "$rc"
EOF
    cat >"$dir/tcpdump" <<'EOF'
#!/usr/bin/env bash
interface=
for arg in "$@"; do
    [ "$arg" = wlan0 ] || [ "$arg" = tailscale0 ] || continue
    interface=$arg
    break
done
printf 'tcpdump-start %s %s\n' "$interface" "$*" >>"$NETWORK_DEBUG_TEST_LOG"
out=
while [ "$#" -gt 0 ]; do
    if [ "$1" = -w ]; then out=$2; shift 2; else shift; fi
done
printf 'pcap\n' >"${out}0"
printf 'tcpdump-done %s\n' "$interface" >>"$NETWORK_DEBUG_TEST_LOG"
EOF
    cat >"$dir/cp" <<'EOF'
#!/usr/bin/env bash
kill -TERM "$PPID"
sleep 1
EOF
    cat >"$dir/diag" <<'EOF'
#!/usr/bin/env bash
printf 'diag-start %s\n' "$*" >>"$NETWORK_DEBUG_TEST_LOG"
case "${1-}" in
    fail) exit 1 ;;
    huge) head -c 1500000 /dev/zero | tr '\0' x ;;
esac
printf 'diag-done %s\n' "$*" >>"$NETWORK_DEBUG_TEST_LOG"
EOF
    chmod +x "$dir"/*
}

load_script() {
    local fakebin=$1 ring=$2 log=$3 state_file=$4
    # shellcheck source=scripts/network-debug-capture
    source "$SCRIPT"
    SYSTEMCTL="$fakebin/systemctl"
    TIMEOUT="$fakebin/timeout"
    TCPDUMP="$fakebin/tcpdump"
    RING_DIR=$ring
    NETWORK_DEBUG_TEST_LOG=$log
    NETWORK_DEBUG_TEST_STATE=$state_file
    export NETWORK_DEBUG_TEST_LOG NETWORK_DEBUG_TEST_STATE
}

freeze_contract() (
    set -euo pipefail
    local fakebin="$WORK/freeze-bin" ring="$WORK/freeze-ring" log="$WORK/freeze-log"
    local state="$WORK/freeze-state" incident="$WORK/freeze-incident"
    make_fakes "$fakebin"
    mkdir -p "$ring" "$incident"
    printf ring >"$ring/trace.pcap0"
    : >"$log"
    printf active >"$state"
    load_script "$fakebin" "$ring" "$log" "$state"

    freeze_ring "$incident" network-debug-pcap.service
    [ "$(cat "$state")" = active ] || fail 'active recorder was not restored'
    assert_contains "$incident/manifest.tsv" $'0\trecorder stop\tfreeze'
    assert_contains "$incident/manifest.tsv" $'0\tring copy\tfreeze'
    assert_contains "$incident/manifest.tsv" $'0\trecorder start\tfreeze'
    assert_contains "$log" 'systemctl is-active network-debug-pcap.service'

    rm -rf "$incident"; mkdir "$incident"; : >"$log"; printf inactive >"$state"
    freeze_ring "$incident" network-debug-pcap.service
    assert_not_contains "$log" 'systemctl stop network-debug-pcap.service'
    assert_not_contains "$log" 'systemctl start network-debug-pcap.service'
    assert_contains "$incident/manifest.tsv" $'0\trecorder start skipped (initially inactive)\tfreeze'

    rm -rf "$incident"; mkdir "$incident"; : >"$log"; printf active >"$state"
    printf 'activating\nactive\n' >"$WORK/state-sequence"
    NETWORK_DEBUG_STATE_SEQUENCE="$WORK/state-sequence"; export NETWORK_DEBUG_STATE_SEQUENCE
    freeze_ring "$incident" network-debug-pcap.service
    assert_contains "$incident/manifest.tsv" $'0\trecorder initially active\tfreeze'
    unset NETWORK_DEBUG_STATE_SEQUENCE

    rm -rf "$incident"; mkdir "$incident"; : >"$log"; printf inactive >"$state"
    printf 'deactivating\ninactive\n' >"$WORK/state-sequence"
    NETWORK_DEBUG_STATE_SEQUENCE="$WORK/state-sequence"; export NETWORK_DEBUG_STATE_SEQUENCE
    freeze_ring "$incident" network-debug-pcap.service
    assert_contains "$incident/manifest.tsv" $'0\trecorder initially inactive\tfreeze'
    assert_not_contains "$log" 'systemctl start network-debug-pcap.service'
    unset NETWORK_DEBUG_STATE_SEQUENCE

    rm -rf "$incident"; mkdir "$incident"; : >"$log"; printf failed >"$state"
    freeze_ring "$incident" network-debug-pcap.service
    assert_contains "$incident/manifest.tsv" $'0\trecorder initially inactive\tfreeze'
    assert_not_contains "$log" 'systemctl start network-debug-pcap.service'

    rm -rf "$incident"; mkdir "$incident"; : >"$log"; printf active >"$state"
    NETWORK_DEBUG_ACTIVE_FAIL=1; export NETWORK_DEBUG_ACTIVE_FAIL
    if freeze_ring "$incident" network-debug-pcap.service; then fail 'state query failure was accepted'; fi
    assert_contains "$incident/manifest.tsv" $'1\trecorder initial state\tfreeze'
    assert_not_contains "$log" 'systemctl stop network-debug-pcap.service'
    unset NETWORK_DEBUG_ACTIVE_FAIL

    rm -rf "$incident"; mkdir "$incident"; : >"$log"; printf active >"$state"
    NETWORK_DEBUG_STOP_FAIL=1; export NETWORK_DEBUG_STOP_FAIL
    if freeze_ring "$incident" network-debug-pcap.service; then fail 'stop failure was accepted'; fi
    [ "$(cat "$state")" = active ] || fail 'stop failure changed active state'
    assert_contains "$incident/manifest.tsv" $'1\trecorder stop\tfreeze'
    unset NETWORK_DEBUG_STOP_FAIL

    rm -rf "$incident"; mkdir "$incident"; : >"$log"; printf active >"$state"
    RING_DIR="$WORK/missing-ring"
    if freeze_ring "$incident" network-debug-pcap.service; then fail 'copy failure was accepted'; fi
    [ "$(cat "$state")" = active ] || fail 'copy failure did not restore recorder'
    assert_contains "$incident/manifest.tsv" $'1\tring copy\tfreeze'
    assert_contains "$incident/manifest.tsv" $'0\trecorder start\tfreeze'

    rm -rf "$incident"; mkdir "$incident"; : >"$log"; printf active >"$state"
    RING_DIR=$ring
    NETWORK_DEBUG_START_FAIL=1; export NETWORK_DEBUG_START_FAIL
    if PATH="$fakebin:$PATH" freeze_ring "$incident" network-debug-pcap.service; then
        fail 'TERM during copy returned success'
    fi
    assert_contains "$log" 'systemctl start network-debug-pcap.service'
    assert_contains "$incident/manifest.tsv" $'1\trecorder start\tfreeze'
    unset NETWORK_DEBUG_START_FAIL

    rm -rf "$incident"; mkdir "$incident"; : >"$log"; printf active >"$state"
    RING_DIR=$ring
    NETWORK_DEBUG_START_FAIL=1; export NETWORK_DEBUG_START_FAIL
    if freeze_ring "$incident" network-debug-pcap.service; then fail 'start failure was accepted'; fi
    assert_contains "$incident/manifest.tsv" $'1\trecorder start\tfreeze'
    unset NETWORK_DEBUG_START_FAIL
)

runtime_contract() (
    set -euo pipefail
    local base="$WORK/incidents" ring="$WORK/ring" fakebin="$WORK/bin" log="$WORK/calls"
    local state="$WORK/state" lock="$WORK/capture.lock"
    mkdir -p "$base" "$ring"
    printf 'ring-zero\n' >"$ring/trace.pcap0"
    printf 'ring-one\n' >"$ring/trace.pcap1"
    : >"$log"; printf active >"$state"
    make_fakes "$fakebin"
    load_script "$fakebin" "$ring" "$log" "$state"
    diagnostic_commands=(
        "working command|5|$fakebin/diag|ok"
        "failing command|5|$fakebin/diag|fail"
        "large command|5|$fakebin/diag|huge"
    )

    NETWORK_DEBUG_TIMEOUT_TCPDUMP_RC=124; export NETWORK_DEBUG_TIMEOUT_TCPDUMP_RC
    capture_main "$base" network-debug-pcap.service 0 false "$lock" \
        $'safe note\nwith\tcontrols\033[31m'
    unset NETWORK_DEBUG_TIMEOUT_TCPDUMP_RC

    mapfile -t incidents < <(find "$base" -mindepth 1 -maxdepth 1 -type d -printf '%f\n')
    [ "${#incidents[@]}" -eq 1 ] || fail 'capture did not create exactly one incident'
    local incident="$base/${incidents[0]}"
    [ -f "$incident/ring/trace.pcap0" ] || fail 'ring file was not copied'
    [ -f "$incident/wlan0.pcap0" ] || fail 'bounded wlan capture missing'
    [ -f "$incident/tailscale0.pcap0" ] || fail 'bounded tailscale capture missing'
    assert_capture_success "$incident/manifest.tsv"
    local failed_capture="$WORK/failed-capture"
    mkdir "$failed_capture"
    (exit 125) & deep_wlan_pid=$!
    (exit 1) & deep_tail_pid=$!
    wait_deep "$failed_capture"
    assert_contains "$failed_capture/manifest.tsv" $'125\ttcpdump wlan0\tcapture'
    assert_contains "$failed_capture/manifest.tsv" $'1\ttcpdump tailscale0\tcapture'
    assert_contains "$incident/manifest.tsv" $'0\tworking command\tbefore'
    assert_contains "$incident/manifest.tsv" $'1\tfailing command\tbefore'
    assert_contains "$incident/manifest.tsv" $'truncated=true\tlarge command\tbefore'
    [ "$(stat -c %s "$incident/before/large_command.txt")" -le 1048576 ] ||
        fail 'diagnostic output exceeded 1 MiB'
    assert_not_contains "$incident/note.txt" '[[:cntrl:]]'
    assert_contains "$log" 'tcpdump-start wlan0 -p -i wlan0 -s 96 -n -C 8 -W 1 -w'
    assert_contains "$log" 'tcpdump-start tailscale0 -p -i tailscale0 -s 96 -n -C 8 -W 1 -w'
    local wlan_start tail_start first_before capture_done after_start
    wlan_start=$(grep -n 'tcpdump-start wlan0 ' "$log" | head -1 | cut -d: -f1)
    tail_start=$(grep -n 'tcpdump-start tailscale0 ' "$log" | head -1 | cut -d: -f1)
    first_before=$(grep -n 'diag-start ' "$log" | head -1 | cut -d: -f1)
    capture_done=$(grep -n 'tcpdump-done' "$log" | tail -1 | cut -d: -f1)
    after_start=$(grep -n 'diag-start ok' "$log" | tail -1 | cut -d: -f1)
    [ "$wlan_start" -lt "$first_before" ] && [ "$tail_start" -lt "$first_before" ] &&
        [ "$capture_done" -lt "$after_start" ] || fail 'deep capture/snapshot order is wrong'
    assert_not_contains "$log" 'bugreport'

    # The lock covers incident creation, so a concurrent capture leaves no partial incident.
    local before_count
    before_count=$(find "$base" -mindepth 1 -maxdepth 1 -type d | wc -l)
    exec {held_fd}>"$lock"
    flock -n "$held_fd"
    if capture_main "$base" network-debug-pcap.service 0 false "$lock" concurrent \
            >"$WORK/locked.out" 2>&1; then
        fail 'concurrent capture acquired a held lock'
    fi
    assert_contains "$WORK/locked.out" 'FAILED: another network debug capture is running'
    [ "$(find "$base" -mindepth 1 -maxdepth 1 -type d | wc -l)" -eq "$before_count" ] ||
        fail 'lock failure created an incident'
    flock -u "$held_fd"

    # Freeze failure preserves evidence, reports FAILED, exits nonzero, and never starts deep capture.
    printf active >"$state"; NETWORK_DEBUG_STOP_FAIL=1; export NETWORK_DEBUG_STOP_FAIL
    if capture_main "$base" network-debug-pcap.service 0 false "$lock" broken \
            >"$WORK/failed.out" 2>&1; then
        fail 'failed freeze returned success'
    fi
    assert_contains "$WORK/failed.out" 'FAILED:'
    local failed_incident
    failed_incident=$(find "$base" -mindepth 1 -maxdepth 1 -type d -printf '%T@ %p\n' |
        sort -nr | head -1 | cut -d' ' -f2-)
    assert_contains "$failed_incident/manifest.tsv" $'1\trecorder stop\tfreeze'
    [ ! -e "$failed_incident/wlan0.pcap0" ] || fail 'deep capture ran after freeze failure'
    unset NETWORK_DEBUG_STOP_FAIL

    # Retention remains bounded and does not delete non-directories.
    touch "$base/keep-file"
    for n in 1 2 3 4 5 6; do
        mkdir "$base/20000101T00000${n}Z.test"
        touch -d "2000-01-0$n 00:00:00" "$base/20000101T00000${n}Z.test"
    done
    prune_incidents "$base"
    [ "$(find "$base" -mindepth 1 -maxdepth 1 -type d | wc -l)" -eq 5 ] ||
        fail 'retention did not keep five incidents'
    [ -f "$base/keep-file" ] || fail 'retention deleted a non-directory'

    assert_no_mutations "$log" || fail 'unexpected network mutation logged'
    printf 'systemctl restart tailscaled\n' >>"$log"
    if assert_no_mutations "$log"; then fail 'mutation self-test accepted tailscaled restart'; fi
)

cli_contract() (
    local fake="$WORK/cli-bin" log="$WORK/sudo-log" cli_script="$WORK/network-debug-capture"
    chmod 0755 "$WORK"
    cp "$SCRIPT" "$cli_script"
    # Test the non-root branch without requiring a user namespace in restricted CI containers.
    # shellcheck disable=SC2016 # Match literal source in the copied script.
    sed -i 's/"${EUID:-$(id -u)}" -ne 0/1000 -ne 0/' "$cli_script"
    chmod 0755 "$cli_script"
    mkdir -p "$fake"
    chmod 0755 "$fake"
    cat >"$fake/id" <<'EOF'
#!/usr/bin/env bash
printf '1000\n'
EOF
    cat >"$fake/sudo" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >"$NETWORK_DEBUG_SUDO_LOG"
exit 42
EOF
    chmod +x "$fake"/*
    : >"$log"
    chmod 0666 "$log"
    if PATH="$fake:$PATH" NETWORK_DEBUG_SUDO_LOG="$log" \
            bash "$cli_script" note >/dev/null 2>&1; then
        fail 'sudo re-exec failure returned success'
    fi
    assert_contains "$log" '-n --'
    assert_contains "$log" "$cli_script note"
)

service_contract
service_identity_mutation_contract
service_directory_mutation_contract
/usr/bin/tcpdump --version >/dev/null
/usr/bin/tcpdump -d 'udp port 3478 or udp port 41641' >/dev/null
static_script_contract
capture_manifest_mutation_contract
freeze_contract
runtime_contract
cli_contract
printf 'OK   network debug recorder contract\n'
