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
    local manifest=$1 label
    local -a labels=(
        'tcpdump wlan0'
        'tcpdump tailscale0'
        'iw event'
        'ip monitor'
        'kernel journal follow'
        'service journal follow'
        'conntrack events'
        'nft monitor'
        'link/ARP timeline'
    )
    for label in "${labels[@]}"; do
        grep -Eq "^0[[:space:]]+${label}( \(duration complete\))?[[:space:]]+capture$" "$manifest" ||
            fail "capture missing successful stream: $label"
    done
}

capture_manifest_mutation_contract() (
    local manifest="$WORK/capture-manifest-mutated.tsv"
    printf '124\ttcpdump wlan0\tcapture\n124\ttcpdump tailscale0\tcapture\n' >"$manifest"
    if (assert_capture_success "$manifest") >/dev/null 2>&1; then
        fail 'capture contract accepted incomplete streams and timeout rc 124'
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

snapshot_command_contract() {
    local script=$1 mode=${2:-static} command
    # shellcheck disable=SC2016 # Assert literal script source, not test variables.
    local -a commands=(
        'record_command "$incident" "$phase" ip-link-stats 5 /usr/bin/ip -s link'
        'record_command "$incident" "$phase" ip-link-xdp 5 /usr/bin/ip -details link show dev wlan0'
        'record_command "$incident" "$phase" tc-qdisc 5 /usr/bin/tc -s qdisc show dev wlan0'
        'record_command "$incident" "$phase" tc-filter-ingress 5 /usr/bin/tc -s filter show dev wlan0 ingress'
        'record_command "$incident" "$phase" tc-filter-egress 5 /usr/bin/tc -s filter show dev wlan0 egress'
        'record_command "$incident" "$phase" ethtool-stats 5 /usr/bin/ethtool -S wlan0'
        'record_command "$incident" "$phase" iw-power-save 5 /usr/bin/iw dev wlan0 get power_save'
        'record_command "$incident" "$phase" iwlwifi-parameters 5 /usr/bin/sh -c'
        'record_command "$incident" "$phase" ip-neigh-wlan 5 /usr/bin/ip -4 neigh show dev wlan0'
        'record_command "$incident" "$phase" networkctl-wlan 10 /usr/bin/networkctl status wlan0 --no-pager'
        'record_command "$incident" "$phase" iw-station 5 /usr/bin/iw dev wlan0 station dump'
        'record_command "$incident" "$phase" route-get-internet 5 /usr/bin/ip -4 route get 1.1.1.1 mark 0x80000'
        'record_command "$incident" "$phase" journal-kernel-network 15'
        'record_command "$incident" "$phase" conntrack-tcp 10 /usr/bin/conntrack -L -p tcp'
        'record_command "$incident" "$phase" ss-tcp 5 /usr/bin/ss -tapne'
        'record_command "$incident" "$phase" ss-udp 5 /usr/bin/ss -uapne'
        'record_command "$incident" "$phase" tailscale-goroutines 15 /usr/bin/tailscale debug daemon-goroutines'
        'record_command "$incident" "$phase" proc-net-snmp 5 /usr/bin/cat /proc/net/snmp'
        'record_command "$incident" "$phase" proc-net-netstat 5 /usr/bin/cat /proc/net/netstat'
        'record_command "$incident" "$phase" nstat 5 /usr/bin/nstat -asz'
        'record_route_events "$incident" "$phase"'
    )
    if [ "$mode" = static ]; then
        commands+=(
            'record_command "$incident" "$phase" probe-gateway-arp 8 /usr/bin/arping -c 3 -w 5 -I wlan0 "$gateway"'
            'record_command "$incident" "$phase" probe-gateway-icmp 8 /usr/bin/ping -n -c 3 -W 1 -I wlan0 "$gateway"'
            'record_command "$incident" "$phase" probe-internet-icmp 8 /usr/bin/ping -n -c 3 -W 1 -m 524288 -I wlan0 1.1.1.1'
            'record_command "$incident" "$phase" probe-internet-http 10 /usr/bin/curl'
        )
    fi
    for command in "${commands[@]}"; do
        assert_contains "$script" "$command"
    done
}

snapshot_command_mutation_contract() (
    local command mutated="$WORK/network-debug-capture-snapshot-mutated"
    # shellcheck disable=SC2016 # Remove literal script source, not test variables.
    local -a commands=(
        'record_command "$incident" "$phase" ip-link-stats 5 /usr/bin/ip -s link'
        'record_command "$incident" "$phase" ip-link-xdp 5 /usr/bin/ip -details link show dev wlan0'
        'record_command "$incident" "$phase" tc-qdisc 5 /usr/bin/tc -s qdisc show dev wlan0'
        'record_command "$incident" "$phase" tc-filter-ingress 5 /usr/bin/tc -s filter show dev wlan0 ingress'
        'record_command "$incident" "$phase" tc-filter-egress 5 /usr/bin/tc -s filter show dev wlan0 egress'
        'record_command "$incident" "$phase" ethtool-stats 5 /usr/bin/ethtool -S wlan0'
        'record_command "$incident" "$phase" iw-power-save 5 /usr/bin/iw dev wlan0 get power_save'
        'record_command "$incident" "$phase" iwlwifi-parameters 5 /usr/bin/sh -c'
        'record_command "$incident" "$phase" ip-neigh-wlan 5 /usr/bin/ip -4 neigh show dev wlan0'
        'record_command "$incident" "$phase" networkctl-wlan 10 /usr/bin/networkctl status wlan0 --no-pager'
        'record_command "$incident" "$phase" iw-station 5 /usr/bin/iw dev wlan0 station dump'
        'record_command "$incident" "$phase" probe-gateway-arp 8 /usr/bin/arping -c 3 -w 5 -I wlan0 "$gateway"'
        'record_command "$incident" "$phase" probe-gateway-icmp 8 /usr/bin/ping -n -c 3 -W 1 -I wlan0 "$gateway"'
        'record_command "$incident" "$phase" probe-internet-icmp 8 /usr/bin/ping -n -c 3 -W 1 -m 524288 -I wlan0 1.1.1.1'
        'record_command "$incident" "$phase" probe-internet-http 10 /usr/bin/curl'
        'record_command "$incident" "$phase" conntrack-tcp 10 /usr/bin/conntrack -L -p tcp'
        'record_command "$incident" "$phase" ss-tcp 5 /usr/bin/ss -tapne'
        'record_command "$incident" "$phase" ss-udp 5 /usr/bin/ss -uapne'
        'record_command "$incident" "$phase" tailscale-goroutines 15 /usr/bin/tailscale debug daemon-goroutines'
        'record_command "$incident" "$phase" proc-net-snmp 5 /usr/bin/cat /proc/net/snmp'
        'record_command "$incident" "$phase" proc-net-netstat 5 /usr/bin/cat /proc/net/netstat'
        'record_command "$incident" "$phase" nstat 5 /usr/bin/nstat -asz'
        'record_route_events "$incident" "$phase"'
    )
    for command in "${commands[@]}"; do
        cp "$SCRIPT" "$mutated"
        grep -Fv -- "$command" "$mutated" >"$mutated.tmp"
        mv "$mutated.tmp" "$mutated"
        if (snapshot_command_contract "$mutated" static) >/dev/null 2>&1; then
            fail "snapshot contract accepted missing command: $command"
        fi
    done
)

snapshot_runtime_contract() (
    local log="$WORK/snapshot-commands"
    # shellcheck source=scripts/network-debug-capture
    source "$SCRIPT"
    : >"$log"
    # Capture run_snapshot's argv without executing host diagnostics.
    record_command() {
        local incident=$1 phase=$2 label=$3 limit=$4
        shift 4
        {
            # shellcheck disable=SC2016 # Emit literal runtime-contract source.
            printf 'record_command "$incident" "$phase" %s %s' "$label" "$limit"
            printf ' %q' "$@"
            printf '\n'
        } >>"$log"
    }
    record_route_events() {
        # shellcheck disable=SC2016 # Emit literal runtime-contract source.
        printf 'record_route_events "$incident" "$phase"\n' >>"$log"
    }
    diagnostic_commands=()
    run_snapshot "$WORK/snapshot" before
    snapshot_command_contract "$log" runtime
)

route_event_summary_contract() (
    set -euo pipefail
    local input="$WORK/route-events-input" output="$WORK/route-events-output"
    # shellcheck source=scripts/network-debug-capture
    source "$SCRIPT"

    cat >"$input" <<'EOF'
Jan 01 00:00:01 host tailscaled[1]: monitor: RTM_NEWROUTE first-route
Jan 01 00:00:02 host network-reconfigure[2]: first-reconfigure
Jan 01 00:00:03 host ignored[3]: unrelated
Jan 01 00:00:04 host tailscaled[1]: monitor: RTM_DELROUTE last-route
Jan 01 00:00:05 host network-reconfigure[2]: last-reconfigure
EOF
    summarize_route_events <"$input" >"$output"
    assert_contains "$output" 'route count=2'
    assert_contains "$output" 'route first=Jan 01 00:00:01 host tailscaled[1]: monitor: RTM_NEWROUTE first-route'
    assert_contains "$output" 'route last=Jan 01 00:00:04 host tailscaled[1]: monitor: RTM_DELROUTE last-route'
    assert_contains "$output" 'reconfigure count=2'
    assert_contains "$output" 'reconfigure first=Jan 01 00:00:02 host network-reconfigure[2]: first-reconfigure'
    assert_contains "$output" 'reconfigure last=Jan 01 00:00:05 host network-reconfigure[2]: last-reconfigure'

    awk 'BEGIN { for (i = 1; i <= 6000; i++) printf "event-%04d monitor: RTM_NEWROUTE\n", i }' |
        summarize_route_events >"$output"
    ! grep -Fxq 'event-0001 monitor: RTM_NEWROUTE' "$output" ||
        fail 'route timeline retained the oldest event outside the first-event header'
    assert_contains "$output" 'event-6000 monitor: RTM_NEWROUTE'

    awk 'BEGIN {
        payload = sprintf("%02000d", 0)
        for (i = 1; i <= 600; i++) printf "long-%04d monitor: RTM_NEWROUTE %s\n", i, payload
        print "last-event monitor: RTM_NEWROUTE complete"
    }' | summarize_route_events >"$output"
    [ "$(stat -c %s "$output")" -lt 1048576 ] || fail 'route timeline exceeded 1 MiB'
    [ "$(tail -1 "$output")" = 'last-event monitor: RTM_NEWROUTE complete' ] ||
        fail 'route timeline did not retain the complete newest event'
)

record_route_events_contract() (
    set -euo pipefail
    local route_incident="$WORK/route-incident" fake="$WORK/fake-journalctl"
    # shellcheck source=scripts/network-debug-capture
    source "$SCRIPT"
    mkdir -p "$route_incident"
    cat >"$fake" <<'EOF'
#!/usr/bin/env bash
printf 'fake monitor: RTM_NEWROUTE newest-route\n'
exit "${FAKE_JOURNALCTL_RC:-0}"
EOF
    chmod +x "$fake"
    TIMEOUT=/usr/bin/timeout

    record_route_events "$route_incident" before "$fake"
    assert_contains "$route_incident/before/route-events.txt" 'fake monitor: RTM_NEWROUTE newest-route'
    assert_contains "$route_incident/manifest.tsv" $'0\troute-events\tbefore'
    assert_not_contains "$route_incident/manifest.tsv" 'truncated=true'

    rm -rf "$route_incident"; mkdir "$route_incident"
    FAKE_JOURNALCTL_RC=7 record_route_events "$route_incident" after "$fake"
    assert_contains "$route_incident/manifest.tsv" $'7\troute-events\tafter'
)

static_script_contract() {
    [ -x "$SCRIPT" ] || fail "missing executable script: $SCRIPT"
    # shellcheck disable=SC2016 # assert literal source, not this test's positional parameters
    assert_contains "$SCRIPT" 'exec sudo -n -- "$0" "$@"'
    # shellcheck disable=SC2016 # Assert literal shell source.
    assert_contains "$SCRIPT" 'capture_main /var/log/network-debug/incidents "$duration" "$bugreport"'
    # shellcheck disable=SC2016 # Assert literal shell source.
    assert_contains "$SCRIPT" '/usr/bin/journalctl -b -u "$entry" --since=-15min -n 5000 --no-pager'
    assert_contains "$SCRIPT" "'arp or icmp or udp port 53 or udp port 67 or udp port 68 or udp port 3478 or udp port 41641 or tcp port 53 or tcp port 80 or tcp port 443'"
    assert_contains "$SCRIPT" "'icmp or udp port 53 or udp port 3478 or tcp port 53 or tcp port 80 or tcp port 443'"
    assert_contains "$SCRIPT" 'MAX_TEXT_BYTES=1048576'
    assert_contains "$SCRIPT" 'monitor: RTM_(NEW|DEL)ROUTE'
    assert_contains "$SCRIPT" '/usr/bin/iw event -t'
    assert_contains "$SCRIPT" '/usr/bin/ip -ts monitor all'
    assert_contains "$SCRIPT" '/usr/bin/conntrack -E -o timestamp,extended'
    assert_contains "$SCRIPT" '/usr/bin/nft monitor'
    assert_contains "$SCRIPT" 'sample_link_state "$incident" "$duration" &'
    snapshot_command_contract "$SCRIPT"
    assert_not_contains "$SCRIPT" 'network-debug-pcap.service|freeze_ring|RING_DIR|restore_recorder'
    assert_not_contains "$SCRIPT" '^[[:space:]]*(/usr/bin/)?tailscale (down|up|set)( |$)'
    assert_not_contains "$SCRIPT" 'systemctl (restart|stop|start) (tailscaled|systemd-networkd|network-reconfigure|smartdns|ngnclient)'

    [ ! -e "$SERVICE" ] || fail 'constant recorder unit remains in the repository'
    grep -Fq 'sudo systemctl disable --now network-debug-pcap.service' "$ROOT/restore.sh" ||
        fail 'restore does not retire the legacy recorder'
    grep -Fq 'sudo rm -rf /var/log/network-debug/ring' "$ROOT/restore.sh" ||
        fail 'restore does not remove the legacy packet ring'
    ! grep -E 'sudo systemctl (enable|restart) network-debug-pcap' "$ROOT/restore.sh" >/dev/null ||
        fail 'restore still enables the constant recorder'
    ! grep -Fq '/var/log/network-debug/incidents' "$ROOT/restore.sh" ||
        fail 'restore deletes retained incidents'
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
command=$1
if [ "${command##*/}" = diag ]; then
    "$@"
    exit $?
fi
if [ "${command##*/}" != tcpdump ]; then
    printf 'stream %s\n' "$*" >>"$NETWORK_DEBUG_TEST_LOG"
    [ "${NETWORK_DEBUG_TIMEOUT_STREAM_RC:-124}" -eq 0 ] || exit "$NETWORK_DEBUG_TIMEOUT_STREAM_RC"
    exit 0
fi
"$@"
rc=$?
if [ "${NETWORK_DEBUG_TIMEOUT_TCPDUMP_RC:-0}" -ne 0 ]; then
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
printf 'pcap\n' >"$out"
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
    sample_link_state() {
        printf 'timeline\n' >"$1/link-state-timeline.txt"
        printf 'now\tgateway=10.0.0.1\tarp=ok\n' >"$1/gateway-arp-timeline.tsv"
    }

    NETWORK_DEBUG_TIMEOUT_TCPDUMP_RC=124; export NETWORK_DEBUG_TIMEOUT_TCPDUMP_RC
    capture_main "$base" 0 false "$lock" \
        $'safe note\nwith\tcontrols\033[31m'
    unset NETWORK_DEBUG_TIMEOUT_TCPDUMP_RC

    mapfile -t incidents < <(find "$base" -mindepth 1 -maxdepth 1 -type d -printf '%f\n')
    [ "${#incidents[@]}" -eq 1 ] || fail 'capture did not create exactly one incident'
    local incident="$base/${incidents[0]}"
    [ ! -e "$incident/ring" ] || fail 'manual capture unexpectedly copied a packet ring'
    [ -f "$incident/wlan0.pcap" ] || fail 'bounded wlan capture missing'
    [ -f "$incident/tailscale0.pcap" ] || fail 'bounded tailscale capture missing'
    [ -f "$incident/iw-events.txt" ] || fail 'iw event capture missing'
    [ -f "$incident/ip-monitor.txt" ] || fail 'ip monitor capture missing'
    [ -f "$incident/journal-kernel-follow.txt" ] || fail 'kernel journal capture missing'
    [ -f "$incident/journal-services-follow.txt" ] || fail 'service journal capture missing'
    [ -f "$incident/gateway-arp-timeline.tsv" ] || fail 'gateway ARP timeline missing'
    assert_capture_success "$incident/manifest.tsv"
    local failed_capture="$WORK/failed-capture"
    mkdir "$failed_capture"
    (exit 125) &
    deep_pids=("$!")
    deep_labels=('tcpdump wlan0')
    (exit 1) &
    deep_pids+=("$!")
    deep_labels+=('tcpdump tailscale0')
    wait_deep "$failed_capture"
    assert_contains "$failed_capture/manifest.tsv" $'125\ttcpdump wlan0\tcapture'
    assert_contains "$failed_capture/manifest.tsv" $'1\ttcpdump tailscale0\tcapture'
    assert_contains "$incident/manifest.tsv" $'0\tworking command\tbefore'
    assert_contains "$incident/manifest.tsv" $'1\tfailing command\tbefore'
    assert_contains "$incident/manifest.tsv" $'truncated=true\tlarge command\tbefore'
    [ "$(stat -c %s "$incident/before/large_command.txt")" -le 1048576 ] ||
        fail 'diagnostic output exceeded 1 MiB'
    assert_not_contains "$incident/note.txt" '[[:cntrl:]]'
    assert_contains "$log" 'tcpdump-start wlan0 -p -i wlan0 -s 256 -n -U -C 16 -W 1 -w'
    assert_contains "$log" 'arp or icmp or udp port 53 or udp port 67 or udp port 68 or udp port 3478 or udp port 41641 or tcp port 53 or tcp port 80 or tcp port 443'
    assert_contains "$log" 'tcpdump-start tailscale0 -p -i tailscale0 -s 256 -n -U -C 16 -W 1 -w'
    assert_contains "$log" 'icmp or udp port 53 or udp port 3478 or tcp port 53 or tcp port 80 or tcp port 443'
    local before_done wlan_start tail_start capture_done after_start
    before_done=$(grep -n 'diag-start ok' "$log" | head -1 | cut -d: -f1)
    wlan_start=$(grep -n 'tcpdump-start wlan0 ' "$log" | head -1 | cut -d: -f1)
    tail_start=$(grep -n 'tcpdump-start tailscale0 ' "$log" | head -1 | cut -d: -f1)
    capture_done=$(grep -n 'tcpdump-done' "$log" | tail -1 | cut -d: -f1)
    after_start=$(grep -n 'diag-start ok' "$log" | tail -1 | cut -d: -f1)
    [ "$before_done" -lt "$wlan_start" ] && [ "$before_done" -lt "$tail_start" ] &&
        [ "$capture_done" -lt "$after_start" ] || fail 'snapshot/deep capture order is wrong'
    assert_not_contains "$log" 'bugreport'

    # The lock covers incident creation, so a concurrent capture leaves no partial incident.
    local before_count
    before_count=$(find "$base" -mindepth 1 -maxdepth 1 -type d | wc -l)
    exec {held_fd}>"$lock"
    flock -n "$held_fd"
    if capture_main "$base" 0 false "$lock" concurrent \
            >"$WORK/locked.out" 2>&1; then
        fail 'concurrent capture acquired a held lock'
    fi
    assert_contains "$WORK/locked.out" 'FAILED: another network debug capture is running'
    [ "$(find "$base" -mindepth 1 -maxdepth 1 -type d | wc -l)" -eq "$before_count" ] ||
        fail 'lock failure created an incident'
    flock -u "$held_fd"

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

/usr/bin/tcpdump --version >/dev/null
/usr/bin/tcpdump -d 'udp port 3478 or udp port 41641' >/dev/null
/usr/bin/tcpdump -d 'arp or icmp or udp port 53 or udp port 67 or udp port 68 or udp port 3478 or udp port 41641 or tcp port 53 or tcp port 80 or tcp port 443' >/dev/null
/usr/bin/tcpdump -d 'icmp or udp port 53 or udp port 3478 or tcp port 53 or tcp port 80 or tcp port 443' >/dev/null
static_script_contract
route_event_summary_contract
record_route_events_contract
snapshot_runtime_contract
snapshot_command_mutation_contract
capture_manifest_mutation_contract
runtime_contract
cli_contract
printf 'OK   network debug recorder contract\n'
