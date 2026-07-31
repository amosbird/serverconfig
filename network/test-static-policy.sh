#!/usr/bin/env bash
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"

fail=0
reject() {
    local description="$1" pattern="$2"
    shift 2
    if grep -RInE "$pattern" "$@" >/tmp/network-static.$$ 2>/dev/null; then
        printf 'FAIL %s\n' "$description" >&2
        cat /tmp/network-static.$$ >&2
        fail=1
    else
        printf 'OK   %s\n' "$description"
    fi
    rm -f /tmp/network-static.$$
}

[ ! -e scripts/network-fallback ] || { echo 'FAIL network-fallback script remains'; fail=1; }
[ ! -e network/systemd/network-fallback.service ] || {
    echo 'FAIL network-fallback unit remains'; fail=1;
}
reject 'no fallback deployment references' \
    'cp .*network-fallback|systemctl enable( --now)? .*network-fallback' \
    restore.sh network/migrate.sh
if ! awk '
    /sudo systemctl disable --now network-fallback\.service/ { disabled = NR }
    /sudo rm -f \/etc\/systemd\/system\/network-fallback\.service/ { unit_removed = NR }
    /sudo rm -rf \/var\/lib\/network-fallback/ { state_removed = NR }
    /sudo systemctl daemon-reload/ { reload = NR }
    END {
        exit !(disabled && disabled < unit_removed && unit_removed < state_removed &&
               state_removed < reload)
    }
' restore.sh; then
    echo 'FAIL restore does not safely retire the installed fallback before daemon-reload' >&2
    fail=1
fi
reject 'restore leaves Tailscale alone' 'tailscale|tailscaled' restore.sh
reject 'no exit-node mutation in local tools' \
    'tailscale (set --exit-node=|down|up)|systemctl restart tailscaled' \
    scripts/network-reconfigure scripts/netfix scripts/network-status
netfix_runtime_contract() {
    local sandbox fakebin log candidate output rc command
    sandbox=$(mktemp -d)
    fakebin="$sandbox/bin"
    log="$sandbox/calls"
    mkdir -p "$fakebin"

    if ! grep -Fq 'RECONFIGURE=${RECONFIGURE:-' scripts/netfix ||
       ! grep -Fq 'NETFIX_TEST:-0' scripts/netfix; then
        echo 'FAIL netfix lacks safe runtime-test injection points' >&2
        rm -rf "$sandbox"
        return 1
    fi

    cat >"$fakebin/fake-command" <<'EOF'
#!/usr/bin/env bash
command=${0##*/}
printf '%s' "$command" >>"$NETFIX_COMMAND_LOG"
case "$command" in
    jq) printf '|%s\n' "$1" >>"$NETFIX_COMMAND_LOG" ;;
    *) printf '|%s' "$@" >>"$NETFIX_COMMAND_LOG"; printf '\n' >>"$NETFIX_COMMAND_LOG" ;;
esac
case "$command" in
    ip)
        case "$*" in
            '-4 route show table main default') echo 'default via 192.0.2.1 dev wlan0' ;;
            'route show table cn')
                i=0
                while [ "$i" -lt 1001 ]; do echo '192.0.2.0/24 dev wlan0'; i=$((i + 1)); done
                ;;
            '-4 rule show pref 500') echo '500: from all fwmark 0x80000/0xff0000 lookup main' ;;
            '-4 rule show pref 1500') echo '1500: from all lookup cn' ;;
            '-4 rule show pref 2500')
                echo '2500: from all fwmark 0x1 lookup ioa'
                echo '2500: from all to 10.0.0.0/8 lookup ioa'
                echo '2500: from all to 100.12.0.0/16 lookup ioa'
                ;;
            '-4 rule show pref 3000') echo '3000: from all to 100.64.0.0/10 lookup 52' ;;
        esac
        ;;
    curl) printf '204' ;;
    dig) echo '142.250.72.14' ;;
    tailscale) echo '{"Health":[]}' ;;
    jq) echo '       exit node: none' ;;
    timeout) shift; "$@" ;;
esac
EOF
    chmod +x "$fakebin/fake-command"
    for command in ip iptables ipset systemctl tailscale curl timeout jq dig; do
        ln -s fake-command "$fakebin/$command"
    done
    cat >"$fakebin/reconfigure" <<'EOF'
#!/usr/bin/env bash
printf 'reconfigure|FORCE=%s|%s\n' "${FORCE-}" "$*" >>"$NETFIX_COMMAND_LOG"
EOF
    chmod +x "$fakebin/reconfigure"

    run_contract() {
        : >"$log"
        output=$(NETFIX_TEST=1 NETFIX_COMMAND_LOG="$log" RECONFIGURE="$fakebin/reconfigure" \
            PATH="$fakebin:/usr/bin:/bin" bash "$1" 2>&1)
        rc=$?
        if [ "$rc" -ne 0 ]; then
            printf 'runtime candidate failed (%s):\n%s\n' "$rc" "$output" >&2
            return 1
        fi
        while IFS= read -r command; do
            case "$command" in
                "timeout|120|env|FORCE=1|$fakebin/reconfigure|wlan0"| \
                'reconfigure|FORCE=1|wlan0'| \
                'ip|-4|route|show|table|main|default'| \
                'ip|route|show|table|cn'| \
                'ip|-4|rule|show|pref|500'| \
                'ip|-4|rule|show|pref|1500'| \
                'ip|-4|rule|show|pref|2500'| \
                'ip|-4|rule|show|pref|3000'| \
                'curl|-s|-o|/dev/null|-m|8|-w|%{http_code}|--resolve|connectivitycheck.gstatic.com:80:216.239.32.117|http://connectivitycheck.gstatic.com/generate_204'| \
                'curl|-s|-o|/dev/null|-m|10|-w|%{http_code}|http://ioa.tencent.com'| \
                'dig|+short|+timeout=2|+tries=1|google.com|@127.0.0.1'| \
                'tailscale|status|--json'| \
                'jq|-r') ;;
                *) printf 'disallowed runtime command: %s\n' "$command" >&2; return 1 ;;
            esac
        done <"$log"
        [ "$(grep -Fxc 'reconfigure|FORCE=1|wlan0' "$log")" -eq 1 ] || {
            echo 'runtime contract requires exactly one FORCE=1 reconfigure' >&2
            return 1
        }
    }

    if ! run_contract scripts/netfix; then
        echo 'FAIL netfix runtime command contract failed' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   netfix runtime commands are read-only except one FORCE=1 reconfigure'

    candidate="$sandbox/netfix-with-mutation"
    awk '/^say "RESULT"/ { print "iptables --flush" } { print }' scripts/netfix >"$candidate"
    chmod +x "$candidate"
    if run_contract "$candidate" >/dev/null 2>&1; then
        echo 'FAIL netfix runtime contract accepted an injected mutation' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   netfix runtime contract rejects an injected mutation'
    rm -rf "$sandbox"
}

if ! netfix_runtime_contract; then
    fail=1
fi

status_fake=$(mktemp -d)
printf '#!/usr/bin/env bash\nexit 0\n' >"$status_fake/ip"
cat >"$status_fake/tailscale" <<'EOF'
#!/usr/bin/env bash
case "$TAILSCALE_TEST" in
    failed) exit 1 ;;
    empty) exit 0 ;;
    malformed) printf 'not JSON\n' ;;
esac
EOF
chmod +x "$status_fake/ip" "$status_fake/tailscale"
for mode in failed empty malformed; do
    status_output=$(TAILSCALE_TEST="$mode" PATH="$status_fake:$PATH" bash scripts/network-status)
    status_rc=$?
    if [ "$status_rc" -eq 0 ] && grep -Fq 'tailscaled not responding' <<<"$status_output"; then
        printf 'OK   network-status reports %s tailscale JSON and remains read-only\n' "$mode"
    else
        printf 'FAIL network-status hides %s tailscale JSON or returns nonzero\n' "$mode" >&2
        fail=1
    fi
done
rm -rf "$status_fake"
reject 'operator tools do not inspect DERP or IOA endpoint caches' \
    'DERP_CACHE|derp-ips|IOA_ENDPOINT_CACHE|ioa-endpoints|IOA_BOOTSTRAP_HOSTS' \
    scripts/netfix scripts/network-status
reject 'no underlay tables' 'UNDERLAY_TABLE|UNDERLAY_STAGE|lookup underlay|table 50[01]' \
    scripts/network-reconfigure scripts/netfix scripts/network-status
reject 'no broad private bypass' \
    'PRIVATE_NETS|192\.168\.0\.0/16.*lookup main|172\.16\.0\.0/12.*lookup main|169\.254\.0\.0/16.*lookup main' \
    scripts/network-reconfigure
reject 'no static 9/8 IOA rule' 'IOA_STATIC_CIDRS=.*9\.0\.0\.0/8' \
    scripts/network-reconfigure
reject 'install is non-destructive' \
    'cleanup_obsolete_policy|systemctl (disable|stop)|ip (rule del|route flush)|iptables .*-[DFX]' \
    <(sed -n '/^install_configs()/,/^}/p' network/migrate.sh)
reject 'table 501 is never flushed automatically' 'ip route flush table 501' network/migrate.sh
reject 'rollback does not delete whole preference bands' \
    'for p in 500 1000 1500 2500 3000|while ip rule del pref' network/migrate.sh
reject 'migration does not mutate tunnel-owned policy' \
    'ip route (flush|del|replace).*table (20|230|52|ioa)|tailscale (set|up|down)' \
    network/migrate.sh
if ! grep -Fq 'cleanup_owned_rules' network/migrate.sh; then
    echo 'FAIL rollback does not use owned rule shapes' >&2
    fail=1
fi
if ! grep -Fq "sed -i '/^[[:space:]]*500[[:space:]]\\+underlay" network/migrate.sh; then
    echo 'FAIL obsolete underlay mapping is not removed' >&2
    fail=1
fi

# Exercise rollback against rules rendered by the installed kernel/iproute2. Run every
# mutation in a verified network namespace so this test cannot touch live policy.
host_netns_link=$(readlink /proc/self/ns/net) || exit 1
host_netns_inode=$(stat -Lc %i /proc/self/ns/net) || exit 1
exec 9</proc/self/ns/net
# shellcheck disable=SC2016 # inner script expands only inside the namespace
if unshare -rn bash -c '
    set -euo pipefail
    migrate=$1
    expected_link=$2
    expected_inode=$3
    host_fd_link=$(readlink /proc/self/fd/9)
    host_fd_inode=$(stat -Lc %i /proc/self/fd/9)
    current_link=$(readlink /proc/self/ns/net)
    current_inode=$(stat -Lc %i /proc/self/ns/net)
    if [ "$host_fd_link" != "$expected_link" ] ||
       [ "$host_fd_inode" != "$expected_inode" ] ||
       [ "$current_link" = "$expected_link" ] ||
       [ "$current_inode" = "$expected_inode" ]; then
        echo "refusing to mutate rules without a verified network namespace" >&2
        exit 1
    fi

    ip rule add pref 2500 fwmark 0x1/0xffffffff lookup ioa
    ip rule add pref 2500 fwmark 0x1/0x1 lookup ioa
    ip rule add pref 2500 from 203.0.113.8 lookup ioa
    source "$migrate"
    cleanup_owned_rules

    rules=$(ip -4 rule show pref 2500)
    expected=$(printf "2500:\tfrom all fwmark 0x1/0x1 lookup ioa\n"\
"2500:\tfrom 203.0.113.8 lookup ioa")
    [ "$rules" = "$expected" ]
' bash "$ROOT/network/migrate.sh" "$host_netns_link" "$host_netns_inode"; then
    echo 'OK   rollback normalizes kernel-rendered mark masks and preserves foreign rules'
else
    echo 'FAIL rollback mishandles kernel-rendered mark masks or foreign rules' >&2
    fail=1
fi

exit "$fail"
