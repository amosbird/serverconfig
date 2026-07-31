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

    if grep -Eq 'NETFIX_TEST|RECONFIGURE=\$\{RECONFIGURE' scripts/netfix; then
        echo 'FAIL netfix contains production test hooks' >&2
        rm -rf "$sandbox"
        return 1
    fi
    # shellcheck disable=SC2016 # literal source lines, not shell expressions
    if [ "$(grep -Fxc 'RECONFIGURE="$REPO/scripts/network-reconfigure"' scripts/netfix)" -ne 1 ] ||
       [ "$(grep -Fxc '    main "$RECONFIGURE"' scripts/netfix)" -ne 1 ]; then
        echo 'FAIL netfix CLI does not pass the fixed reconciler to main exactly once' >&2
        rm -rf "$sandbox"
        return 1
    fi
    # shortcut: the runtime allowlist covers current external calls; reject known absolute
    # sensitive paths statically and extend both lists when netfix gains a command.
    if grep -Eq '/usr/(s?bin)/(ip|iptables|ipset|nft|systemctl|tailscale)([[:space:]]|$)' \
        scripts/netfix; then
        echo 'FAIL netfix bypasses PATH for a sensitive command' >&2
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
    ln -s /usr/bin/bash "$fakebin/bash"
    for command in ip iptables ipset nft systemctl tailscale curl timeout jq dig; do
        ln -s fake-command "$fakebin/$command"
    done
    for command in awk grep wc head env date; do
        ln -s "/usr/bin/$command" "$fakebin/$command"
    done
    cat >"$fakebin/reconfigure" <<'EOF'
#!/usr/bin/env bash
printf 'reconfigure|FORCE=%s|%s\n' "${FORCE-}" "$*" >>"$NETFIX_COMMAND_LOG"
EOF
    chmod +x "$fakebin/reconfigure"

    run_contract() {
        : >"$log"
        # shellcheck disable=SC2016 # positional parameters belong to the inner shell
        output=$(env -i NETFIX_COMMAND_LOG="$log" PATH="$fakebin" /usr/bin/bash -c \
            'source "$1"; main "$2"' bash "$1" "$fakebin/reconfigure" 2>&1)
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
        echo 'FAIL netfix function runtime command contract failed' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   netfix function commands are read-only except one FORCE=1 reconfigure'

    candidate="$sandbox/netfix-with-mutation"
    awk '/^    say "RESULT"/ { print "    iptables --flush" } { print }' \
        scripts/netfix >"$candidate"
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

status_runtime_contract() {
    local sandbox fakebin log candidate output rc command mode mutation
    sandbox=$(mktemp -d)
    fakebin="$sandbox/bin"
    log="$sandbox/calls"
    mkdir -p "$fakebin"

    # shortcut: PATH interception covers the current sensitive tool set; extend it and the
    # allowlist together if network-status intentionally gains another read-only query tool.
    if grep -Eq '/usr/(s?bin)/(ip|iptables|ipset|nft|systemctl|tailscale)([[:space:]]|$)' \
        scripts/network-status; then
        echo 'FAIL network-status bypasses PATH for a sensitive command' >&2
        rm -rf "$sandbox"
        return 1
    fi

    cat >"$fakebin/sensitive-command" <<'EOF'
#!/usr/bin/env bash
command=${0##*/}
printf '%s' "$command" >>"$STATUS_COMMAND_LOG"
printf '|%s' "$@" >>"$STATUS_COMMAND_LOG"
printf '\n' >>"$STATUS_COMMAND_LOG"
case "$command|$*" in
    'ip|-4 -o addr show scope global') echo '2: wlan0 inet 192.0.2.10/24 scope global wlan0' ;;
    'ip|-4 route show table main default') echo 'default via 192.0.2.1 dev wlan0' ;;
    'ip|-4 route show table main scope link') echo '192.0.2.0/24 dev wlan0 scope link' ;;
    'ip|-4 rule show pref 1000') echo '1000: from all to 192.0.2.0/24 lookup main' ;;
    'ip|-4 route show table cn') echo '203.0.113.0/24 via 192.0.2.1 dev wlan0' ;;
    'ip|-4 rule show pref 1500') echo '1500: from all lookup cn' ;;
    'ip|-4 route show table ioa') echo '10.0.0.0/8 dev tun0' ;;
    'ip|-4 -o addr show tun0') echo '8: tun0 inet 198.51.100.2/24 scope global tun0' ;;
    'ip|-4 rule show pref 2500') echo '2500: from all to 10.0.0.0/8 lookup ioa' ;;
    'ip|-4 rule show') echo '490: from all fwmark 0xa38 lookup main' ;;
    'ip|-4 route show table 52') echo 'default dev tailscale0' ;;
    'ip|-4 rule show pref 3000') echo '3000: from all to 100.64.0.0/10 lookup 52' ;;
    'tailscale|status --json')
        case "${TAILSCALE_TEST-ok}" in
            failed) exit 1 ;;
            empty) exit 0 ;;
            malformed) printf 'not JSON\n' ;;
            *) printf '{"BackendState":"Running","Health":[]}\n' ;;
        esac
        ;;
esac
EOF
    chmod +x "$fakebin/sensitive-command"
    ln -s /usr/bin/bash "$fakebin/bash"
    for command in ip tailscale systemctl iptables ipset nft; do
        ln -s sensitive-command "$fakebin/$command"
    done
    for command in awk wc head sed tr; do
        ln -s "/usr/bin/$command" "$fakebin/$command"
    done
    cat >"$fakebin/jq" <<'EOF'
#!/usr/bin/env bash
IFS= read -r input || true
case "${TAILSCALE_TEST-ok}:$input" in
    failed:*|empty:*|malformed:*) exit 1 ;;
    ok:'{"BackendState":"Running","Health":[]}')
        printf '  backend       Running\n  exit node     none\n'
        ;;
    *) exit 1 ;;
esac
EOF
    chmod +x "$fakebin/jq"

    run_contract() {
        : >"$log"
        output=$(/usr/bin/env -i STATUS_COMMAND_LOG="$log" TAILSCALE_TEST="${mode-ok}" \
            PATH="$fakebin" /usr/bin/bash "$1" 2>&1)
        rc=$?
        if [ "$rc" -ne 0 ]; then
            printf 'runtime candidate failed (%s):\n%s\n' "$rc" "$output" >&2
            return 1
        fi
        while IFS= read -r command; do
            case "$command" in
                'ip|-4|-o|addr|show|scope|global'| \
                'ip|-4|route|show|table|main|default'| \
                'ip|-4|route|show|table|main|scope|link'| \
                'ip|-4|rule|show|pref|1000'| \
                'ip|-4|route|show|table|cn'| \
                'ip|-4|rule|show|pref|1500'| \
                'ip|-4|route|show|table|ioa'| \
                'ip|-4|-o|addr|show|tun0'| \
                'ip|-4|rule|show|pref|2500'| \
                'ip|-4|rule|show'| \
                'ip|-4|route|show|table|52'| \
                'ip|-4|rule|show|pref|3000'| \
                'tailscale|status|--json') ;;
                *) printf 'disallowed network-status command: %s\n' "$command" >&2; return 1 ;;
            esac
        done <"$log"
        [ "$(grep -Fxc 'tailscale|status|--json' "$log")" -eq 1 ] || {
            echo 'runtime contract requires exactly one tailscale status query' >&2
            return 1
        }
    }

    mode=ok
    if ! run_contract scripts/network-status ||
       ! grep -Fq 'backend       Running' <<<"$output"; then
        echo 'FAIL network-status runtime command contract or status data failed' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   network-status runtime commands are read-only status queries'

    for mutation in 'ip rule del pref 1000' 'tailscale down'; do
        candidate="$sandbox/network-status-with-mutation"
        awk -v mutation="$mutation" '/^set -u$/ { print mutation } { print }' \
            scripts/network-status >"$candidate"
        if run_contract "$candidate" >/dev/null 2>&1; then
            printf 'FAIL network-status runtime contract accepted injected %s\n' "$mutation" >&2
            rm -rf "$sandbox"
            return 1
        fi
    done
    echo 'OK   network-status runtime contract rejects injected mutations'

    for mode in failed empty malformed; do
        if run_contract scripts/network-status &&
           grep -Fq 'tailscaled not responding' <<<"$output"; then
            printf 'OK   network-status reports %s tailscale JSON without mutation\n' "$mode"
        else
            printf 'FAIL network-status hides %s tailscale JSON or violates contract\n' \
                "$mode" >&2
            rm -rf "$sandbox"
            return 1
        fi
    done
    rm -rf "$sandbox"
}

if ! status_runtime_contract; then
    fail=1
fi
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
