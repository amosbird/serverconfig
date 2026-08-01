#!/usr/bin/env bash
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"

fail=0
IOA_SERVER='server 192.168.255.10 -group ioa -exclude-default-group'
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

if [ "$(grep -Fxc "$IOA_SERVER" network/smartdns/smartdns.conf)" -ne 1 ]; then
    echo 'FAIL SmartDNS base config does not contain exactly one permanent IOA resolver' >&2
    fail=1
else
    echo 'OK   SmartDNS base config contains exactly one permanent IOA resolver'
fi
reject 'SmartDNS base config does not include a dynamic IOA fragment' \
    'conf-file[[:space:]]+/etc/smartdns/ioa-dns\.conf' network/smartdns/smartdns.conf
reject 'production reconciler has no dynamic IOA DNS logic' \
    'IOA_RESOLVER|ioa-dns\.conf|ip -4 -o addr show tun0' scripts/network-reconfigure
# Activation must not create or seed the legacy fragment.
if sed -n '/^activate() {/,/^}/p' network/migrate.sh |
   grep -Eq 'ioa-dns\.conf|IOA tunnel is down'; then
    echo 'FAIL migration activation depends on a dynamic IOA DNS fragment' >&2
    fail=1
else
    echo 'OK   migration activation does not depend on a dynamic IOA DNS fragment'
fi

smartdns_deployment_contract() {
    local sandbox smartdns_dir fake_smartdns fake_timeout fake_systemctl calls
    sandbox=$(mktemp -d)
    smartdns_dir="$sandbox/etc/smartdns"
    calls="$sandbox/calls"
    mkdir -p "$smartdns_dir"
    printf '%s\n' 'old config' >"$smartdns_dir/smartdns.conf"
    cat >"$sandbox/new.conf" <<'EOF'
bind 127.0.0.1:53
bind-tcp 127.0.0.1:53
server 192.0.2.53
EOF

    fake_smartdns="$sandbox/smartdns"
    cat >"$fake_smartdns" <<'EOF'
#!/usr/bin/env bash
printf 'validate:%s\n' "$*" >>"$SMARTDNS_TEST_CALLS"
config=${*: -1}
grep -Eq '^bind 127\.0\.0\.1:[2-4][0-9]{4}$' "$config"
grep -Eq '^bind-tcp 127\.0\.0\.1:[2-4][0-9]{4}$' "$config"
grep -Fqx 'server 192.0.2.53' "$config"
EOF
    fake_timeout="$sandbox/timeout"
    cat >"$fake_timeout" <<'EOF'
#!/usr/bin/env bash
shift
"$@"
rc=$?
[ "$rc" -eq 0 ] && exit 124
exit "$rc"
EOF
    fake_systemctl="$sandbox/systemctl"
    cat >"$fake_systemctl" <<'EOF'
#!/usr/bin/env bash
printf 'restart:%s\n' "$*" >>"$SMARTDNS_TEST_CALLS"
grep -Fqx 'server 192.0.2.53' "$SMARTDNS_TEST_DIR/smartdns.conf"
EOF
    chmod +x "$fake_smartdns" "$fake_timeout" "$fake_systemctl"

    # shellcheck source=network/smartdns-deploy.sh
    source network/smartdns-deploy.sh
    if ! SMARTDNS_TEST_CALLS="$calls" SMARTDNS_TEST_DIR="$smartdns_dir" \
        deploy_smartdns_config "$sandbox/new.conf" "$smartdns_dir" \
            "$fake_smartdns" "$fake_timeout" "$fake_systemctl"; then
        echo 'FAIL valid SmartDNS deployment did not complete' >&2
        rm -rf "$sandbox"
        return 1
    fi
    case "$(sed -n '1p' "$calls")" in
        "validate:-f -x -p - -c $smartdns_dir/"*) ;;
        *)
            echo 'FAIL SmartDNS deployment did not validate before restart' >&2
            rm -rf "$sandbox"
            return 1
            ;;
    esac
    if [ "$(sed -n '2p' "$calls")" != 'restart:restart smartdns.service' ]; then
        echo 'FAIL SmartDNS deployment did not validate before restart' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   SmartDNS deployment validates, atomically installs, then restarts'

    printf '%s\n' 'known good config' >"$smartdns_dir/smartdns.conf"
    cat >"$fake_timeout" <<'EOF'
#!/usr/bin/env bash
exit 2
EOF
    if SMARTDNS_TEST_CALLS="$calls" SMARTDNS_TEST_DIR="$smartdns_dir" \
        deploy_smartdns_config "$sandbox/new.conf" "$smartdns_dir" \
            "$fake_smartdns" "$fake_timeout" "$fake_systemctl" >/dev/null 2>&1 ||
       ! grep -Fqx 'known good config' "$smartdns_dir/smartdns.conf"; then
        echo 'FAIL invalid SmartDNS deployment replaced the live config' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   failed SmartDNS validation preserves the live config'

    cat >"$fake_timeout" <<'EOF'
#!/usr/bin/env bash
exit 124
EOF
    cat >"$fake_systemctl" <<'EOF'
#!/usr/bin/env bash
exit 1
EOF
    if SMARTDNS_TEST_CALLS="$calls" SMARTDNS_TEST_DIR="$smartdns_dir" \
        deploy_smartdns_config "$sandbox/new.conf" "$smartdns_dir" \
            "$fake_smartdns" "$fake_timeout" "$fake_systemctl" >/dev/null 2>&1; then
        echo 'FAIL SmartDNS restart failure was hidden' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   SmartDNS restart failure remains visible'
    rm -rf "$sandbox"
}

if ! smartdns_deployment_contract; then
    fail=1
fi

smartdns_rollback_contract() {
    local sandbox smartdns_dir backup_dir
    sandbox=$(mktemp -d)
    smartdns_dir="$sandbox/etc/smartdns"
    backup_dir="$sandbox/backup"
    mkdir -p "$smartdns_dir" "$backup_dir"
    cat >"$backup_dir/smartdns.conf.bak" <<'EOF'
bind 127.0.0.1:53
conf-file /etc/smartdns/ioa-dns.conf
EOF

    source network/migrate.sh
    restore_smartdns_backup "$smartdns_dir" "$backup_dir" false
    if ! grep -Fqx '# Legacy IOA resolver unavailable during rollback' \
            "$smartdns_dir/ioa-dns.conf" ||
       ! awk '
           $1 == "conf-file" {
               path = $2
               sub("^/etc/smartdns/", dir "/", path)
               if (system("test -f \"" path "\"") != 0) exit 1
           }
       ' dir="$smartdns_dir" "$smartdns_dir/smartdns.conf"; then
        echo 'FAIL rollback did not leave all restored SmartDNS includes parseable' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback seeds a safe fragment when an old config includes it'

    printf '%s\n' 'server 192.168.255.10 -group ioa' >"$backup_dir/ioa-dns.conf.bak"
    restore_smartdns_backup "$smartdns_dir" "$backup_dir" false
    if ! cmp -s "$backup_dir/ioa-dns.conf.bak" "$smartdns_dir/ioa-dns.conf"; then
        echo 'FAIL rollback did not restore its backed-up IOA fragment' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback restores the backed-up IOA fragment'

    printf '%s\n' 'bind 127.0.0.1:53' >"$backup_dir/smartdns.conf.bak"
    restore_smartdns_backup "$smartdns_dir" "$backup_dir" false
    if [ -e "$smartdns_dir/ioa-dns.conf" ]; then
        echo 'FAIL rollback retained an obsolete unreferenced IOA fragment' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback removes the IOA fragment only when the restored config omits it'
    rm -rf "$sandbox"
}

if ! smartdns_rollback_contract; then
    fail=1
fi

if ! awk '
    /office\.conf/ && /sudo tee/ { office = NR }
    /dhcp-dns\.conf/ && /sudo cp/ { dhcp = NR }
    /smartdns-deploy\.sh/ { deploy = NR }
    END { exit !(office && dhcp && deploy && office < deploy && dhcp < deploy) }
' restore.sh; then
    echo 'FAIL restore does not seed SmartDNS fragments before safe deployment' >&2
    fail=1
else
    echo 'OK   restore seeds SmartDNS fragments before safe deployment'
fi

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
if ! grep -Fq 'sudo rm -f /var/lib/network-reconfigure/derp-ips' restore.sh ||
   ! grep -Fq '/var/lib/network-reconfigure/ioa-endpoints' restore.sh; then
    echo 'FAIL restore does not remove obsolete network cache files' >&2
    fail=1
fi
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
reject 'rollback does not flush cn_stage outside the ownership lock' \
    '^[[:space:]]*ip route flush table cn_stage' network/migrate.sh
if ! grep -Fq 'cleanup_owned_rules' network/migrate.sh; then
    echo 'FAIL rollback does not use owned rule shapes' >&2
    fail=1
fi

stage_registration_cleanup_contract() {
    local sandbox rt_tables lock ip_log fake_ip ip_fail before output cleanup_pid holder_pid
    sandbox=$(mktemp -d)
    rt_tables="$sandbox/rt_tables"
    lock="$sandbox/network-reconfigure.lock"
    ip_log="$sandbox/ip.log"
    fake_ip="$sandbox/ip"
    ip_fail="$sandbox/ip.fail"
    printf '#!/usr/bin/env bash\nprintf "%%s\\n" "$*" >>%q\n[ ! -e %q ]\n' \
        "$ip_log" "$ip_fail" >"$fake_ip"
    chmod +x "$fake_ip"

    # Source the helper and pass only disposable fixtures; never redirect the production CLI.
    source network/migrate.sh

    cat >"$rt_tables" <<'EOF'
# reserved values
255 local
10042 cn_stage
101 cn
EOF
    cleanup_stage_registration "$rt_tables" "$lock" "$fake_ip"
    if grep -Eq '^[[:space:]]*10042[[:space:]]+cn_stage[[:space:]]*$' "$rt_tables" ||
       [ "$(cat "$ip_log")" != 'route flush table 10042' ]; then
        echo 'FAIL rollback does not flush a unique owned cn_stage ID before unregistering it' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback removes a unique owned cn_stage registration'

    cat >"$rt_tables" <<'EOF'
10042 cn_stage
101 cn
EOF
    : >"$ip_log"
    touch "$ip_fail"
    before=$(cat "$rt_tables")
    if output=$(cleanup_stage_registration "$rt_tables" "$lock" "$fake_ip" 2>&1); then
        echo 'FAIL cleanup helper reports success after its route flush fails' >&2
        rm -rf "$sandbox"
        return 1
    fi
    rm -f "$ip_fail"
    if [ "$(cat "$rt_tables")" != "$before" ] ||
       [ "$(cat "$ip_log")" != 'route flush table 10042' ] ||
       ! grep -Fq 'could not flush cn_stage table 10042' <<<"$output"; then
        echo 'FAIL rollback unregisters cn_stage after its route flush fails' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback retains cn_stage registration when its route flush fails'

    cat >"$rt_tables" <<'EOF'
10042 cn_stage
10043 cn_stage
101 cn
EOF
    : >"$ip_log"
    before=$(cat "$rt_tables")
    output=$(cleanup_stage_registration "$rt_tables" "$lock" "$fake_ip" 2>&1)
    if [ "$(cat "$rt_tables")" != "$before" ] || [ -s "$ip_log" ] ||
       ! grep -Fq 'cn_stage registration is ambiguous' <<<"$output"; then
        echo 'FAIL rollback mutates duplicate cn_stage name registrations' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback preserves duplicate cn_stage name registrations'

    cat >"$rt_tables" <<'EOF'
10042 cn_stage
10042 foreign
101 cn
EOF
    : >"$ip_log"
    before=$(cat "$rt_tables")
    output=$(cleanup_stage_registration "$rt_tables" "$lock" "$fake_ip" 2>&1)
    if [ "$(cat "$rt_tables")" != "$before" ] || [ -s "$ip_log" ] ||
       ! grep -Fq 'cn_stage registration is ambiguous' <<<"$output"; then
        echo 'FAIL rollback mutates a cn_stage registration with an ID conflict' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback preserves cn_stage registrations with ID conflicts'

    cat >"$rt_tables" <<'EOF'
10042 cn_stage
101 cn
EOF
    # shellcheck disable=SC2016 # positional arguments expand inside the helper shell
    flock "$lock" bash -c '
        touch "$1"
        while [ ! -e "$2" ]; do sleep 0.01; done
        printf "%s\n" "400 concurrent" >>"$3"
    ' _ "$sandbox/locked" "$sandbox/release" "$rt_tables" &
    holder_pid=$!
    while [ ! -e "$sandbox/locked" ]; do sleep 0.01; done
    cleanup_stage_registration "$rt_tables" "$lock" "$fake_ip" &
    cleanup_pid=$!
    sleep 0.1
    if ! kill -0 "$cleanup_pid" 2>/dev/null; then
        echo 'FAIL rollback cleanup does not wait for the reconfigure lock' >&2
        touch "$sandbox/release"
        wait "$holder_pid"
        wait "$cleanup_pid" || true
        rm -rf "$sandbox"
        return 1
    fi
    touch "$sandbox/release"
    wait "$holder_pid"
    wait "$cleanup_pid"
    if ! grep -Fqx '400 concurrent' "$rt_tables" ||
       grep -Eq '^[[:space:]]*10042[[:space:]]+cn_stage[[:space:]]*$' "$rt_tables"; then
        echo 'FAIL locked cleanup does not preserve the latest concurrent registration state' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback cleanup serializes and rewrites the latest registration state'
    rm -rf "$sandbox"
}

if ! stage_registration_cleanup_contract; then
    fail=1
fi

if ! grep -Fq "cleanup_stage_registration \\" network/migrate.sh ||
   ! grep -Fq '/etc/iproute2/rt_tables /run/lock/network-reconfigure.lock /usr/bin/ip' \
       network/migrate.sh; then
    echo 'FAIL rollback does not use the fixed live rt_tables and shared lock paths' >&2
    fail=1
fi
if grep -Eq 'RT_TABLES_(OVERRIDE|TEST)|MIGRATE_.*RT_TABLES' network/migrate.sh; then
    echo 'FAIL migration CLI exposes an rt_tables environment override' >&2
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
    iptables -t nat -A POSTROUTING -m mark --mark 0x1/0xffffffff -o tun0 -j MASQUERADE
    iptables -t nat -A POSTROUTING -m mark --mark 0x1/0xffffffff -o tun0 \
        -j SNAT --to-source 192.0.2.10
    iptables -t nat -A POSTROUTING -m mark --mark 0x1/0xffffffff -o tun1 -j MASQUERADE
    source "$migrate"

    sandbox=$(mktemp -d)
    rt_tables="$sandbox/rt_tables"
    lock="$sandbox/network-reconfigure.lock"
    printf "%s\n" "10042 cn_stage" "101 cn" >"$rt_tables"
    ip route add blackhole 203.0.113.0/24 table 10042
    cleanup_stage_registration "$rt_tables" "$lock" /usr/bin/ip
    ! ip route show table 10042 | grep -q .
    ! grep -Eq "^[[:space:]]*10042[[:space:]]+cn_stage[[:space:]]*$" "$rt_tables"

    printf "%s\n" "10042 cn_stage" "10043 cn_stage" "101 cn" >"$rt_tables"
    ip route add blackhole 203.0.113.0/24 table 10042
    cleanup_stage_registration "$rt_tables" "$lock" /usr/bin/ip 2>/dev/null
    ip route show table 10042 | grep -Fq "blackhole 203.0.113.0/24"
    grep -Fq "10042 cn_stage" "$rt_tables"

    printf "%s\n" "10042 cn_stage" "10042 foreign" "101 cn" >"$rt_tables"
    cleanup_stage_registration "$rt_tables" "$lock" /usr/bin/ip 2>/dev/null
    ip route show table 10042 | grep -Fq "blackhole 203.0.113.0/24"
    grep -Fq "10042 cn_stage" "$rt_tables"
    rm -rf "$sandbox"

    cleanup_owned_rules
    cleanup_owned_nat

    rules=$(ip -4 rule show pref 2500)
    expected=$(printf "2500:\tfrom all fwmark 0x1/0x1 lookup ioa\n"\
"2500:\tfrom 203.0.113.8 lookup ioa")
    [ "$rules" = "$expected" ]
    nat_rules=$(iptables -t nat -S POSTROUTING)
    ! grep -Fq -- "-A POSTROUTING -o tun0 -m mark --mark 0x1 -j MASQUERADE" \
        <<<"$nat_rules"
    grep -Fq -- \
        "-A POSTROUTING -o tun0 -m mark --mark 0x1 -j SNAT --to-source 192.0.2.10" \
        <<<"$nat_rules"
    grep -Fq -- "-A POSTROUTING -o tun1 -m mark --mark 0x1 -j MASQUERADE" \
        <<<"$nat_rules"
' bash "$ROOT/network/migrate.sh" "$host_netns_link" "$host_netns_inode"; then
    echo 'OK   rollback safely flushes and unregisters staging state in a real namespace'
else
    echo 'FAIL rollback mishandles staging ownership or kernel-rendered state' >&2
    fail=1
fi

exit "$fail"
