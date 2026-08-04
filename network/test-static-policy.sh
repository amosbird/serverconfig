#!/usr/bin/env bash
# shellcheck disable=SC2031 # sourced helpers use subshell functions; caller locals remain unchanged
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

reject 'SmartDNS caching and nonzero TTL rewrites are disabled' \
    '^(prefetch-domain yes|serve-expired yes|rr-ttl-(min|max)[[:space:]]+[1-9])' \
    network/smartdns/smartdns.conf
for directive in \
    'cache-size 0' \
    'cache-persist no' \
    'prefetch-domain no' \
    'serve-expired no' \
    'rr-ttl-min 0' \
    'bind 127.0.0.1:53 -no-cache -no-serve-expired' \
    'bind-tcp 127.0.0.1:53 -no-cache -no-serve-expired'
do
    if [ "$(grep -Fxc "$directive" network/smartdns/smartdns.conf)" -ne 1 ]; then
        echo "FAIL SmartDNS cache directive is not unique: $directive" >&2
        fail=1
    fi
done

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

smartdns_deployment_contract() (
    local sandbox smartdns_dir fake_systemctl calls output first_pid second_pid old_uid old_gid
    sandbox=$(mktemp -d)
    smartdns_dir="$sandbox/etc/smartdns"
    calls="$sandbox/calls"
    mkdir -p "$smartdns_dir"
    printf '%s\n' 'old config' >"$smartdns_dir/smartdns.conf"
    chmod 600 "$smartdns_dir/smartdns.conf"
    old_uid=$(stat -c %u "$smartdns_dir/smartdns.conf")
    old_gid=$(stat -c %g "$smartdns_dir/smartdns.conf")
    printf '%s\n' 'new config' >"$sandbox/new.conf"

    fake_systemctl="$sandbox/systemctl"
    cat >"$fake_systemctl" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$SMARTDNS_TEST_CALLS"
if [ "$1" = restart ]; then
    count=$(grep -c '^restart smartdns.service$' "$SMARTDNS_TEST_CALLS")
    if [ "$count" -eq 1 ] && [ "${SMARTDNS_FAIL_RESTART:-0}" = 1 ]; then
        exit 1
    fi
    if [ "$count" -eq 2 ] && [ "${SMARTDNS_FAIL_ROLLBACK:-0}" = 1 ]; then
        exit 1
    fi
    if [ -n "${SMARTDNS_BLOCK_ENTERED:-}" ] && [ "$count" -eq 1 ]; then
        : >"$SMARTDNS_BLOCK_ENTERED"
        while [ ! -e "$SMARTDNS_BLOCK_RELEASE" ]; do sleep 0.02; done
    fi
elif [ "$1 $2" = 'is-active --quiet' ]; then
    count=$(grep -c '^is-active --quiet smartdns.service$' "$SMARTDNS_TEST_CALLS")
    if { [ "$count" -eq 1 ] && [ "${SMARTDNS_FAIL_ACTIVE:-0}" = 1 ]; } ||
       { [ "$count" -eq 2 ] && [ "${SMARTDNS_FAIL_ROLLBACK_ACTIVE:-0}" = 1 ]; }; then
        exit 1
    fi
fi
EOF
    chmod +x "$fake_systemctl"

    # shellcheck source=network/smartdns-deploy.sh
    source network/smartdns-deploy.sh

    # Inject failures without a live filesystem or service. Each wrapper delegates by default.
    mktemp() {
        if [ "${SMARTDNS_FAIL_MKTEMP:-0}" = 1 ]; then return 1; fi
        command mktemp "$@"
    }
    cp() {
        if [ "${SMARTDNS_FAIL_CP:-0}" = 1 ]; then return 1; fi
        command cp "$@"
    }
    chmod() {
        if [ "${SMARTDNS_FAIL_CHMOD:-0}" = 1 ]; then return 1; fi
        command chmod "$@"
    }
    chown() {
        if [ "${SMARTDNS_FAIL_CHOWN:-0}" = 1 ]; then return 1; fi
        command chown "$@"
    }
    mv() {
        if [ "${SMARTDNS_FAIL_CANDIDATE_MV:-0}" = 1 ] &&
           [[ "${1:-}" = *'.smartdns.conf.candidate.'* ]]; then
            return 1
        fi
        command mv "$@"
    }

    assert_pre_install_failure() (
        local description=$1 source=$2 failure=${3:-}
        printf '%s\n' 'known good config' >"$smartdns_dir/smartdns.conf"
        : >"$calls"
        if [ -n "$failure" ]; then
            export "${failure?}"
        fi
        if SMARTDNS_TEST_CALLS="$calls" \
            deploy_smartdns_config "$source" "$smartdns_dir" \
                "$fake_systemctl" "$sandbox/deploy.lock" >/dev/null 2>&1 ||
           ! grep -Fqx 'known good config' "$smartdns_dir/smartdns.conf" ||
           [ -s "$calls" ]; then
            printf 'FAIL %s changed the target or called systemctl\n' "$description" >&2
            return 1
        fi
        printf 'OK   %s leaves the old config untouched and does not call systemctl\n' \
            "$description"
    )

    assert_pre_install_failure 'missing SmartDNS source' "$sandbox/missing.conf" || return 1
    assert_pre_install_failure 'SmartDNS candidate mktemp failure' "$sandbox/new.conf" \
        SMARTDNS_FAIL_MKTEMP=1 || return 1
    assert_pre_install_failure 'SmartDNS source copy failure' "$sandbox/new.conf" \
        SMARTDNS_FAIL_CP=1 || return 1
    assert_pre_install_failure 'SmartDNS candidate chmod failure' "$sandbox/new.conf" \
        SMARTDNS_FAIL_CHMOD=1 || return 1
    assert_pre_install_failure 'SmartDNS candidate chown failure' "$sandbox/new.conf" \
        SMARTDNS_FAIL_CHOWN=1 || return 1
    assert_pre_install_failure 'SmartDNS candidate install failure' "$sandbox/new.conf" \
        SMARTDNS_FAIL_CANDIDATE_MV=1 || return 1

    : >"$calls"
    if ! SMARTDNS_TEST_CALLS="$calls" \
        deploy_smartdns_config "$sandbox/new.conf" "$smartdns_dir" \
            "$fake_systemctl" "$sandbox/deploy.lock"; then
        echo 'FAIL valid SmartDNS deployment did not complete' >&2
        rm -rf "$sandbox"
        return 1
    fi
    if ! grep -Fqx 'new config' "$smartdns_dir/smartdns.conf" ||
       [ "$(stat -c %a "$smartdns_dir/smartdns.conf")" != 600 ] ||
       [ "$(stat -c %u "$smartdns_dir/smartdns.conf")" != "$old_uid" ] ||
       [ "$(stat -c %g "$smartdns_dir/smartdns.conf")" != "$old_gid" ] ||
       [ "$(sed -n '1p' "$calls")" != 'restart smartdns.service' ] ||
       [ "$(sed -n '2p' "$calls")" != 'is-active --quiet smartdns.service' ]; then
        echo 'FAIL successful SmartDNS deployment contract was not met' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   SmartDNS deployment preserves mode and owner, restarts, and checks active state'

    printf '%s\n' 'known good config' >"$smartdns_dir/smartdns.conf"
    chmod 640 "$smartdns_dir/smartdns.conf"
    : >"$calls"
    if SMARTDNS_TEST_CALLS="$calls" SMARTDNS_FAIL_RESTART=1 \
        deploy_smartdns_config "$sandbox/new.conf" "$smartdns_dir" \
            "$fake_systemctl" "$sandbox/deploy.lock" >/dev/null 2>&1 ||
       ! grep -Fqx 'known good config' "$smartdns_dir/smartdns.conf" ||
       [ "$(stat -c %a "$smartdns_dir/smartdns.conf")" != 640 ] ||
       [ "$(grep -Fxc 'restart smartdns.service' "$calls")" -ne 2 ]; then
        echo 'FAIL failed SmartDNS restart did not atomically restore the old config' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   failed SmartDNS restart restores bytes and mode, then restarts the old service'

    : >"$calls"
    if SMARTDNS_TEST_CALLS="$calls" SMARTDNS_FAIL_ACTIVE=1 \
        deploy_smartdns_config "$sandbox/new.conf" "$smartdns_dir" \
            "$fake_systemctl" "$sandbox/deploy.lock" >/dev/null 2>&1 ||
       ! grep -Fqx 'known good config' "$smartdns_dir/smartdns.conf" ||
       [ "$(grep -Fxc 'restart smartdns.service' "$calls")" -ne 2 ] ||
       [ "$(grep -Fxc 'is-active --quiet smartdns.service' "$calls")" -ne 2 ]; then
        echo 'FAIL inactive SmartDNS deployment was not rolled back' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   inactive SmartDNS deployment rolls back after the active-state query'

    : >"$calls"
    set +e
    output=$(SMARTDNS_TEST_CALLS="$calls" SMARTDNS_FAIL_ACTIVE=1 \
        SMARTDNS_FAIL_ROLLBACK_ACTIVE=1 \
        deploy_smartdns_config "$sandbox/new.conf" "$smartdns_dir" \
            "$fake_systemctl" "$sandbox/deploy.lock" 2>&1)
    rc=$?
    set -e
    if [ "$rc" -ne 2 ] ||
       [ "$(grep -Fxc 'restart smartdns.service' "$calls")" -ne 2 ] ||
       [ "$(grep -Fxc 'is-active --quiet smartdns.service' "$calls")" -ne 2 ] ||
       [[ "$output" != *'rollback service is inactive'* ]]; then
        echo 'FAIL inactive SmartDNS after rollback was not reported as return 2' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   SmartDNS deployment returns 2 when the rolled-back service stays inactive'

    : >"$calls"
    output=$(SMARTDNS_TEST_CALLS="$calls" SMARTDNS_FAIL_RESTART=1 SMARTDNS_FAIL_ROLLBACK=1 \
        deploy_smartdns_config "$sandbox/new.conf" "$smartdns_dir" \
            "$fake_systemctl" "$sandbox/deploy.lock" 2>&1) && {
        echo 'FAIL rollback restart failure was hidden' >&2
        rm -rf "$sandbox"
        return 1
    }
    if [[ "$output" != *'rollback restart failed'* ]]; then
        echo 'FAIL rollback restart failure was not distinguished' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   SmartDNS deployment distinguishes rollback restart failure'

    printf '%s\n' 'first config' >"$sandbox/first.conf"
    printf '%s\n' 'second config' >"$sandbox/second.conf"
    : >"$calls"
    SMARTDNS_TEST_CALLS="$calls" SMARTDNS_BLOCK_ENTERED="$sandbox/entered" \
        SMARTDNS_BLOCK_RELEASE="$sandbox/release" \
        deploy_smartdns_config "$sandbox/first.conf" "$smartdns_dir" \
            "$fake_systemctl" "$sandbox/deploy.lock" &
    first_pid=$!
    while [ ! -e "$sandbox/entered" ]; do sleep 0.02; done
    SMARTDNS_TEST_CALLS="$calls" \
        deploy_smartdns_config "$sandbox/second.conf" "$smartdns_dir" \
            "$fake_systemctl" "$sandbox/deploy.lock" &
    second_pid=$!
    sleep 0.1
    if ! grep -Fqx 'first config' "$smartdns_dir/smartdns.conf" ||
       ! kill -0 "$second_pid" 2>/dev/null; then
        echo 'FAIL SmartDNS deployment lock did not serialize transactions' >&2
        : >"$sandbox/release"
        wait "$first_pid" "$second_pid" || true
        rm -rf "$sandbox"
        return 1
    fi
    : >"$sandbox/release"
    wait "$first_pid" "$second_pid"
    if ! grep -Fqx 'second config' "$smartdns_dir/smartdns.conf"; then
        echo 'FAIL blocked SmartDNS deployment did not finish' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   SmartDNS deployment lock serializes complete transactions'
    rm -rf "$sandbox"
)

if ! smartdns_deployment_contract; then
    fail=1
fi

# shellcheck disable=SC2016 # fragment variables are literal source text
if ! grep -Fq 'local -a fragments=(office.conf dhcp-dns.conf ioa-dns.conf)' \
        network/migrate.sh ||
   [ "$(grep -Fxc '            cp -a "/etc/smartdns/$fragment" "$BACKUP_DIR/$fragment.bak"' \
        network/migrate.sh)" -ne 1 ]; then
    echo 'FAIL install does not back up every repository-managed SmartDNS fragment with metadata' >&2
    fail=1
else
    echo 'OK   install backs up all repository-managed SmartDNS fragments with metadata'
fi

smartdns_rollback_contract() {
    local sandbox smartdns_dir backup_dir fake_systemctl calls old_uid old_gid rc
    sandbox=$(mktemp -d)
    smartdns_dir="$sandbox/etc/smartdns"
    backup_dir="$sandbox/backup"
    mkdir -p "$smartdns_dir" "$backup_dir"
    fake_systemctl="$sandbox/systemctl"
    calls="$sandbox/calls"
    cat >"$fake_systemctl" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$SMARTDNS_TEST_CALLS"
if [ "$1" = restart ]; then
    count=$(grep -c '^restart smartdns.service$' "$SMARTDNS_TEST_CALLS")
    if [ "$count" -eq 1 ] && [ "${SMARTDNS_FAIL_RESTART:-0}" = 1 ]; then
        exit 1
    fi
fi
exit 0
EOF
    chmod +x "$fake_systemctl"
    : >"$calls"
    export SMARTDNS_TEST_CALLS="$calls"
    cat >"$backup_dir/smartdns.conf.bak" <<'EOF'
bind 127.0.0.1:53
conf-file /etc/smartdns/office.conf
conf-file /etc/smartdns/dhcp-dns.conf
conf-file /etc/smartdns/ioa-dns.conf
EOF
    printf '%s\n' 'nameserver /office.example/192.0.2.1' >"$backup_dir/office.conf.bak"
    chmod 600 "$backup_dir/office.conf.bak"
    printf '%s\n' 'server 192.168.255.10 -group ioa' >"$backup_dir/ioa-dns.conf.bak"
    chmod 640 "$backup_dir/ioa-dns.conf.bak"

    source network/migrate.sh
    restore_smartdns_backup "$smartdns_dir" "$backup_dir" \
        "$fake_systemctl" "$sandbox/rollback.lock"
    if ! cmp -s "$backup_dir/office.conf.bak" "$smartdns_dir/office.conf" ||
       ! cmp -s "$backup_dir/ioa-dns.conf.bak" "$smartdns_dir/ioa-dns.conf" ||
       [ "$(stat -c %a "$smartdns_dir/office.conf")" != 600 ] ||
       [ "$(stat -c %a "$smartdns_dir/ioa-dns.conf")" != 640 ] ||
       [ "$(stat -c %u:%g "$smartdns_dir/office.conf")" != \
            "$(stat -c %u:%g "$backup_dir/office.conf.bak")" ] ||
       [ "$(stat -c %u:%g "$smartdns_dir/ioa-dns.conf")" != \
            "$(stat -c %u:%g "$backup_dir/ioa-dns.conf.bak")" ] ||
       [ ! -f "$smartdns_dir/dhcp-dns.conf" ] ||
       [ -s "$smartdns_dir/dhcp-dns.conf" ] ||
       ! awk '
           $1 == "conf-file" {
               path = $2
               sub("^/etc/smartdns/", dir "/", path)
               if (system("test -f \"" path "\"") != 0) exit 1
           }
       ' dir="$smartdns_dir" "$smartdns_dir/smartdns.conf"; then
        echo 'FAIL rollback did not restore or safely seed every referenced fragment' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback restores backed fragments and safely seeds missing ones'

    printf '%s\n' 'old live base' >"$smartdns_dir/smartdns.conf"
    chmod 640 "$smartdns_dir/smartdns.conf"
    old_uid=$(stat -c %u "$smartdns_dir/smartdns.conf")
    old_gid=$(stat -c %g "$smartdns_dir/smartdns.conf")
    printf '%s\n' 'old live office' >"$smartdns_dir/office.conf"
    chmod 601 "$smartdns_dir/office.conf"
    printf '%s\n' 'old live DHCP' >"$smartdns_dir/dhcp-dns.conf"
    chmod 602 "$smartdns_dir/dhcp-dns.conf"
    rm -f "$smartdns_dir/ioa-dns.conf"
    printf '%s\n' 'invalid backup base' >"$backup_dir/smartdns.conf.bak"
    chmod 604 "$backup_dir/smartdns.conf.bak"
    printf '%s\n' 'backup office' >"$backup_dir/office.conf.bak"
    chmod 610 "$backup_dir/office.conf.bak"
    printf '%s\n' 'backup DHCP' >"$backup_dir/dhcp-dns.conf.bak"
    chmod 620 "$backup_dir/dhcp-dns.conf.bak"
    printf '%s\n' 'backup IOA' >"$backup_dir/ioa-dns.conf.bak"
    chmod 630 "$backup_dir/ioa-dns.conf.bak"
    : >"$calls"
    set +e
    SMARTDNS_FAIL_RESTART=1 restore_smartdns_backup "$smartdns_dir" "$backup_dir" \
        "$fake_systemctl" "$sandbox/rollback.lock" >/dev/null 2>&1
    rc=$?
    set -e
    if [ "$rc" -eq 0 ] ||
       ! grep -Fqx 'old live base' "$smartdns_dir/smartdns.conf" ||
       ! grep -Fqx 'old live office' "$smartdns_dir/office.conf" ||
       ! grep -Fqx 'old live DHCP' "$smartdns_dir/dhcp-dns.conf" ||
       [ -e "$smartdns_dir/ioa-dns.conf" ] ||
       [ "$(stat -c %a "$smartdns_dir/smartdns.conf")" != 640 ] ||
       [ "$(stat -c %a "$smartdns_dir/office.conf")" != 601 ] ||
       [ "$(stat -c %a "$smartdns_dir/dhcp-dns.conf")" != 602 ] ||
       [ "$(stat -c %u "$smartdns_dir/smartdns.conf")" != "$old_uid" ] ||
       [ "$(stat -c %g "$smartdns_dir/smartdns.conf")" != "$old_gid" ] ||
       [ "$(grep -Fxc 'restart smartdns.service' "$calls")" -ne 3 ] ||
       [ "$(grep -Fxc 'is-active --quiet smartdns.service' "$calls")" -ne 2 ]; then
        echo 'FAIL failed backup restore did not restore all live fragments and absence' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   failed backup restore atomically restores all live fragment states'

    printf '%s\n' 'bind 127.0.0.1:53' >"$backup_dir/smartdns.conf.bak"
    restore_smartdns_backup "$smartdns_dir" "$backup_dir" \
        "$fake_systemctl" "$sandbox/rollback.lock"
    if [ -e "$smartdns_dir/ioa-dns.conf" ]; then
        echo 'FAIL rollback retained an obsolete unreferenced IOA fragment' >&2
        rm -rf "$sandbox"
        return 1
    fi
    if [ ! -e "$smartdns_dir/office.conf" ] || [ ! -e "$smartdns_dir/dhcp-dns.conf" ]; then
        echo 'FAIL rollback removed non-obsolete repository-managed fragments' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback removes only the obsolete unreferenced IOA fragment'

    printf '%s\n' 'backup metadata base' >"$backup_dir/smartdns.conf.bak"
    chmod 604 "$backup_dir/smartdns.conf.bak"
    printf '%s\n' 'current target' >"$smartdns_dir/smartdns.conf"
    chmod 640 "$smartdns_dir/smartdns.conf"
    : >"$calls"
    restore_smartdns_backup "$smartdns_dir" "$backup_dir" \
        "$fake_systemctl" "$sandbox/rollback.lock"
    if [ "$(stat -c %a "$smartdns_dir/smartdns.conf")" != 604 ] ||
       [ "$(stat -c %u "$smartdns_dir/smartdns.conf")" != \
            "$(stat -c %u "$backup_dir/smartdns.conf.bak")" ] ||
       [ "$(stat -c %g "$smartdns_dir/smartdns.conf")" != \
            "$(stat -c %g "$backup_dir/smartdns.conf.bak")" ]; then
        echo 'FAIL rollback did not preserve backup base metadata' >&2
        rm -rf "$sandbox"
        return 1
    fi
    echo 'OK   rollback preserves backup base metadata instead of target metadata'
    unset SMARTDNS_TEST_CALLS
    rm -rf "$sandbox"
}

if ! smartdns_rollback_contract; then
    fail=1
fi

foreign_routing_config=network/systemd-networkd.conf.d/foreign-routing.conf
if [ ! -f "$foreign_routing_config" ] ||
   [ "$(grep -Fxc 'ManageForeignRoutingPolicyRules=no' "$foreign_routing_config")" -ne 1 ] ||
   [ "$(grep -Fxc 'ManageForeignRoutes=no' "$foreign_routing_config")" -ne 1 ]; then
    echo 'FAIL networkd does not preserve foreign routes and rules' >&2
    fail=1
else
    echo 'OK   networkd preserves foreign routes and rules'
fi
if ! grep -Fq 'systemd-networkd.conf.d/foreign-routing.conf' restore.sh ||
   ! grep -Fq 'systemd-networkd.conf.d/foreign-routing.conf' network/migrate.sh; then
    echo 'FAIL foreign-routing drop-in is not installed by both deployment paths' >&2
    fail=1
fi
if ! sed -n '/^rollback() {/,/^}/p' network/migrate.sh |
        grep -Fq 'rm -f /etc/systemd/networkd.conf.d/foreign-routing.conf'; then
    echo 'FAIL migration rollback does not remove the foreign-routing drop-in' >&2
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
