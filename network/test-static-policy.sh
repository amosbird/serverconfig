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
for domain in \
    smartgate.oa.tencent.com \
    sgw.woa.com \
    ioa.tencent.com \
    '*-smartgate.oa.tencent.com' \
    cloud-smartvpn.oa.tencent.com \
    http-cloud-smartvpn.oa.tencent.com
do
    if [ "$(grep -Fxc "ipset /$domain/-" network/smartdns/smartdns.conf)" -ne 1 ]; then
        echo "FAIL base SmartDNS does not exclude $domain exactly once from ioa" >&2
        fail=1
    fi
done
for domain in smartgate.oa.tencent.com sgw.woa.com ioa.tencent.com; do
    if [ "$(grep -Fxc "nameserver /$domain/china" network/smartdns/smartdns.conf)" -ne 1 ]; then
        echo "FAIL base SmartDNS does not map $domain exactly once to china" >&2
        fail=1
    fi
    if [ "$(grep -Fxc "nameserver /$domain/ioa" network/smartdns/office.conf)" -ne 1 ]; then
        echo "FAIL office SmartDNS does not map $domain exactly once to ioa" >&2
        fail=1
    fi
done
if [ "$(grep -Fxc 'ipset /woa.com/ioa' network/smartdns/smartdns.conf)" -ne 1 ]; then
    echo 'FAIL SmartDNS broad woa.com business classification is missing or duplicated' >&2
    fail=1
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

foreign_routing_config=network/systemd-networkd.conf.d/foreign-routing.conf
iwd_config=network/iwd/main.conf
wireless_config=network/systemd-network/25-wireless.network
tencent_wired_link=network/systemd-network/10-tencent-wired.link
obsolete_tencent_config=network/systemd-network/26-wireless-tencent.network

if [ ! -f "$tencent_wired_link" ] ||
   [ "$(grep -Fxc 'Path=pci-0000:00:14.0-usb-0:1:1.0' "$tencent_wired_link")" -ne 1 ] ||
   [ "$(grep -Fxc 'MACAddress=08:3a:88:5a:b5:37' "$tencent_wired_link")" -ne 1 ]; then
    echo 'FAIL Tencent wired registered MAC link policy is missing or incorrect' >&2
    fail=1
fi
if ! grep -Fq 'network/systemd-network/*.link' restore.sh; then
    echo 'FAIL restore does not deploy systemd link policy' >&2
    fail=1
fi
if ! grep -Fqx 'ExecStart=/usr/bin/wpa_supplicant -D wired -c /etc/wpa_supplicant/wpa_supplicant-wired.conf -i %I' \
        network/systemd/wpa_supplicant@.service.d/override.conf; then
    echo 'FAIL wired wpa_supplicant override does not select the wired driver' >&2
    fail=1
fi
if [ "$(grep -Fxc '[Install]' network/systemd/wpa_supplicant@.service.d/override.conf)" -ne 1 ] ||
   [ "$(grep -Fxc 'WantedBy=' network/systemd/wpa_supplicant@.service.d/override.conf)" -ne 1 ] ||
   [ "$(grep -Fxc 'WantedBy=sys-subsystem-net-devices-%i.device' \
        network/systemd/wpa_supplicant@.service.d/override.conf)" -ne 1 ] ||
   [ "$(grep -Fxc 'BindsTo=sys-subsystem-net-devices-%i.device' \
        network/systemd/wpa_supplicant@.service.d/override.conf)" -ne 1 ] ||
   [ "$(grep -Fxc 'After=sys-subsystem-net-devices-%i.device' \
        network/systemd/wpa_supplicant@.service.d/override.conf)" -ne 1 ]; then
    echo 'FAIL wired wpa_supplicant lacks the USB device-bound hot-plug contract' >&2
    fail=1
fi
if grep -Eq 'systemctl enable( --now)? wpa_supplicant@' restore.sh; then
    echo 'FAIL generic restore enables a machine-specific wired 802.1X instance' >&2
    fail=1
fi
if [ "$(grep -Fxc 'SUBSYSTEM=="net", ACTION!="remove", ATTRS{idVendor}=="0b95", ATTRS{idProduct}=="1790", ATTRS{serial}=="00000EC65DE788", TAG+="systemd", ENV{SYSTEMD_WANTS}="wpa_supplicant@enp9s0u2u1u2.service"' \
        network/udev/90-wired-8021x.rules)" -ne 1 ]; then
    echo 'FAIL wired 802.1X hot-plug rule does not survive the registered USB adapter rename' >&2
    fail=1
fi
reject 'wired 802.1X hot-plug is not limited to add or the post-link MAC' \
    'ACTION=="add"|ATTR\{address\}=="08:3a:88:5a:b5:37"' \
    network/udev/90-wired-8021x.rules
reject 'restore does not start or restart wired 802.1X' \
    'systemctl (start|restart|try-restart) wpa_supplicant@' restore.sh

if [ "$(grep -Fxc 'AddressRandomization=network' "$iwd_config")" -ne 1 ]; then
    echo 'FAIL iwd does not use stable per-network MAC addresses' >&2
    fail=1
else
    echo 'OK   iwd uses stable per-network MAC addresses'
fi
if [ "$(grep -Fxc 'IgnoreCarrierLoss=3s' "$wireless_config")" -ne 1 ] ||
   grep -Eq '^IgnoreCarrierLoss=(yes|infinite)$' "$wireless_config"; then
    echo 'FAIL wireless carrier grace is not exactly finite 3s' >&2
    fail=1
else
    echo 'OK   wireless carrier grace is finite 3s'
fi
if [ -e "$obsolete_tencent_config" ]; then
    echo 'FAIL obsolete Tencent no-gateway networkd config still exists' >&2
    fail=1
else
    echo 'OK   Tencent-WiFi uses the generic physical Wi-Fi configuration'
fi

if [ ! -f "$foreign_routing_config" ] ||
   [ "$(grep -Fxc 'ManageForeignRoutingPolicyRules=no' "$foreign_routing_config")" -ne 1 ] ||
   [ "$(grep -Fxc 'ManageForeignRoutes=no' "$foreign_routing_config")" -ne 1 ]; then
    echo 'FAIL networkd does not preserve foreign routes and rules' >&2
    fail=1
else
    echo 'OK   networkd preserves foreign routes and rules'
fi
if ! grep -Fq 'systemd-networkd.conf.d/foreign-routing.conf' restore.sh; then
    echo 'FAIL foreign-routing drop-in is not installed by restore' >&2
    fail=1
fi
if ! grep -Fq 'sudo rm -f /etc/systemd/network/26-wireless-tencent.network' restore.sh ||
   ! grep -Fq 'if sudo systemctl is-active --quiet systemd-networkd.service; then' restore.sh ||
   ! grep -Fq 'sudo networkctl reload' restore.sh; then
    echo 'FAIL restore does not remove stale Tencent policy or safely reload networkd' >&2
    fail=1
else
    echo 'OK   restore removes stale Tencent policy and only reloads active networkd'
fi
if ! grep -Fq 'for unit in /etc/systemd/system/multi-user.target.wants/netctl@*.service; do' \
       restore.sh ||
   ! grep -Fq 'sudo systemctl disable "$(basename "$unit")"' restore.sh ||
   ! grep -Fq 'sudo systemctl disable netctl.service' restore.sh; then
    echo 'FAIL restore does not disable stale netctl units for the next boot' >&2
    fail=1
else
    echo 'OK   restore disables stale netctl units without stopping the active link'
fi
reject 'restore does not stop legacy network owners during deployment' \
    "systemctl disable --now .*netctl|systemctl stop .*netctl|netctl stop" restore.sh
if ! grep -Fq 'sudo systemctl enable systemd-networkd.service iwd.service' restore.sh; then
    echo 'FAIL restore does not enable the network stack for the next boot' >&2
    fail=1
else
    echo 'OK   restore enables the network stack for the next boot'
fi
if ! grep -Fq "grep -Fqx 'AddressOverride=1e:dc:46:00:66:1b'" restore.sh; then
    echo 'FAIL restore does not validate the Tencent Android MAC override' >&2
    fail=1
fi
reject 'restore never prints Tencent credential profile contents' \
    'cat .*Tencent-WiFi\.8021x|sed .*Tencent-WiFi\.8021x|grep -v .*Tencent-WiFi\.8021x' \
    restore.sh
reject 'restore does not restart network owners for Wi-Fi policy deployment' \
    'systemctl (restart|try-restart) (systemd-networkd|iwd|tailscaled|smartdns|ngnclient)' \
    restore.sh
reject 'restore does not require the retired packet recorder dependency' \
    'tcpdump is required|\[ -x /usr/bin/tcpdump \]' restore.sh

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
    restore.sh
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
reject 'no obsolete underlay staging tables' \
    '(^|[^A-Z_])UNDERLAY_(TABLE|STAGE)|lookup underlay|table 50[01]' \
    scripts/network-reconfigure scripts/netfix scripts/network-status
reject 'no broad private bypass' \
    'PRIVATE_NETS|192\.168\.0\.0/16.*lookup main|172\.16\.0\.0/12.*lookup main|169\.254\.0\.0/16.*lookup main' \
    scripts/network-reconfigure
reject 'no static 9/8 IOA rule' 'IOA_STATIC_CIDRS=.*9\.0\.0\.0/8' \
    scripts/network-reconfigure
reject 'no static 21/8 IOA rule' \
    'IOA_STATIC_CIDRS=.*21\.0\.0\.0/8|ip rule add to 21\.0\.0\.0/8.*lookup ioa' \
    scripts/network-reconfigure
reject 'active policy no longer intersects SmartDNS results with a static prefix set' \
    'IOA_INTRANET|--match-set ioa_intranet|ipset (create|add|flush) ioa_intranet' \
    scripts/network-reconfigure network/README.md
reject 'CN promotion does not launch one ip process per route' \
    'ip route replace \\$route table "\\$CN_TABLE"|grep -Fqx "\\$route" <<<"\\$stage_routes"' \
    scripts/network-reconfigure
reject 'table 19 is advertisement-only and has no policy rule' \
    'ip rule (add|replace).*lookup (19|wired_underlay)' scripts/network-reconfigure
reject 'wired advertisement does not mutate tunnel-owned tables' \
    'ip route (flush|del|replace).*table (20|230|52|ioa)' scripts/network-reconfigure
for statement in \
    'IgnoreCarrierLoss=3s' \
    'AddressRandomization=network' \
    'AddressOverride=1e:dc:46:00:66:1b' \
    'restore.sh'
do
    if ! grep -Fq "$statement" network/README.md; then
        echo "FAIL network README omits current deployment policy: $statement" >&2
        fail=1
    fi
done
legacy_network_sources=(
    restore.sh
    network/README.md
    network/iwd
    network/smartdns
    network/systemd
    network/systemd-network
    network/systemd-networkd.conf.d
    network/udev
    scripts
    .config/fish/conf.d/completions.fish
)
legacy_network_pattern='dhcpcd|ncswitch|network/migrate\.sh|network/rollback'
if grep -RInE "$legacy_network_pattern" "${legacy_network_sources[@]}" \
        >/tmp/network-legacy.$$ 2>/dev/null; then
    echo 'FAIL current network sources still reference the retired migration stack' >&2
    cat /tmp/network-legacy.$$ >&2
    fail=1
else
    echo 'OK   current network sources do not reference the retired migration stack'
fi
rm -f /tmp/network-legacy.$$
for retired in network/migrate.sh network/rollback scripts/ncswitch old_README.org; do
    if [ -e "$retired" ]; then
        echo "FAIL retired network artifact still exists: $retired" >&2
        fail=1
    fi
done

exit "$fail"
