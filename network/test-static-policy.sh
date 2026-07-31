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
    'cp .*network-fallback|systemctl enable .*network-fallback' restore.sh network/migrate.sh
reject 'no exit-node mutation in local tools' \
    'tailscale (set --exit-node=|down|up)|systemctl restart tailscaled' \
    scripts/network-reconfigure scripts/netfix scripts/network-status
reject 'no DERP or IOA endpoint caches' \
    'DERP_CACHE|derp-ips|IOA_ENDPOINT_CACHE|ioa-endpoints|IOA_BOOTSTRAP_HOSTS' \
    scripts/network-reconfigure scripts/netfix scripts/network-status
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
