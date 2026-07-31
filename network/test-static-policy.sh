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

# Exercise rollback rule selection through a fake ip command. The fixture includes
# foreign rules at repository-used preferences; only repository-owned shapes may be deleted.
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cat > "$tmp/ip" <<'EOF'
#!/usr/bin/env bash
if [ "$1 $2 $3 $4 $5" = '-4 rule show pref 500' ]; then
    printf '%s\n' \
        '500: from all fwmark 0x80000/0xff0000 lookup main' \
        '500: from 192.0.2.10 lookup main'
elif [ "$1 $2 $3 $4 $5" = '-4 rule show pref 1000' ]; then
    printf '%s\n' \
        '1000: from all to 192.168.1.0/24 lookup main' \
        '1000: from 198.51.100.7 lookup main'
elif [ "$1 $2 $3 $4 $5" = '-4 rule show pref 1500' ]; then
    printf '%s\n' '1500: from all lookup cn' '1500: from all lookup foreign'
elif [ "$1 $2 $3 $4 $5" = '-4 rule show pref 2500' ]; then
    printf '%s\n' \
        '2500: from all fwmark 0x1/0xffffffff lookup ioa' \
        '2500: from all to 10.0.0.0/8 lookup ioa' \
        '2500: from 203.0.113.8 lookup ioa'
elif [ "$1 $2 $3 $4 $5" = '-4 rule show pref 3000' ]; then
    printf '%s\n' \
        '3000: from all to 100.64.0.0/10 lookup 52' \
        '3000: from all lookup 52'
elif [ "$1 $2" = 'rule del' ]; then
    printf '%s\n' "$*" >> "$IP_LOG"
fi
EOF
chmod +x "$tmp/ip"
IP_LOG="$tmp/deleted" PATH="$tmp:$PATH" MIGRATE_LIB_ONLY=1 \
    bash -c 'source network/migrate.sh; cleanup_owned_rules'
cat > "$tmp/expected" <<'EOF'
rule del pref 500 from all fwmark 0x80000/0xff0000 lookup main
rule del pref 1000 from all to 192.168.1.0/24 lookup main
rule del pref 1500 from all lookup cn
rule del pref 2500 from all fwmark 0x1/0xffffffff lookup ioa
rule del pref 2500 from all to 10.0.0.0/8 lookup ioa
rule del pref 3000 from all to 100.64.0.0/10 lookup 52
EOF
if ! cmp -s "$tmp/expected" "$tmp/deleted"; then
    echo 'FAIL rollback rule cleanup deleted outside owned shapes' >&2
    diff -u "$tmp/expected" "$tmp/deleted" >&2 || true
    fail=1
else
    echo 'OK   rollback preserves foreign same-preference rules'
fi

exit "$fail"
