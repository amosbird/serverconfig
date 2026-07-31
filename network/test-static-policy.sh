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
audit_netfix() {
    python3 - "$1" <<'PY_AUDIT'
import re, shlex, sys
path = sys.argv[1]
allowed = tuple(map(re.compile, (
    r"ip(?:\s+-4)?\s+rule\s+show(?:\s|$)",
    r"ip(?:\s+-4)?\s+route\s+(?:show|get)(?:\s|$)",
    r"ip(?:\s+-4)?(?:\s+-o)?\s+addr\s+show(?:\s|$)",
    r"iptables\s+-S(?:\s|$)", r"ipset\s+list(?:\s|$)",
    r"systemctl\s+is-active(?:\s|$)", r"tailscale\s+status\s+--json(?:\s|$)",
)))
sensitive = re.compile(r"(?:^|/)(ip|iptables|ipset|systemctl|tailscale)$")
text = open(path, encoding="utf-8").read()
for number, raw in enumerate(text.splitlines(), 1):
    line = raw.split("#", 1)[0].strip()
    if not line:
        continue
    try:
        words = shlex.split(line, comments=True, posix=True)
    except ValueError:
        words = re.findall(r"[^\s|;]+", line.removesuffix("\\"))
    for index, word in enumerate(words):
        command = word.strip("$();{}")
        if sensitive.search(command):
            shape = " ".join([command.rsplit("/", 1)[-1], *words[index + 1:]])
            if not any(pattern.match(shape) for pattern in allowed):
                print(f"{path}:{number}: disallowed sensitive command: {line}", file=sys.stderr)
                sys.exit(1)
if len(re.findall(r'env\s+FORCE=1\s+"\$RECONFIGURE"', text)) != 1:
    print(f'{path}: expected exactly one env FORCE=1 "$RECONFIGURE"', file=sys.stderr)
    sys.exit(1)
PY_AUDIT
}

if audit_netfix scripts/netfix; then
    echo 'OK   netfix sensitive commands match the read-only allowlist'
else
    echo 'FAIL netfix sensitive-command allowlist audit failed' >&2
    fail=1
fi
for mutation in \
    '/usr/bin/ip -4 rule del pref 500' \
    'command ip route flush table main' \
    'iptables --flush' \
    'tailscale down'
do
    candidate=$(mktemp)
    cp scripts/netfix "$candidate"
    printf '\n%s\n' "$mutation" >>"$candidate"
    if audit_netfix "$candidate" >/dev/null 2>&1; then
        printf 'FAIL netfix auditor accepted mutation: %s\n' "$mutation" >&2
        fail=1
    else
        printf 'OK   netfix auditor rejects mutation: %s\n' "$mutation"
    fi
    rm -f "$candidate"
done

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
