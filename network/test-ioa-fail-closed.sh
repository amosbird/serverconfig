#!/usr/bin/env bash

set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT="$ROOT/scripts/network-reconfigure"

if [ -z "${IN_NETNS:-}" ]; then
    for expected in \
        'IOA_OWNER_MARK="0x1000000/0xffffffff"' \
        'system.slice/ngnclient.service' \
        'active_ioa_cgroups()' \
        'IOA_CGROUP_PATHS_OVERRIDE+x' \
        'DESIRED_BANDS[$P_IOA_OWNER]="from all fwmark $IOA_OWNER_MARK lookup main"' \
        'DESIRED_BANDS[$P_IOA_OWNER_STOP]="from all fwmark $IOA_OWNER_MARK prohibit"' \
        '-m cgroup --path "$cgroup"' \
        '-j MARK --set-xmark "$IOA_OWNER_MARK"' \
        'iptables -t nat -A POSTROUTING -m mark --mark "$IOA_OWNER_MARK"' \
        '-o "$physical_dev" -j MASQUERADE'
    do
        grep -Fq -- "$expected" "$SCRIPT" || {
            printf 'FAIL missing policy: %s\n' "$expected" >&2
            exit 1
        }
    done
    exec sudo -n unshare -rn env IN_NETNS=1 bash "$0"
fi

ip link add physical0 type dummy
ip link set physical0 up
ip addr add 192.0.2.2/24 dev physical0
ip link add tailscale0 type dummy
ip link set tailscale0 up
ip addr add 100.64.0.2/32 dev tailscale0
ip route add default via 192.0.2.1 dev physical0 table main
ip route add default dev tailscale0 table 52
ip rule add pref 400 fwmark 0x1000000/0xffffffff lookup main
ip rule add pref 401 fwmark 0x1000000/0xffffffff prohibit
ip rule add pref 5270 lookup 52

route=$(ip route get 203.0.113.1 mark 0x1000000)
[[ "$route" == *'dev physical0'* ]] || {
    echo "FAIL IOA underlay did not use physical0: $route" >&2
    exit 1
}
echo 'OK   IOA underlay uses the physical default'

ip route del default via 192.0.2.1 dev physical0 table main
if output=$(ip route get 203.0.113.1 mark 0x1000000 2>&1); then
    echo "FAIL IOA underlay leaked to Tailscale: $output" >&2
    exit 1
fi
[[ "$output" == *'Permission denied'* ]] || {
    echo "FAIL IOA underlay did not fail terminally: $output" >&2
    exit 1
}
echo 'OK   IOA underlay cannot fall through to the Tailscale exit node'
