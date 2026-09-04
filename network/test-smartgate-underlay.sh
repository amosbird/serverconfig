#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
HOST_NETNS_LINK=$(readlink /proc/self/ns/net)
HOST_NETNS_INODE=$(stat -Lc %i /proc/self/ns/net)

sudo -n env \
    HOST_NETNS_LINK="$HOST_NETNS_LINK" \
    HOST_NETNS_INODE="$HOST_NETNS_INODE" \
    ROOT="$ROOT" \
    unshare --net --mount-proc bash <<'NS'
set -euo pipefail

current_link=$(readlink /proc/self/ns/net)
current_inode=$(stat -Lc %i /proc/self/ns/net)
if [ "$current_link" = "$HOST_NETNS_LINK" ] || [ "$current_inode" = "$HOST_NETNS_INODE" ]; then
    echo "REFUSE verified host network namespace" >&2
    exit 1
fi

route_device() {
    ip route get "$@" |
        awk '{for (i = 1; i < NF; i++) if ($i == "dev") {print $(i + 1); exit}}'
}

ip link set lo up
ip link add wlan0 type dummy
ip link add enp1s0 type dummy
ip link add tun0 type dummy
ip link add tailscale0 type dummy
ip link set wlan0 up
ip link set enp1s0 up
ip link set tun0 up
ip link set tailscale0 up
ip addr add 198.51.100.20/24 dev wlan0
ip addr add 192.0.2.20/24 dev enp1s0
ip addr add 192.168.255.10/24 dev tun0
ip route add default via 192.0.2.1 dev enp1s0 metric 900
ip route add default via 198.51.100.1 dev wlan0 metric 100
ip route add default via 192.0.2.1 dev enp1s0 table 19
ip route add default dev tailscale0 table 52
ip rule add priority 500 fwmark 0x80000/0xff0000 lookup main
ip rule add priority 5270 lookup 52

IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" route add default via 198.51.100.1 table 20
IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" route add default via 198.51.100.1 table 230
IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" rule add fwmark 2616 table 20
IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" rule add from 192.0.2.20 table 230
IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" \
    route add default via 192.168.255.1 dev tun0

[ "$(ip route show table 20 | sed -E 's/[[:space:]]+$//')" = \
    "default via 192.0.2.1 dev enp1s0" ]
[ "$(ip route show table 230 | sed -E 's/[[:space:]]+$//')" = \
    "default via 192.0.2.1 dev enp1s0" ]
ip rule show pref 1100 | grep -Eq 'fwmark (0x)?a38 lookup 20'
ip rule show pref 1200 | grep -Eq 'from 192\.0\.2\.20 lookup (230|ioa_src)'
[ "$(route_device 203.0.113.1 from 192.0.2.20 mark 0x80000)" = "wlan0" ]
[ "$(route_device 203.0.113.1 from 192.0.2.20 mark 0xa38)" = "enp1s0" ]
[ "$(route_device 203.0.113.1 from 192.0.2.20)" = "enp1s0" ]

echo 'OK   Tencent Ethernet overrides the lower-metric Wi-Fi default for IOA'
echo 'OK   Tailscale owner-marked traffic still follows main independently'

IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" rule del from 192.0.2.20 table 230
IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" rule del fwmark 2616 table 20
! ip rule show | grep -Eq 'fwmark (0x)?a38 lookup 20|from 192\.0\.2\.20 lookup (230|ioa_src)'
ip route flush table 230
ip route flush table 20
ip route flush table 19

IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" route add default via 198.51.100.1 table 20
IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" route add default via 198.51.100.1 table 230
IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" rule add fwmark 2616 table 20
IP_REAL=/usr/bin/ip "$ROOT/scripts/overrides/ip" rule add from 198.51.100.20 table 230

[ "$(ip route show table 20 | sed -E 's/[[:space:]]+$//')" = \
    "default via 198.51.100.1 dev wlan0" ]
[ "$(ip route show table 230 | sed -E 's/[[:space:]]+$//')" = \
    "default via 198.51.100.1 dev wlan0" ]
ip rule show pref 1100 | grep -Eq 'fwmark (0x)?a38 lookup 20'
ip rule show pref 1200 | grep -Eq 'from 198\.51\.100\.20 lookup (230|ioa_src)'
[ "$(route_device 203.0.113.1 from 198.51.100.20 mark 0x80000)" = "wlan0" ]
[ "$(route_device 203.0.113.1 from 198.51.100.20 mark 0xa38)" = "wlan0" ]
[ "$(route_device 203.0.113.1 from 198.51.100.20)" = "wlan0" ]

json=$(ip -j route show table 400 default)
python3 - "$json" <<'PY'
import json
import sys

routes = json.loads(sys.argv[1])
assert len(routes) == 1, routes
route = routes[0]
assert route.get("dev") == "tun0", route
assert "gateway" not in route, route
PY

echo 'OK   Wi-Fi gateway selects wlan0 without hard-coding it'
echo 'OK   table ioa remains gateway-free'
NS
