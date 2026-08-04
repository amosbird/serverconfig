#!/usr/bin/env bash
set -euo pipefail

[ "$(id -u)" -eq 0 ] || {
    echo "SKIP requires root for a network namespace" >&2
    exit 77
}

[ -n "${HOST_NETNS_LINK:-}" ] && [ -n "${HOST_NETNS_INODE:-}" ] || {
    echo "REFUSE missing host namespace identity" >&2
    exit 1
}
current_link=$(readlink /proc/self/ns/net)
current_inode=$(stat -Lc %i /proc/self/ns/net)
if [ "$current_link" = "$HOST_NETNS_LINK" ] || [ "$current_inode" = "$HOST_NETNS_INODE" ]; then
    echo "REFUSE verified host network namespace" >&2
    exit 1
fi

ip link add tun0 type dummy
ip link set tun0 up
ip addr add 192.168.255.10/24 dev tun0
ip route add default dev tun0 table 400 metric 101
ip rule add pref 2500 fwmark 1 lookup 400

route=$(ip route get 203.0.113.1 mark 1)
[[ "$route" == *"dev tun0"* && "$route" == *"table 400"* ]] || {
    echo "FAIL marked lookup did not use table 400 and tun0: $route" >&2
    exit 1
}
echo "OK   marked IOA traffic uses gateway-free tun0 default"

json=$(ip -j route show table 400 default)
python3 - "$json" <<'PY'
import json
import sys

routes = json.loads(sys.argv[1])
assert len(routes) == 1, routes
route = routes[0]
assert route.get("dst") == "default", route
assert route.get("dev") == "tun0", route
assert "gateway" not in route, route
PY
echo "OK   IOA default exposes no gateway to netlink consumers"
