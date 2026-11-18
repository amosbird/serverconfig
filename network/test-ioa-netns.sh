#!/usr/bin/env bash
# Prove network/ioa-netns builds an iOA namespace that:
#   1. reaches the physical uplink through the host, masqueraded to the uplink address, and gets
#      the replies back even though the host's catch-all would send them into the exit node;
#   2. never leaves through the Tailscale exit node, even though the host's catch-all prefers it;
#   3. fails closed immediately (EHOSTUNREACH, not a timeout) when the physical route is gone;
#   4. carries the host's own iOA traffic (mark 0x1, literal 10/8, the tunnel network SmartDNS asks)
#      through tun0 inside the namespace, sourced from the tunnel address; without tun0 refuses it
#      instead of sending it out the uplink, and once `down` ran refuses it rather than leaking it;
#   5. is idempotent (`up` twice) and removed completely by `down`.
#
# Every run happens in a sandbox: new network, mount and PID namespaces, with /run/netns on a
# private tmpfs. The sandbox's own network namespace plays the host, so the real host's rules,
# tables and links are unreachable. When the sandbox's first process exits, the kernel kills every
# process in it, so no listener can outlive the test or hold its output open. Each sandbox also has
# a hard time limit.
#
# Usage: test-ioa-netns.sh [ioa-netns-script]   (the argument exists for mutation testing)
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT=$(realpath "${1:-$ROOT/network/ioa-netns}")
LIMIT=${TEST_LIMIT:-60}

fail() { echo "FAIL $*" >&2; exit 1; }
ok() { echo "OK   $*"; }

sandbox() {
    sudo -n timeout -k 5 "$LIMIT" \
        unshare --net --mount --pid --fork --kill-child --mount-proc --propagation private \
        env IOA_NETNS_SANDBOX=1 bash "$0" "$1"
}

if [ "${IOA_NETNS_SANDBOX:-}" != 1 ]; then
    sandbox "$SCRIPT"
    [ -z "${1:-}" ] || exit 0
    mutate() {
        local work rc=0
        work=$(mktemp)
        sed "$2" "$SCRIPT" >"$work"
        if cmp -s "$SCRIPT" "$work"; then rm -f "$work"; fail "mutant $1 did not change the script"; fi
        sandbox "$work" >/dev/null 2>&1 || rc=$?
        rm -f "$work"
        case $rc in
            0) fail "mutant $1 survived" ;;
            124 | 137) fail "mutant $1 hit the ${LIMIT}s limit instead of failing an assertion" ;;
        esac
    }
    mutate no-main-rule 's/reconcile_rule "\$RULE_PREF" "iif \$HOST_IF lookup main"/:/'
    mutate no-stop-rule 's/reconcile_rule "\$RULE_STOP_PREF" "iif \$HOST_IF prohibit"/:/'
    mutate no-masquerade 's/ masquerade$/ accept/'
    mutate no-forwarding 's/net.ipv4.conf.\$HOST_IF.forwarding=1/net.ipv4.conf.$HOST_IF.forwarding=0/'
    mutate no-return-rule 's/reconcile_rule "\$RULE_RETURN_PREF" "to \$NET lookup main"/:/'
    mutate down-keeps-rules 's/while "\$IP" -4 rule del pref "\$pref"/while false/'
    mutate no-tunnel-route 's/"\$IP" route replace default via "\$NS_ADDR" dev "\$HOST_IF" table "\$TUNNEL_TABLE"/:/'
    # No mutant drops the namespace's own ip_forward: on this kernel every new network namespace
    # starts with forwarding on (net.ipv4.ip_forward=1 even from a namespace that set it to 0), so
    # that mutant is equivalent here. The setting stays in the script because it is not guaranteed.
    mutate no-ns-masquerade 's/postrouting iifname "\$NS_IF" oifname != "\$NS_IF" masquerade/postrouting iifname "$NS_IF" oifname != "$NS_IF" accept/'
    # No down-keeps-link mutant: deleting the namespace destroys the veth pair with it, so skipping
    # `link del` is equivalent. Keeping the namespace itself is the meaningful failure.
    mutate down-keeps-ns 's/! ns_exists || "\$IP" netns del "\$NS"/true/'
    mutate down-keeps-nat 's/"\$NFT" delete table ip "\$NFT_TABLE"/true/'
    mutate no-tunnel-only 's/ meta l4proto tcp reject with tcp reset$/ accept/'
    mutate no-tunnel-icmp 's/ reject with icmp type admin-prohibited$/ accept/'
    mutate icmp-ratelimit 's/net.ipv4.icmp_ratelimit=0/net.ipv4.icmp_ratelimit=1000/'
    mutate no-underlay-rule 's/reconcile_rule "\$RULE_UNDERLAY_PREF" "iif \$HOST_IF lookup \$UNDERLAY_TABLE"/:/'
    mutate no-dns-redirect 's/th dport 53 dnat to \$HOST_ADDR$/th dport 53 accept/'
    mutate no-dns-to-smartdns 's/th dport 53 dnat to 127.0.0.1$/th dport 53 accept/'
    mutate no-ns-localnet 's/net.ipv4.conf.\$NS_IF.route_localnet=1/net.ipv4.conf.$NS_IF.route_localnet=0/'
    mutate no-host-localnet 's/net.ipv4.conf.\$HOST_IF.route_localnet=1/net.ipv4.conf.$HOST_IF.route_localnet=0/'
    mutate no-loopback-masquerade 's/postrouting ip saddr 127.0.0.0\/8 oifname "\$NS_IF" masquerade/postrouting ip saddr 127.0.0.0\/8 oifname "$NS_IF" accept/'
    mutate no-resolver-snat 's/snat ip to ip daddr$/accept/'
    ok "mutation checks: every mutant fails an assertion"
    exit 0
fi

# ---- inside the sandbox -------------------------------------------------------------------------

# The real host's /run/netns is a shared mount; without a private tmpfs on top, namespaces created
# here would appear in the host's `ip netns list`.
mkdir -p /run/netns
mount -t tmpfs tmpfs /run/netns
ip link set lo up

# The sandbox's own namespace is the host: an uplink to `up` (the internet) and a fake tailscale0
# to `ts` (the exit node). Both answer on 198.51.100.7:7000 and say which one they are.
ip netns add up
ip netns add ts
ip link add wl0 type veth peer name up0 netns up
ip link add tailscale0 type veth peer name ts0 netns ts
ip addr add 192.0.2.10/24 dev wl0
ip link set wl0 up
ip addr add 100.88.0.1/32 dev tailscale0
ip link set tailscale0 up
ip -n up link set lo up
ip -n up addr add 192.0.2.1/24 dev up0
ip -n up addr add 198.51.100.7/32 dev lo
ip -n up link set up0 up
ip -n up route add default via 192.0.2.10 dev up0
ip -n ts link set lo up
ip -n ts addr add 198.51.100.7/32 dev lo
ip -n ts link set ts0 up
ip -n ts route add default dev ts0
ip route add default via 192.0.2.1 dev wl0
# Tailscale's shape: table 52 carries everything and the catch-all rule prefers it to main.
ip route add default dev tailscale0 table 52
ip rule add pref 5270 lookup 52

listen() {
    ip netns exec "$1" python3 -c '
import socket, sys
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(("198.51.100.7", 7000)); s.listen(8)
while True:
    c, a = s.accept(); c.sendall((sys.argv[1] + " " + a[0] + "\n").encode()); c.close()
' "$1" </dev/null >/dev/null 2>&1 &
}
listen up
listen ts
# The office wire: a second uplink, used only while table 19 has a default.
ip netns add wire
ip link add en0 type veth peer name wr0 netns wire
ip addr add 192.0.3.10/24 dev en0
ip link set en0 up
ip -n wire link set lo up
ip -n wire addr add 192.0.3.1/24 dev wr0
ip -n wire addr add 198.51.100.7/32 dev lo
ip -n wire link set wr0 up
ip -n wire route add default via 192.0.3.10 dev wr0
listen wire
for ns in up ts wire; do
    for _ in $(seq 50); do
        ip netns exec "$ns" ss -Hltn 'sport = :7000' | grep -q . && break
        sleep 0.1
    done
    ip netns exec "$ns" ss -Hltn 'sport = :7000' | grep -q . || fail "fixture: $ns listener never came up"
done

# Prints "<side> <peer>" on success, "ERR <errno name>" on failure, "TIMEOUT" if nothing answers.
connect_from() {
    "$@" timeout 4 python3 -c '
import errno, socket
try:
    s = socket.create_connection(("198.51.100.7", 7000), timeout=2)
    print(s.recv(64).decode().strip())
except socket.timeout:
    print("TIMEOUT")
except OSError as e:
    print("ERR", errno.errorcode.get(e.errno, e.errno))
' || echo TIMEOUT
}
from_host() { connect_from env; }
from_ioa() { connect_from ip netns exec ioa; }
netns_script() {
    IOA_NETNS=ioa IOA_NETNS_HOST_IF=vio IOA_NETNS_NS_IF=vio-ns IOA_NETNS_TUNNEL_TABLE=400 \
        bash "$SCRIPT" "$@" >/dev/null
}
# SmartGateAgent's tunnel, inside the namespace: tun0 is a veth to `corp`, the intranet, which serves
# 10.20.30.40:7000 and a fakeip 100.12.0.2:7000 and only routes back to the tunnel address.
add_tunnel() {
    ip netns add corp
    ip link add tun0 netns ioa type veth peer name ct0 netns corp
    ip -n ioa addr add 192.168.255.10/24 dev tun0
    ip -n ioa link set tun0 up
    ip -n ioa route add 10.0.0.0/8 dev tun0
    ip -n ioa route add 100.12.0.0/16 dev tun0
    ip -n corp link set lo up
    ip -n corp addr add 192.168.255.1/24 dev ct0
    ip -n corp addr add 10.20.30.40/32 dev lo
    ip -n corp addr add 100.12.0.2/32 dev lo
    ip -n corp link set ct0 up
    listen_on corp 10.20.30.40 corp
    listen_on corp 100.12.0.2 fakeip
}
listen_on() {  # listen_on <netns> <addr> <name>
    ip netns exec "$1" python3 -c '
import socket, sys
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind((sys.argv[1], 7000)); s.listen(8)
while True:
    c, a = s.accept(); c.sendall((sys.argv[2] + " " + a[0] + "\n").encode()); c.close()
' "$2" "$3" </dev/null >/dev/null 2>&1 &
    for _ in $(seq 50); do
        ip netns exec "$1" ss -Hltn "src $2 and sport = :7000" | grep -q . && return 0
        sleep 0.1
    done
    fail "fixture: nothing listens on $2:7000 in $1"
}
# A host process reaching the intranet: by mark 0x1 (what SmartDNS-classified names and tinyproxy
# get) or by a literal 10/8 address (which rule 1150 sends to table ioa unmarked).
host_to() {  # host_to <addr> [mark]
    timeout 4 python3 -c '
import errno, socket, sys
try:
    s = socket.socket()
    if len(sys.argv) > 2: s.setsockopt(socket.SOL_SOCKET, socket.SO_MARK, int(sys.argv[2], 0))
    s.settimeout(2); s.connect((sys.argv[1], 7000))
    print(s.recv(64).decode().strip())
except socket.timeout:
    print("TIMEOUT")
except OSError as e:
    print("ERR", errno.errorcode.get(e.errno, e.errno))
' "$@" || echo TIMEOUT
}
# The host's SmartDNS stand-in on 127.0.0.1:53: answers each UDP datagram and TCP connection with
# "smartdns <proto>".
serve_dns() {
    python3 -c '
import socket, threading
u = socket.socket(socket.AF_INET, socket.SOCK_DGRAM); u.bind(("127.0.0.1", 53))
t = socket.socket(); t.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
t.bind(("127.0.0.1", 53)); t.listen(8)
def udp():
    while True:
        _, a = u.recvfrom(512); u.sendto(b"smartdns udp", a)
threading.Thread(target=udp, daemon=True).start()
while True:
    c, _ = t.accept(); c.sendall(b"smartdns tcp"); c.close()
' </dev/null >/dev/null 2>&1 &
    for _ in $(seq 50); do
        ss -Hltn 'src 127.0.0.1 and sport = :53' | grep -q . && return 0
        sleep 0.1
    done
    fail "fixture: the SmartDNS stand-in never listened"
}
ns_dns() {  # ns_dns udp|tcp: a query to 127.0.0.1:53 from inside the namespace
    ip netns exec ioa timeout 4 python3 -c '
import errno, socket, sys
try:
    if sys.argv[1] == "udp":
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM); s.settimeout(2)
        s.connect(("127.0.0.1", 53)); s.send(b"q")
    else:
        s = socket.create_connection(("127.0.0.1", 53), timeout=2)
    print(s.recv(64).decode().strip())
except socket.timeout:
    print("TIMEOUT")
except OSError as e:
    print("ERR", errno.errorcode.get(e.errno, e.errno))
' "$1" || echo TIMEOUT
}

out=$(from_host)
[ "$out" = "ts 100.88.0.1" ] || fail "fixture: the host's own traffic should take the exit node, got '$out'"

# The host's iOA policy as network-reconfigure installs it: mark 0x1 and literal 10/8 use table ioa,
# and a marked packet with no route in it is refused, never sent to the exit node.
# Numbered, never named: the sandbox shares the host's /etc, so naming a table would mean writing to
# the real rt_tables.
ip rule add pref 1150 fwmark 0x1 lookup 400
ip rule add pref 1150 to 10.0.0.0/8 fwmark 0 lookup 400
ip rule add pref 1150 to 192.168.255.0/24 fwmark 0 lookup 400
ip rule add pref 1151 fwmark 0x1 prohibit

netns_script up
out=$(from_ioa)
[ "$out" = "up 192.0.2.10" ] ||
    fail "namespace traffic should leave through the uplink masqueraded to 192.0.2.10, got '$out'"
ok "namespace traffic leaves through the uplink, masqueraded, while the host prefers the exit node"

add_tunnel
out=$(host_to 10.20.30.40)
[ "$out" = "corp 192.168.255.10" ] ||
    fail "a host connect to a literal 10/8 address should cross the tunnel from 192.168.255.10, got '$out'"
ok "the host reaches a literal intranet address through the tunnel in the namespace"
out=$(host_to 100.12.0.2 0x1)
[ "$out" = "fakeip 192.168.255.10" ] ||
    fail "a marked host connect to a fakeip should cross the tunnel, got '$out'"
ok "the host reaches a marked fakeip address through the tunnel in the namespace"
listen_on corp 192.168.255.1 resolver
out=$(host_to 192.168.255.1)
[ "$out" = "resolver 192.168.255.10" ] ||
    fail "an unmarked host connect to iOA's tunnel network (SmartDNS's upstream) should cross the tunnel, got '$out'"
ok "the host reaches iOA's tunnel network unmarked, as SmartDNS's ioa upstream does"
# SmartGateAgent's own resolver listens on the tunnel address in the namespace. SmartDNS asks it
# unmarked and must be seen from that same address, as it was while tun0 lived on the host.
listen_on ioa 192.168.255.10 agentdns
out=$(host_to 192.168.255.10)
[ "$out" = "agentdns 192.168.255.10" ] ||
    fail "the host should reach iOA's resolver on the tunnel address, seen from it, got '$out'"
ok "the host reaches iOA's own resolver on the tunnel address"

# iOA's own lookups go to 127.0.0.1:53, the host's SmartDNS. Nothing listens there in the namespace.
serve_dns
for proto in udp tcp; do
    out=$(ns_dns "$proto")
    [ "$out" = "smartdns $proto" ] ||
        fail "a $proto query to 127.0.0.1:53 in the namespace should reach the host's SmartDNS, got '$out'"
done
ok "lookups to 127.0.0.1:53 in the namespace reach the host's SmartDNS, over UDP and TCP"

# At an office desk the authorized wire carries iOA, ahead of main (network-reconfigure keeps its
# default in table 19 while 802.1X says authorized, and flushes it otherwise).
ip route add default via 192.0.3.1 dev en0 table 19
out=$(from_ioa)
[ "$out" = "wire 192.0.3.10" ] || fail "with an office underlay, the namespace should use the wire, got '$out'"
ip route flush table 19
[ "$(from_ioa)" = "up 192.0.2.10" ] || fail "with no office underlay, the namespace should use main"
ok "the namespace prefers the authorized office wire, and main without it"

netns_script up
[ "$(ip -4 rule show pref 900 | wc -l)" -eq 2 ] && [ "$(ip -4 rule show pref 901 | wc -l)" -eq 1 ] &&
    [ "$(ip -4 rule show pref 899 | wc -l)" -eq 1 ] ||
    fail "up twice left duplicate rules"
[ "$(from_ioa)" = "up 192.0.2.10" ] || fail "a second up broke the path"
ok "up is idempotent"

ip route del default via 192.0.2.1 dev wl0
out=$(from_ioa)
[ "$out" = "ERR EHOSTUNREACH" ] ||
    fail "with no physical route the namespace must be refused at once (EHOSTUNREACH), got '$out'"
ok "with no physical route the namespace fails closed at once"
ip route add default via 192.0.2.1 dev wl0
[ "$(from_ioa)" = "up 192.0.2.10" ] || fail "the path did not return with the physical route"
ok "the path returns with the physical route without re-running up"

# iOA stopped or logged out: tun0 is gone and the namespace's only route leads back to the host. The
# uplink also answers on the intranet address, so a leak would show up as "leak ...".
ip -n up addr add 10.20.30.40/32 dev lo
listen_on up 10.20.30.40 leak
ip -n ioa link del tun0
for dst in 10.20.30.40 192.168.255.1; do
    out=$(host_to "$dst")
    case $out in
        ERR*) ;;
        *) fail "with no tunnel, host traffic to $dst must be refused at once, got '$out'" ;;
    esac
done
# DNS to the tunnel resolver is UDP: the refusal has to arrive as ICMP, not as a timeout.
out=$(timeout 4 python3 -c '
import errno, socket
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM); s.settimeout(2)
s.connect(("192.168.255.1", 53))
try:
    s.send(b"x"); s.recv(64); print("ANSWER")
except socket.timeout:
    print("TIMEOUT")
except OSError as e:
    print("ERR", errno.errorcode.get(e.errno, e.errno))
' || echo TIMEOUT)
case $out in
    ERR*) ;;
    *) fail "with no tunnel, UDP to the tunnel resolver must be refused at once, got '$out'" ;;
esac
ok "with no tunnel, intranet traffic is refused, never sent out the physical uplink"

netns_script down
out=$(host_to 100.12.0.2 0x1)
[ "$out" = "ERR EACCES" ] ||
    fail "with the namespace down, marked intranet traffic must be refused (1151), got '$out'"
ok "with the namespace down, marked intranet traffic is refused, never sent to the exit node"
left=$(
    ip netns list | awk '$1 == "ioa"'
    ip -o link show vio 2>/dev/null || true
    ip -4 rule show pref 899
    ip -4 rule show pref 900
    ip -4 rule show pref 901
    ip route show table 400
    nft list tables | grep -w ioa_netns || true
)
[ -z "$left" ] || fail "down left state behind: $left"
netns_script down || fail "a second down failed"
ok "down removes everything and is idempotent"
