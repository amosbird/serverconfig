#!/usr/bin/env bash
# Fault injection for network-reconfigure, inside a network namespace.
#
#     sudo bash network/test-reconfigure.sh
#
# Nothing here touches the live routing table. Everything the script would
# change — rules, tables, iptables — is namespaced, so a test that breaks the
# network breaks only the namespace.
#
# This exists because the alternative was tried: injecting faults on the live
# machine and trusting the script to repair them. That is circular — if the
# script can repair it the test proves nothing, and if it cannot the machine
# goes offline. It went offline three times before this file existed.
#
# The script under test is copied verbatim, with only its *external* couplings
# stubbed: networkctl, systemctl, tailscale, dig, and the paths under /etc and
# /var. No routing logic is replaced, so what passes here is what runs.
set -uo pipefail

# Re-exec into a network namespace, building the stubbed copy *first*, as the
# invoking user. `unshare -r` maps the caller to nobody inside the namespace, so
# anything that has to read the repo or write /tmp must happen out here — a
# build that fails in there fails silently and the test then re-runs whatever
# copy was left over, reporting a pass for code that was never exercised.
REPO=/home/amos/git/serverconfig
SCRIPT=""   # set once $WORK exists
# Named, not numbered: `ip rule show` prints the name from rt_tables, so a
# numeric comparison here would never match what the kernel reports back.
UNDERLAY_TABLE="underlay"
CN_TABLE="cn"
CN_STAGE_TABLE="cn_stage"
pass=0; fail=0

ok()   { printf '  \033[32mOK\033[0m   %s\n' "$*"; pass=$((pass+1)); }
bad()  { printf '  \033[31mFAIL\033[0m %s\n' "$*"; fail=$((fail+1)); }
head_() { printf '\n\033[1m%s\033[0m\n' "$*"; }

# A namespace that looks enough like the real machine for the script to run:
# a "physical" link with a gateway, a DHCP-style resolver, and tunnel-owner sentinels.
OFFICE_RESOLVERS='10.76.9.15 21.7.193.132 21.7.193.156'

office_lease() {
    printf '   3 router %s\n' "${1:-10.76.76.193}"
    printf '   6 domain name server 21.7.193.132\n'
    printf '                        21.7.193.156\n'
    printf '                        10.76.9.15\n'
}

# The two verdicts the wired link is judged on, each a file so a test can change the network's mind
# between runs: the supplicant's authorization state and whether the gateway reaches the internet.
supplicant_says() { printf 'suppPortStatus=%s\n' "$1" > "$WORK/supp-status"; }
probe_verdict()   { printf '%s\n' "$1" > "$WORK/probe-verdict"; }

# The domains the shipped base config hands to iOA's tunnel resolver. Read from that config rather
# than listed, so the assertions cover whatever it says today.
tunnel_resolver_domains() {
    sed -n 's|^nameserver /\([^/]*\)/ioa$|\1|p' "$WORK/smartdns-base.conf"
}

# Reading ngnclient's state is fine; changing it is not. A restart stops iOA.bin, takes SmartGateAgent
# and the tunnel with it, and leaves the user at a login prompt.
ngnclient_disruptions() {
    grep -Ec '(^|[[:space:]])(try-restart|restart|start|stop|kill|reload)[[:space:]]+ngnclient' \
        "$WORK/systemctl-calls" 2>/dev/null || true
}

write_tool_stubs() {
    cat >"$WORK/wpa_cli" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$(dirname "$0")/wpa_cli-calls"
cat "$(dirname "$0")/supp-status" 2>/dev/null
EOF
    cat >"$WORK/probe" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$(dirname "$0")/probe-calls"
exit "$(cat "$(dirname "$0")/probe-verdict" 2>/dev/null || echo 1)"
EOF
    chmod 755 "$WORK/wpa_cli" "$WORK/probe"
}

setup() {
    ip link add wlan0 type dummy
    ip link set wlan0 up
    ip addr add 10.36.48.162/20 dev wlan0
    # Model gateways exposed both as a host scope-link route and as the default gateway.
    ip route add 10.36.48.1/32 dev wlan0 scope link
    ip route add default via 10.36.48.1 dev wlan0
    ip link add enp1s0 type veth peer name wired-peer
    ip link set wired-peer up
    ip link set enp1s0 up
    ip addr add 10.76.76.210/26 dev enp1s0
    ip link add tun0 type dummy
    ip link set tun0 up
    ip addr add 192.168.255.10/24 dev tun0
    ip link add owner0 type dummy
    ip link set owner0 up
    ip route add default via 192.168.255.1 dev tun0 table 400
    ip route add default dev owner0 table 20
    ip route add default dev owner0 table 230
    ip route add blackhole 203.0.113.0/24 table 102
    ip route add default dev tun0 table 52
    ip route add blackhole 198.18.0.0/15 table 52
    ip rule add fwmark 0xa38 lookup 20 pref 490
    ip rule add fwmark 0x80000/0xff0000 lookup main pref 5210
    ip rule add lookup 52 pref 5270
}

# The script is driven with its own helpers stubbed where they would reach
# outside the namespace: DNS, tailscale, smartdns, the routefile.
run_script() {
    FORCE="${1:-0}" NSTEST=1 NETWORK_RECONFIGURE_LOCKED=1 \
        IOA_CGROUP_PATHS_OVERRIDE= \
        WPA_CLI_OVERRIDE="$WORK/wpa_cli" PROBE_OVERRIDE="$WORK/probe" \
        bash "$SCRIPT" wlan0 2>&1 |
        grep -vE '^\+'
    local rc=${PIPESTATUS[0]}
    return "$rc"
}

# Run only for status when output is irrelevant. A run that aborts leaves the
# reset done and the rules half-built, so "did it come back clean" is distinct
# from "are the rules right" and needs its own check.
run_status() {
    FORCE="${1:-0}" NSTEST=1 NETWORK_RECONFIGURE_LOCKED=1 \
        IOA_CGROUP_PATHS_OVERRIDE= \
        WPA_CLI_OVERRIDE="$WORK/wpa_cli" PROBE_OVERRIDE="$WORK/probe" \
        bash "$SCRIPT" wlan0 >/dev/null 2>&1
    echo $?
}

# Move the namespace onto a different AP: new subnet, new gateway, and new
# resolvers. No RFC 8910 portal is advertised, so the public resolvers must not
# acquire a physical-route exception that bypasses the selected exit node.
roam_to() {
    local subnet="$1" gw="$2" dns1="$3" dns2="$4"
    ip addr flush dev wlan0
    ip addr add "$subnet" dev wlan0
    ip route replace default via "$gw" dev wlan0
    printf '   6 domain name server %s\n                        %s\n' \
        "$dns1" "$dns2" > "$WORK/lease"
}

band() { ip -4 rule show pref "$1" 2>/dev/null | sed 's/^[0-9]*:[[:space:]]*//' | sort; }
count() { ip -4 rule show pref "$1" 2>/dev/null | wc -l; }
routes() { ip route show table "$1" 2>/dev/null | wc -l; }
cn_entries() {
    ipset save cn_direct 2>/dev/null |
        awk '$1 == "add" {print $3 ($4 == "nomatch" ? " nomatch" : "")}' | sort
}
cn_contains() { ipset test cn_direct "$1" >/dev/null 2>&1; }

snapshot_owner_state() {
    local table pref
    for table in 20 230 52 400; do
        printf 'table %s|' "$table"
        ip route show table "$table" | sort | paste -sd';' -
    done
    for pref in 490 5210 5270; do
        printf 'pref %s|' "$pref"
        band "$pref" | paste -sd';' -
    done
}

snapshot_policy_state() {
    ip -4 rule show
    iptables-save
}

write_rt_tables() {
    awk '$1 ~ /^#/ || ($1 != 101 && $1 != 400 && $2 != "cn" && $2 != "ioa" && $2 != "cn_stage")' \
        /etc/iproute2/rt_tables > "$WORK/rt_tables"
    printf '%s\n' "$@" >> "$WORK/rt_tables"
}

check_fixed_table_conflict() {
    local name=$1 owner_before policy_before rt_tables_before rc
    shift
    write_rt_tables "$@"
    owner_before=$(snapshot_owner_state)
    policy_before=$(snapshot_policy_state)
    rt_tables_before=$(cat "$WORK/rt_tables")
    rc=$(run_status 1)
    [ "$rc" -ne 0 ] \
        && ok "$name fails closed" || bad "$name exited zero"
    [ "$(snapshot_owner_state)" = "$owner_before" ] &&
        [ "$(snapshot_policy_state)" = "$policy_before" ] &&
        [ "$(cat "$WORK/rt_tables")" = "$rt_tables_before" ] \
        && ok "$name preserves routing tables, rules, and iptables" \
        || bad "$name mutated routing tables, rules, or iptables"
}

snapshot_marking() {
    iptables -t mangle -S OUTPUT | grep -- '-j NETMODE_IOA' || true
    iptables -t mangle -S NETMODE_IOA 2>/dev/null || true
}

full_width_fwmark_rule() {
    local pref=$1 mark=$2 table=$3
    band "$pref" | grep -Eq \
        "^from all fwmark $mark(/0xffffffff)? lookup $table$"
}

route_table() {
    local args=() output rc table
    [ -n "${2:-}" ] && read -r -a args <<<"$2"
    output=$(ip route get "$1" "${args[@]}" 2>&1)
    rc=$?
    if [ "$rc" -ne 0 ]; then
        printf 'ERROR: ip route get %s failed (%s): %s\n' "$1" "$rc" "$output" >&2
        return "$rc"
    fi
    table=$(awk '{for (i=1; i<=NF; i++) if ($i == "table") {print $(i+1); exit}}' \
        <<<"$output")
    printf '%s\n' "${table:-main}"
}

# Build the stubbed copy from the real script, so the test can never drift from
# what is deployed. Every rewrite is deliberately one-for-one: if production
# moves or duplicates an external path, the harness must fail closed rather
# than execute an incompletely isolated copy.
build_under_test() {
    python3 - "$REPO/scripts/network-reconfigure" "$WORK" <<'EOF'
import sys
src = open(sys.argv[1]).read()
work = sys.argv[2]

def replace_once(old, new):
    count = src.count(old)
    if count != 1:
        raise SystemExit("expected exactly one occurrence of %r, found %d" % (old, count))
    return src.replace(old, new)

stub = """
networkctl() {
    if [ "${1:-}" = dhcp-lease ] && [[ "${2:-}" == enp* ]]; then
        cat WORKDIR/wired-lease
    else
        cat WORKDIR/lease
    fi
}
systemctl()  {
    printf '%s\n' "$*" >> WORKDIR/systemctl-calls
    return 0
}
tailscale()  { return 1; }
logger()     { printf '%s\n' "$*" >> WORKDIR/logger; return 0; }
dig()        { return 9; }
ip() {
    local batch
    printf '%s\n' "$*" >> WORKDIR/ip-calls
    if [ "${1:-}" = -batch ]; then
        if [ "${2:--}" = - ]; then
            batch=$(cat)
            printf '%s\n' "$batch" >> WORKDIR/ip-batch-commands
            command ip "$@" <<<"$batch"
            return
        fi
        cat "$2" >> WORKDIR/ip-batch-commands
    fi
    command ip "$@"
}
iptables() {
    if [ "${FAIL_NEXT_MARK:-0}" = 1 ] && [ " $* " = " -t mangle -A NETMODE_IOA_NEXT -m set --match-set ioa dst -j MARK --set-xmark 0x1/0xffffffff " ]; then
        return 42
    fi
    local rc
    command iptables "$@"
    rc=$?
    if [ "$rc" -eq 0 ] && [ "${AUDIT_CN_NAT_GAP:-0}" = 1 ] &&
       [[ " $* " == *" -t nat "* ]] && [[ " $* " == *" POSTROUTING "* ]]; then
        if [ "$(command iptables -t nat -S POSTROUTING 2>/dev/null |
            grep -Ec -- '--mark 0x2(/0xffffffff)? -j MASQUERADE$')" -eq 0 ]; then
            : > "${NAT_GAP_FILE:?}"
        fi
    fi
    return "$rc"
}
iptables-restore() {
    local rules before after rc
    rules=$(cat)
    if [ "${FAIL_MARK_RESTORE:-0}" = 1 ]; then
        before=$(iptables-save -t mangle)
        rules=${rules/COMMIT/-A NETMODE_IOA_MISSING -j ACCEPT$'\n'COMMIT}
        set +e
        command iptables-restore "$@" <<<"$rules" 2>/dev/null
        rc=$?
        set -e
        after=$(iptables-save -t mangle)
        [ "$before" = "$after" ] && : > WORKDIR/failed-restore-was-atomic
        return "${rc:-1}"
    fi
    command iptables-restore "$@" <<<"$rules"
}
STATE_FILE_OVERRIDE=WORKDIR/last-applied
CN_STATE_FILE_OVERRIDE=WORKDIR/cn-last-applied
CACHE_DIR_OVERRIDE=WORKDIR/cache
IOA_ENV_STATE_OVERRIDE=WORKDIR/ioa-environment
RT_TABLES_OVERRIDE=WORKDIR/rt_tables
TMPDIR_OVERRIDE=WORKDIR/tmp
"""
marker = 'IFACE="${1:-wlan0}"'
if src.count(marker) != 1:
    raise SystemExit("expected exactly one stub insertion marker, found %d" % src.count(marker))
i = src.index(marker)
src = src[:i] + stub.replace('WORKDIR', work) + src[i:]
for old, new in [
    ('exec flock -w 60 /run/lock/network-reconfigure.lock "$0" "$@"',
     'exec flock -w 60 "%s/reconf.lock" "$0" "$@"' % work),
    ('CACHE_DIR="/var/lib/network-reconfigure"',
     'CACHE_DIR="${CACHE_DIR_OVERRIDE:-/var/lib/network-reconfigure}"'),
    ('STATE_FILE="$CACHE_DIR/last-applied"',
     'STATE_FILE="${STATE_FILE_OVERRIDE:-$CACHE_DIR/last-applied}"'),
    ('CN_STATE_FILE="$CACHE_DIR/cn-last-applied"',
     'CN_STATE_FILE="${CN_STATE_FILE_OVERRIDE:-$CACHE_DIR/cn-last-applied}"'),
    ('ROUTEFILE="/home/amos/.routefile"', 'ROUTEFILE="%s/routefile"' % work),
    ('CN_EXCLUDE_FILE="/home/amos/git/serverconfig/network/cn-exclude.conf"',
     'CN_EXCLUDE_FILE="%s/cn-exclude.conf"' % work),
    # OFFICE_SRC points at a *copy* of the shipped fragment rather than a hand-written stub. The stub
    # is what let the intranet bootstrap override slip in: it never had those lines, so the assertion
    # that forbids them passed while the shipped file broke iOA login. The copy is needed because
    # `unshare -r` runs the namespace as nobody, which cannot read anything under /home/amos.
    ('OFFICE_SRC="/home/amos/git/serverconfig/network/smartdns/office.conf"',
     'OFFICE_SRC="%s/office.conf"' % work),
    ('OFFICE_DST="/etc/smartdns/office.conf"', 'OFFICE_DST="%s/office-dst.conf"' % work),
    ('SMARTDNS_BASE="/etc/smartdns/smartdns.conf"',
     'SMARTDNS_BASE="%s/smartdns-base.conf"' % work),
    ('IOA_RESOLVERS_DST="/etc/smartdns/ioa-resolvers.conf"',
     'IOA_RESOLVERS_DST="%s/ioa-resolvers.conf"' % work),
    ('INTRANET_RESOLVER_CACHE="/var/lib/network-reconfigure/intranet-resolvers"',
     'INTRANET_RESOLVER_CACHE="%s/intranet-resolvers"' % work),
    ('write_if_changed /etc/smartdns/dhcp-dns.conf ' + '\\' + '\n',
     'write_if_changed %s/dhcp-dns.conf ' % work + '\\' + '\n'),
    ('write_if_changed /etc/smartdns/dhcp-dns.conf "# No captive portal DNS"',
     'write_if_changed %s/dhcp-dns.conf "# No captive portal DNS"' % work),
]:
    src = replace_once(old, new)
open(work + '/nr-under-test', 'w').write(src)
EOF
    # `python3 ... <<EOF` is the last command only by accident; make the failure
    # explicit. A build that fails silently is how this harness once reported
    # twelve passes for a script whose delete path had been removed.
    [ -s "$WORK/nr-under-test" ] || return 1
    install -d -m 777 "$WORK/tmp"
    chmod 755 "$WORK/nr-under-test"
    write_rt_tables '19 wired_underlay' '102 kwai'
    # Same shape as the real ~/.routefile: `ip -batch` commands, not bare route
    # specs. The earlier fixture omitted the `route add` verb, so every batch
    # was rejected and the cn table was never created — and no test noticed,
    # because none of them looked at the cn table.
    printf 'route add 1.0.1.0/24 via GATEWAY table cn\n'  > "$WORK/routefile"
    printf 'route add 1.0.2.0/23 via GATEWAY table cn\n' >> "$WORK/routefile"
    printf 'route add 10.20.0.0/16 via GATEWAY table cn\n' >> "$WORK/routefile"
    printf 'route add 100.12.34.0/24 via GATEWAY table cn\n' >> "$WORK/routefile"
    printf '# Explicit CN bypasses\n193.112.78.32/32\n' > "$WORK/cn-exclude.conf"
    # The lease is a file so a test can hand out a different resolver, which is
    # what a roam onto another AP actually does. Two servers on a continuation
    # line, because that wrapping is what the awk state machine exists for.
    printf '   6 domain name server 202.152.254.230\n                        202.152.254.65\n 114 captive portal     https://login.hotel.test/api\n' \
        > "$WORK/lease"
    # The office resolvers arrive in the wired lease, like they do in the real one, so the office
    # fragment and its pins follow whichever site the laptop is plugged into.
    office_lease > "$WORK/wired-lease"
    # Start as the authorized office port whose gateway has no way out, which is what the real one is.
    supplicant_says Authorized
    probe_verdict 1
    chmod 644 "$WORK"/*
    write_tool_stubs
    chmod 777 "$WORK/tmp"
    chmod 755 "$WORK/nr-under-test"
}

main() {
    setup
    local owner_before route_error route_rc table pref
    owner_before=$(snapshot_owner_state)

    for table in 20 230 52 400; do
        if grep -Eq "^table $table\|.+" <<<"$owner_before"; then
            ok "owner snapshot includes table $table"
        else
            bad "owner snapshot omitted or emptied table $table"
        fi
    done
    for pref in 490 5210 5270; do
        if grep -Eq "^pref $pref\|.+" <<<"$owner_before"; then
            ok "owner snapshot includes pref $pref"
        else
            bad "owner snapshot omitted or emptied pref $pref"
        fi
    done

    head_ "fixed routing table registration conflicts"
    check_fixed_table_conflict 'table 400 bound to foreign name' \
        '400 foreign' '102 kwai'
    local duplicate_owner_before duplicate_rc
    write_rt_tables '101 cn' '400 ioa' '400 ioa' '102 kwai'
    duplicate_owner_before=$(snapshot_owner_state)
    duplicate_rc=$(run_status 1)
    if [ "$duplicate_rc" -eq 0 ] &&
       [ "$(awk '$1 == 400 && $2 == "ioa" {count++} END {print count + 0}' \
           "$WORK/rt_tables")" -eq 1 ] &&
       [ "$(awk '$2 == "cn" || $2 == "cn_stage" {count++} END {print count + 0}' \
           "$WORK/rt_tables")" -eq 0 ]; then
        ok "duplicate exact ioa mapping is normalized"
    else
        bad "duplicate exact ioa mapping was not normalized (rc=$duplicate_rc)"
    fi
    [ "$(snapshot_owner_state)" = "$duplicate_owner_before" ] \
        && ok "duplicate ioa normalization preserves tunnel-owner state" \
        || bad "duplicate ioa normalization mutated tunnel-owner state"

    local fixed_owner_before fixed_rc
    write_rt_tables '19 wired_underlay' '102 kwai'
    fixed_owner_before=$(snapshot_owner_state)
    fixed_rc=$(run_status 1)
    if [ "$fixed_rc" -eq 0 ] &&
       [ "$(awk '$1 == 101 && $2 == "cn" {count++} END {print count + 0}' "$WORK/rt_tables")" -eq 0 ] &&
       [ "$(awk '$2 == "cn_stage" {count++} END {print count + 0}' "$WORK/rt_tables")" -eq 0 ] &&
       [ "$(awk '$1 == 400 && $2 == "ioa" {count++} END {print count + 0}' "$WORK/rt_tables")" -eq 1 ]; then
        ok "unused fixed table IDs register once and leave retired CN tables unregistered"
    else
        bad "normal fixed table registration failed (rc=$fixed_rc)"
    fi
    [ "$(snapshot_owner_state)" = "$fixed_owner_before" ] \
        && ok "fixed table registration preserves SmartGateAgent table 400 contents" \
        || bad "fixed table registration changed SmartGateAgent table 400 contents"

    head_ "wired underlay advertisement"
    ipset create ioa_intranet hash:net
    ipset add ioa_intranet 9.0.0.0/8
    run_script 1 >/dev/null
    if ipset list ioa_intranet >/dev/null 2>&1; then
        bad "retired ioa_intranet set survives reconciliation"
    else
        ok "reconciliation removes the retired ioa_intranet set"
    fi
    if ip route show table 19 | sed -E 's/[[:space:]]+$//' | grep -Fqx \
            'default via 10.76.76.193 dev enp1s0 onlink'; then
        ok "wired DHCP gateway is advertised in table 19"
    else
        bad "table 19 does not contain the wired DHCP gateway: $(ip route show table 19)"
    fi
    # Recorded, not acted on. Restarting ngnclient logs the user out of iOA and drops the tunnel, and
    # iOA refetches its scene by itself every five minutes and on every network change.
    if [ "$(cat "$WORK/ioa-environment")" = office ] &&
       [ "$(ngnclient_disruptions)" -eq 0 ]; then
        ok "entering office records the edge without touching iOA"
    else
        bad "office entry disturbed iOA $(ngnclient_disruptions) times"
    fi
    if [ "$(band 400 | grep -Fxc \
            'from all fwmark 0x1000000 lookup wired_underlay')" -eq 1 ] &&
       [ "$(route_table 10.30.1.1)" = ioa ]; then
        ok "authorized office prefers wired for iOA underlay while business stays tunneled"
    else
        bad "authorized office mixed underlay and payload policy: owner=$(band 400)"
    fi
    for mapping in \
        'nameserver /smartgate.oa.tencent.com/office' \
        'nameserver /sgw.woa.com/office' \
        'nameserver /ioa.tencent.com/office' \
        'nameserver /woa.com/office'
    do
        if grep -Fqx "$mapping" "$WORK/office-dst.conf"; then
            ok "office DNS selects internal bootstrap: $mapping"
        else
            bad "office DNS omitted internal bootstrap: $mapping"
        fi
    done
    # iOA starts its DNS server only in the EXTRA and OVERSEA scenes, so on this LAN the `ioa` group
    # has no upstream and every domain pointed at it resolves nowhere. The office fragment has to
    # claim all of them, derived from the base config: a hand-kept list is what left
    # mirrors.tencent.com and eight others dead from an office desk on 2026-09-21.
    local domain tunnel_domains=0 unclaimed=''
    while read -r domain; do
        tunnel_domains=$((tunnel_domains + 1))
        grep -Fqx "nameserver /$domain/office" "$WORK/office-dst.conf" ||
            unclaimed="$unclaimed $domain"
    done < <(tunnel_resolver_domains)
    if [ "$tunnel_domains" -gt 0 ] && [ -z "$unclaimed" ]; then
        ok "office DNS claims all $tunnel_domains base tunnel-resolver domains"
    else
        bad "office DNS leaves tunnel-resolver domains unresolvable:${unclaimed:- none found}"
    fi
    # iOA's own resolver NXDOMAINs every name outside its forward policy, so the group needs a real
    # intranet resolver. Learn it here, from the lease, while there is a lease to learn it from — but
    # only the addresses policy routes into the tunnel. This lease also advertises 21.7.x, which lands
    # on Tailscale off the office LAN and would send intranet DNS out of the exit node.
    local learned=1
    if ! grep -Fqx 'server 10.76.9.15 -group ioa -exclude-default-group -interface tun0' \
            "$WORK/ioa-resolvers.conf" 2>/dev/null; then
        learned=0
    fi
    if grep -Eq '^server 21\.' "$WORK/ioa-resolvers.conf" 2>/dev/null ||
       grep -Eq '^21\.' "$WORK/intranet-resolvers" 2>/dev/null; then
        learned=0
    fi
    if [ "$learned" -eq 1 ]; then
        ok "the office lease teaches the IOA group only tunnel-reachable resolvers"
    else
        bad "learned resolvers are wrong: $(cat "$WORK/ioa-resolvers.conf" 2>/dev/null)"
    fi
    local initial_nat
    initial_nat=$(iptables -t nat -S POSTROUTING)
    if grep -Fq -- '-o enp1s0 -m mark --mark 0x1000000 -j MASQUERADE' <<<"$initial_nat" &&
       grep -Fq -- '-o tun0 -m mark --mark 0x1 -j MASQUERADE' <<<"$initial_nat"; then
        ok "office owner uses wired NAT while business payload keeps tunnel NAT"
    else
        bad "office NAT does not follow the selected underlay: $initial_nat"
    fi
    # A pin to main is worthless while main has no route to the resolver: this link deliberately
    # contributes no default route, so each resolver needs its own way through the wired gateway.
    for resolver in $OFFICE_RESOLVERS; do
        if ip -4 route show table main dev enp1s0 | grep -Fq "$resolver via 10.76.76.193"; then
            ok "office resolver $resolver reaches main through the wired gateway"
        else
            bad "office resolver $resolver has no route through the wired gateway"
        fi
        if grep -Fqx "server $resolver -group office -exclude-default-group" \
                "$WORK/office-dst.conf"; then
            ok "office DNS adopts lease resolver $resolver"
        else
            bad "office DNS omits lease resolver $resolver"
        fi
    done
    # Another site hands out other resolvers; last site's host routes must not survive the move.
    printf '   3 router 10.76.76.193\n   6 domain name server 10.76.9.15\n' > "$WORK/wired-lease"
    run_script 1 >/dev/null
    if ip -4 route show table main dev enp1s0 | grep -Fq '21.7.193.132 via'; then
        bad "resolver from the previous site survived the lease change"
    else
        ok "resolver from the previous site is withdrawn on a lease change"
    fi
    office_lease > "$WORK/wired-lease"
    run_script 1 >/dev/null

    head_ "the office LAN is the link that authenticated us"
    # A port that hands out a lease but refuses to authorize us is not the office LAN, however much
    # its lease looks like one. This is the state the real port was in on 2026-09-20, and calling it
    # the office LAN is what pointed iOA's bootstrap at an address it could not reach.
    supplicant_says Unauthorized
    run_script 1 >/dev/null
    if grep -Fq 'Not on the office LAN' "$WORK/office-dst.conf"; then
        ok "an unauthorized port is not treated as the office LAN"
    else
        bad "an unauthorized port was treated as the office LAN"
    fi
    if [ -z "$(ip route show table 19)" ]; then
        ok "an unauthorized port gets no underlay advertisement"
    else
        bad "an unauthorized port was advertised in table 19: $(ip route show table 19)"
    fi
    # The exit edge is where the restart did real damage: unplugging on 2026-09-21 killed a tunnel that
    # had already picked up the EXTRA scene, and the session did not survive the service restart.
    if [ "$(cat "$WORK/ioa-environment")" = external ] &&
       [ "$(ngnclient_disruptions)" -eq 0 ]; then
        ok "leaving office records the edge without dropping the tunnel"
    else
        bad "office exit disturbed iOA $(ngnclient_disruptions) times"
    fi
    if [ "$(band 400 | grep -Fxc 'from all fwmark 0x1000000 lookup main')" -eq 1 ]; then
        ok "unauthorized wired falls back to ordinary owner main and tunnel business policy"
    else
        bad "unauthorized wired retained office routing: owner=$(band 400) office=$(band 1125)"
    fi
    for mapping in \
        'nameserver /smartgate.oa.tencent.com/office' \
        'nameserver /sgw.woa.com/office' \
        'nameserver /ioa.tencent.com/office' \
        'nameserver /woa.com/office'
    do
        if grep -Fqx "$mapping" "$WORK/office-dst.conf"; then
            bad "unauthorized wired retained internal bootstrap: $mapping"
        fi
    done
    # Off the office LAN those domains belong back on the tunnel resolver, which is exactly where
    # iOA does run its DNS server. Retaining the overrides would point them at resolvers that the
    # laptop can no longer reach.
    while read -r domain; do
        if grep -Fqx "nameserver /$domain/office" "$WORK/office-dst.conf"; then
            bad "unauthorized wired retained tunnel-resolver override: $domain"
        fi
    done < <(tunnel_resolver_domains)
    # The learned resolvers are the half that must survive leaving: iOA proxies them through the
    # tunnel, and they are the only upstream that answers names outside iOA's own forward policy.
    local kept=1
    grep -Fqx 'server 10.76.9.15 -group ioa -exclude-default-group -interface tun0' \
        "$WORK/ioa-resolvers.conf" 2>/dev/null || kept=0
    if [ "$kept" -eq 1 ]; then
        ok "learned intranet resolvers stay available off the office LAN"
    else
        bad "leaving office discarded the learned intranet resolvers"
    fi
    local unauthorized_nat
    unauthorized_nat=$(iptables -t nat -S POSTROUTING)
    if grep -Fq -- '-o wlan0 -m mark --mark 0x1000000 -j MASQUERADE' <<<"$unauthorized_nat" &&
       grep -Fq -- '-o tun0 -m mark --mark 0x1 -j MASQUERADE' <<<"$unauthorized_nat"; then
        ok "unauthorized wired restores the internet owner underlay and keeps tunnel payload"
    else
        bad "unauthorized wired retained stale office NAT: $unauthorized_nat"
    fi
    for resolver in $OFFICE_RESOLVERS; do
        if band 1000 | grep -Fq "to $resolver lookup main"; then
            bad "an unauthorized port retained office resolver pin $resolver"
        fi
    done
    # A link with no supplicant at all — a tether, a hotel port — fails for the same reason.
    rm -f "$WORK/supp-status"
    run_script 1 >/dev/null
    if grep -Fq 'Not on the office LAN' "$WORK/office-dst.conf"; then
        ok "a link with no supplicant is not treated as the office LAN"
    else
        bad "a link with no supplicant was treated as the office LAN"
    fi
    [ "$(ngnclient_disruptions)" -eq 0 ] \
        && ok "no reconciliation so far has disturbed ngnclient" \
        || bad "reconciliation disturbed ngnclient $(ngnclient_disruptions) times"

    head_ "a wired default route has to be earned"
    # Still unauthenticated, and now the gateway answers a public anchor: an ordinary network that is
    # the only way out while it is plugged in, so it gets main's default route.
    probe_verdict 0
    run_script 1 >/dev/null
    if ip -4 route show table main default | grep -Fq 'default via 10.76.76.193 dev enp1s0 metric 100'; then
        ok "a gateway that reaches the internet earns main's default route"
    else
        bad "a verified wired gateway got no default route: $(ip -4 route show table main default)"
    fi
    if [ "$(grep -c -- "--target 216.239.32.117 --port 80" "$WORK/probe-calls")" -ge 1 ]; then
        ok "the wired gateway is judged by a probe across it"
    else
        bad "no probe was placed across the wired gateway"
    fi
    if ip -4 route show table main | grep -Fq '216.239.32.117'; then
        bad "the probe's host route was left behind: $(ip -4 route show table main | grep 216.239)"
    else
        ok "the probe withdraws its own host route"
    fi
    # The probe could not be placed, so it measured nothing. Installing a default route on that would
    # be installing one on no evidence.
    probe_verdict 2
    run_script 1 >/dev/null
    if ip -4 route show table main default | grep -Fq 'dev enp1s0'; then
        bad "an unplaceable probe was read as success: $(ip -4 route show table main default)"
    else
        ok "an unplaceable probe does not earn a default route"
    fi
    probe_verdict 1
    run_script 1 >/dev/null
    if ip -4 route show table main default | grep -Fq 'dev enp1s0'; then
        bad "a failed probe left a wired default route: $(ip -4 route show table main default)"
    else
        ok "a gateway that reaches nothing loses main's default route"
    fi
    supplicant_says Authorized
    run_script 1 >/dev/null
    if grep -Fq 'nameserver /oa.com/office' "$WORK/office-dst.conf"; then
        ok "re-authorization restores the office fragment"
    else
        bad "re-authorization did not restore the office fragment"
    fi
    if ip -4 route show table main default | grep -Fq 'dev enp1s0'; then
        bad "the authorized office gateway kept a default route it cannot serve"
    else
        ok "the authorized office gateway still owns no default route"
    fi
    # The recorded environment follows authorization in both directions and never asks systemd for
    # anything, however many times the edge is crossed.
    local crossings=0 recorded_correctly=1 want
    for want in external office external office; do
        case $want in
            office)   supplicant_says Authorized ;;
            external) supplicant_says Unauthorized ;;
        esac
        run_script 1 >/dev/null
        crossings=$((crossings + 1))
        [ "$(cat "$WORK/ioa-environment")" = "$want" ] || recorded_correctly=0
    done
    if [ "$recorded_correctly" -eq 1 ] && [ "$(ngnclient_disruptions)" -eq 0 ]; then
        ok "$crossings office edges recorded, none of them restarting iOA"
    else
        bad "edge recording drifted: state=$(cat "$WORK/ioa-environment") disturbances=$(ngnclient_disruptions)"
    fi
    supplicant_says Authorized
    run_script 1 >/dev/null
    local wired_owner_before
    wired_owner_before=$(snapshot_owner_state)
    ip link set enp1s0 down
    local wired_loss_rc
    wired_loss_rc=$(run_status 1)
    [ "$wired_loss_rc" -eq 0 ] \
        && ok "wired loss reconciliation completes" \
        || bad "wired loss reconciliation exited $wired_loss_rc"
    [ -z "$(ip route show table 19)" ] \
        && ok "wired loss removes table 19 advertisement" \
        || bad "wired loss retained table 19: $(ip route show table 19)"
    [ "$(snapshot_owner_state)" = "$wired_owner_before" ] \
        && ok "wired loss preserves tunnel-owned state" \
        || bad "wired loss changed tunnel-owned state"
    if grep -Fq 'nameserver /ioa.tencent.com/office' "$WORK/office-dst.conf"; then
        bad "wired loss retained office bootstrap DNS mappings"
    else
        ok "wired loss removes office bootstrap DNS mappings"
    fi
    for resolver in $OFFICE_RESOLVERS; do
        if band 1000 | grep -Fq "to $resolver lookup main"; then
            bad "wired loss retained office resolver pin $resolver"
        else
            ok "wired loss removes office resolver pin $resolver"
        fi
    done
    ip link set enp1s0 up
    office_lease 10.76.76.194 > "$WORK/wired-lease"
    run_script 1 >/dev/null
    if ip route show table 19 | sed -E 's/[[:space:]]+$//' | grep -Fqx \
            'default via 10.76.76.194 dev enp1s0 onlink'; then
        ok "wired gateway change replaces table 19 advertisement"
    else
        bad "wired gateway change did not converge: $(ip route show table 19)"
    fi
    office_lease > "$WORK/wired-lease"
    run_script 1 >/dev/null

    route_error=$(route_table invalid-destination 2>&1)
    route_rc=$?
    if [ "$route_rc" -ne 0 ] && [[ "$route_error" == ERROR:* ]]; then
        ok "route lookup errors are distinguishable from main-table hits"
    else
        bad "route lookup error reported rc=$route_rc output=$route_error"
    fi

    head_ "baseline"
    if ip route show table main scope link | grep -Fq '10.36.48.1 dev wlan0'; then
        ok "fixture exposes the gateway as a scope-link host route"
    else
        bad "fixture lacks a scope-link gateway host route"
    fi
    local baseline_rc
    baseline_rc=$(run_status 1)
    if [ "$baseline_rc" -eq 0 ]; then
        ok "forced run accepts duplicate desired gateway inputs"
    else
        bad "forced run exited $baseline_rc for duplicate desired gateway inputs"
    fi
    if [ "$(band 1000 | grep -Fxc 'from all to 10.36.48.1 lookup main')" -eq 1 ]; then
        ok "duplicate desired gateway inputs converge to one canonical rule"
    else
        bad "gateway rule did not converge uniquely: $(band 1000)"
    fi
    if [ "$(band 1000 |
        grep -Fxc 'from all lookup main suppress_prefixlength 0')" -eq 1 ]; then
        ok "live connected routes always override stale IOA 10/8 policy"
    else
        bad "priority 1000 lacks the live connected-route lookup: $(band 1000)"
    fi
    for resolver in $OFFICE_RESOLVERS; do
        if [ "$(band 1000 | grep -Fxc "from all to $resolver lookup main")" -eq 1 ]; then
            ok "active office resolver $resolver is pinned to main"
        else
            bad "active office resolver $resolver is not pinned to main"
        fi
    done
    run_script 1 >/dev/null
    [ "$(awk '$2 == "cn" || $2 == "cn_stage" {count++} END {print count + 0}' "$WORK/rt_tables")" -eq 0 ] \
        && ok "retired CN routing tables stay unregistered" \
        || bad "retired CN routing tables were registered: $(grep -E '[[:space:]](cn|cn_stage)$' "$WORK/rt_tables")"
    ip route show table 102 | grep -Fq 'blackhole 203.0.113.0/24' \
        && ok "foreign kwai staging sentinel is untouched" \
        || bad "foreign kwai staging sentinel was changed"

    head_ "CN rebuild gating"
    ip route replace 1.0.1.0/24 via 192.168.255.1 dev tun0 table 101 2>/dev/null || true
    run_script 0 >/dev/null
    if [ -z "$(ip route show table cn 2>/dev/null)" ] &&
       [ -z "$(ip route show table cn_stage 2>/dev/null)" ] &&
       [ -z "$(ip route show table 101 2>/dev/null)" ]; then
        ok "legacy gateway-coupled CN tables are retired"
    else
        bad "legacy CN route survived reconciliation: $(ip route show table 101 2>/dev/null)"
    fi
    local cn_before
    cn_before=$(cn_entries)
    run_script 1 >/dev/null
    if [ "$(cn_entries)" = "$cn_before" ]; then
        ok "ordinary FORCE leaves the healthy CN classifier unchanged"
    else
        bad "ordinary FORCE mutated the healthy CN classifier"
    fi

    ipset del cn_direct 1.0.1.0/24
    FORCE_CN=1 run_script 0 >/dev/null
    if cn_contains 1.0.1.1; then
        ok "FORCE_CN rebuilds the CN classifier"
    else
        bad "FORCE_CN did not rebuild the CN classifier"
    fi

    local cn_state_before
    cn_state_before=$(cat "$WORK/cn-last-applied")
    ipset del cn_direct 1.0.1.0/24
    ipset add cn_direct 198.18.0.0/15
    run_script 0 >/dev/null
    if cn_contains 1.0.1.1 && ! cn_contains 198.18.0.1; then
        ok "same-count CN set drift triggers exact repair"
    else
        bad "same-count CN set drift survived reconciliation"
    fi

    cn_state_before=$(cat "$WORK/cn-last-applied")
    printf '# Explicit CN bypasses\n10.20.1.0/24\n' > "$WORK/cn-exclude.conf"
    run_script 0 >/dev/null
    if ! cn_contains 10.20.1.1 && cn_contains 10.20.2.1; then
        ok "CN exclusions are atomic nomatch entries"
    else
        bad "CN exclusion change was not reconciled"
    fi
    [ "$(cat "$WORK/cn-last-applied")" != "$cn_state_before" ] \
        && ok "CN exclusion hash is recorded in state" \
        || bad "CN exclusion state did not change"

    cn_state_before=$(cat "$WORK/cn-last-applied")
    printf '10.20.1.0/24\n10.20.1.0/24\n' > "$WORK/cn-exclude.conf"
    run_status 0 >/dev/null
    [ "$(cat "$WORK/cn-last-applied")" = "$cn_state_before" ] \
        && ok "duplicate CN exclusion preserves state" \
        || bad "duplicate CN exclusion advanced state"
    printf '# Explicit CN bypasses\n193.112.78.32/32\n' > "$WORK/cn-exclude.conf"

    cn_state_before=$(cat "$WORK/cn-last-applied")
    printf 'route add malformed via GATEWAY table cn\n' > "$WORK/routefile"
    run_status 0 >/dev/null
    [ "$(cat "$WORK/cn-last-applied")" = "$cn_state_before" ] \
        && ok "failed CN rebuild preserves its state" \
        || bad "failed CN rebuild advanced its state"
    printf '%s\n' \
        'route add 1.0.1.0/24 via GATEWAY table cn' \
        'route add 1.0.2.0/23 via GATEWAY table cn' \
        'route add 10.20.0.0/16 via GATEWAY table cn' \
        'route add 100.12.34.0/24 via GATEWAY table cn' > "$WORK/routefile"
    FORCE_CN=1 run_script 0 >/dev/null

    local base500 base1000 base1150 chain duplicate_rc
    base500=$(band 500); base1000=$(band 1000); base1150=$(band 1150)
    [ "$(count 1150)" -gt 0 ] && ok "1150 installed ($(count 1150) rules)" \
                              || bad "1150 empty"
    [ "$(count 1000)" -gt 0 ] && ok "1000 installed ($(count 1000) rules)" \
                              || bad "1000 empty"

    head_ "duplicate installed policy rule"
    if ip rule add fwmark 0x1/0xffffffff lookup ioa pref 1150 2>/dev/null; then
        [ "$(band 1150 | grep -Ec '^from all fwmark 0x1(/0xffffffff)? lookup ioa$')" -eq 2 ] \
            || bad "kernel accepted but did not expose the duplicate owned rule"
        run_script 0 >/dev/null
        duplicate_rc=$?
        if [ "$duplicate_rc" -eq 0 ] &&
           [ "$(band 1150 | grep -Ec '^from all fwmark 0x1(/0xffffffff)? lookup ioa$')" -eq 1 ]; then
            ok "duplicate owned rule triggers non-FORCE reconciliation"
        else
            bad "duplicate owned rule survived non-FORCE run (rc=$duplicate_rc): $(band 1150)"
        fi
    else
        ok "kernel rejects duplicate owned rules"
    fi

    head_ "policy ownership and ordering"
    [ "$(count 500)" -eq 1 ] && ok "Tailscale mark has one early escape rule" \
                               || bad "Tailscale mark escape is missing or duplicated"
    [ "$(count 1500)" -eq 1 ] && ok "routefile has one direct lookup rule" \
                                || bad "routefile direct rule missing"
    [ "$(band 1501 | grep -Ec '^from all fwmark 0x2(/0xffffffff)? prohibit$')" -eq 1 ] \
        && ok "CN direct lookup fails closed before Tailscale" \
        || bad "CN direct traffic can fall through to Tailscale: $(band 1501)"
    ip route del default
    if ! ip route get 203.0.113.1 mark 0x2 >/dev/null 2>&1; then
        ok "CN traffic is prohibited while main has no route"
    else
        bad "CN traffic fell through while main had no route"
    fi
    ip route add default via 10.36.48.1 dev wlan0
    [ -z "$(band 1400)" ] \
        && ok "retired early IOA mark band is empty" \
        || bad "early IOA mark band still overrides routefile: $(band 1400)"
    [ "$(band 1150 | grep -Ec '^from all fwmark 0x1(/0xffffffff)? lookup ioa$')" -eq 1 ] \
        && ok "IOA has exactly one post-routefile full-width mark rule" \
        || bad "IOA post-routefile exact mark rule is missing or duplicated"
    [ "$(band 1150 | grep -Fxc 'from all to 10.0.0.0/8 fwmark 0 lookup ioa')" -eq 1 ] \
        && ok "IOA has exactly one unmarked 10/8 fallback rule" \
        || bad "IOA 10/8 rule is missing or duplicated"
    ! band 1150 | grep -q '100.12.0.0/16' \
        && ok "100.12/16 is classified dynamically, not statically" \
        || bad "static 100.12/16 IOA rule still exists"
    ! band 1150 | grep -q '9.0.0.0/8' && ok "9/8 is not statically routed to IOA" \
                                           || bad "9/8 still has a static IOA rule"
    ! ip -4 rule show | grep -qE 'to (192\.168\.0\.0/16|172\.16\.0\.0/12|169\.254\.0\.0/16)' \
        && ok "no broad private-network bypass remains" \
        || bad "broad private-network bypass remains"
    [ "$(snapshot_owner_state)" = "$owner_before" ] \
        && ok "tunnel-owned tables and rules are untouched" \
        || bad "tunnel-owned tables or rules changed"

    head_ "SmartDNS fragments ignore tun0 and track DHCP content"
    rm -f "$WORK/ioa-dns.conf"
    : > "$WORK/systemctl-calls"
    ip addr flush dev tun0
    run_script 1 >/dev/null
    if [ ! -e "$WORK/ioa-dns.conf" ] &&
       ! grep -Fq 'restart smartdns' "$WORK/systemctl-calls"; then
        ok "tun0 down writes no IOA fragment and does not restart SmartDNS"
    else
        bad "tun0 down changed IOA DNS state or restarted SmartDNS"
    fi
    : > "$WORK/systemctl-calls"
    ip addr add 192.168.255.10/24 dev tun0
    run_script 1 >/dev/null
    ip addr replace 192.168.255.77/24 dev tun0
    ip route replace default dev tun0 table 52
    run_script 1 >/dev/null
    if [ ! -e "$WORK/ioa-dns.conf" ] &&
       ! grep -Fq 'restart smartdns' "$WORK/systemctl-calls"; then
        ok "tun0 up and address changes write no IOA fragment and do not restart SmartDNS"
    else
        bad "tun0 up or address change changed IOA DNS state or restarted SmartDNS"
    fi
    : > "$WORK/systemctl-calls"
    printf '   6 domain name server 203.0.113.53\n 114 captive portal     https://login.hotel.test/api\n' > "$WORK/lease"
    run_script 1 >/dev/null
    if grep -Fqx '# Captive portal DNS from wlan0' "$WORK/dhcp-dns.conf" &&
       grep -Fqx 'server 203.0.113.53 -group captive -exclude-default-group' \
           "$WORK/dhcp-dns.conf" &&
       grep -Fqx 'nameserver /login.hotel.test/captive' "$WORK/dhcp-dns.conf" &&
       grep -Fq 'to 203.0.113.53 lookup main' <<<"$(band 1000)" &&
       [ "$(grep -Fxc 'restart smartdns' "$WORK/systemctl-calls")" -eq 1 ]; then
        ok "changed captive DNS is portal-scoped and physically reachable"
    else
        bad "changed captive DNS escaped its portal-only group"
    fi
    : > "$WORK/systemctl-calls"
    run_script 1 >/dev/null
    if ! grep -Fq 'restart smartdns' "$WORK/systemctl-calls"; then
        ok "unchanged DHCP DNS does not restart SmartDNS"
    else
        bad "unchanged captive DNS restarted SmartDNS"
    fi
    printf '   6 domain name server 203.0.113.53\n' > "$WORK/lease"
    run_script 1 >/dev/null
    if grep -Fqx '# No captive portal DNS' "$WORK/dhcp-dns.conf" &&
       ! grep -Fq 'server ' "$WORK/dhcp-dns.conf" &&
       ! grep -Fq 'to 203.0.113.53 lookup main' <<<"$(band 1000)"; then
        ok "ordinary DHCP DNS has neither a SmartDNS nor a physical-route exception"
    else
        bad "ordinary DHCP DNS leaked into SmartDNS: $(cat "$WORK/dhcp-dns.conf")"
    fi
    printf '   6 domain name server 202.152.254.230\n                        202.152.254.65\n 114 captive portal     https://login.hotel.test/api\n' \
        > "$WORK/lease"
    run_script 1 >/dev/null

    head_ "direct routes override IOA business policy"
    if cn_contains 10.20.1.1 && [ "$(route_table 10.20.1.1 'mark 0x2')" = main ]; then
        ok "routefile classification overrides static 10/8 IOA through current main"
    else
        bad "routefile classification lost to static 10/8"
    fi
    if cn_contains 100.12.34.5 &&
       [ "$(route_table 100.12.34.5 'mark 0x2')" = main ]; then
        ok "unclassified 100.12 routefile destination uses current main"
    else
        bad "unclassified routefile destination did not use current main"
    fi
    [ "$(route_table 10.36.48.1)" = main ] && ok "connected 10/8 LAN overrides IOA" \
                                            || bad "connected LAN routed into IOA"

    head_ "NETMODE_IOA only classifies unmarked SmartDNS business packets"
    ipset add ioa 21.34.11.74 -exist
    ipset add ioa 10.20.1.1 -exist
    chain=$(iptables -t mangle -S NETMODE_IOA)
    grep -Eq -- '! --mark 0x0(/0xffffffff)? -j RETURN' <<<"$chain" \
        && ok "all non-zero marks are preserved" || bad "non-zero mark guard missing"
    grep -q -- '--set-xmark 0x1/0xffffffff' <<<"$chain" \
        && ok "IOA business mark is written exactly" || bad "IOA mark write is not exact"
    ! grep -q -- '--match-set ioa dst -m set' <<<"$chain" \
        && ok "IOA classification has no static prefix intersection" \
        || bad "IOA classification still depends on a static prefix set"
    full_width_fwmark_rule 1150 0x1 ioa \
        && ok "IOA rule matches the exact full-width mark before CN" \
        || bad "IOA rule is not exact 0x1/0xffffffff before CN"
    # SmartDNS's business classification is the curated one, and it must win over the
    # routefile's thousands of APNIC prefixes. Work mail is the case that proves it:
    # exmail's public addresses sit inside 163.177.0.0/16 and 157.255.0.0/16, so a
    # routefile that ran first would mark them 0x2 and send them out the physical
    # interface instead of the tunnel.
    cn_line=$(grep -n -- "--match-set cn_direct dst" <<<"$chain" | cut -d: -f1)
    ioa_line=$(grep -n -- "--match-set ioa dst" <<<"$chain" | cut -d: -f1)
    [ -n "$cn_line" ] && [ -n "$ioa_line" ] && [ "$ioa_line" -lt "$cn_line" ] \
        && ok "SmartDNS business classification precedes routefile acceleration" \
        || bad "routefile acceleration precedes SmartDNS business classification"
    [ "$(band 500 | grep -Fxc 'from all fwmark 0x80000/0xff0000 lookup main')" -eq 1 ] \
        && ok "pref 500 contains the complete Tailscale mark rule exactly once" \
        || bad "pref 500 lacks the complete Tailscale mark rule: $(band 500)"
    while ip rule del pref 5210 2>/dev/null; do :; done
    [ "$(route_table 203.0.113.1 'mark 0x80000')" = main ] \
        && ok "pref 500 routes the Tailscale mark to main" \
        || bad "Tailscale mark fell through pref 500"
    ip rule add fwmark 0x80000/0xff0000 lookup main pref 5210
    [ "$(route_table 10.30.1.1 'mark 0xa38')" = 20 ] \
        && ok "SmartGateAgent mark remains owner-routed" \
        || bad "SmartGateAgent mark was captured"
    [ "$(route_table 10.30.1.1 'mark 0x80000')" = main ] \
        && ok "Tailscale mark remains owner-routed" \
        || bad "Tailscale mark was captured"
    [ "$(route_table 203.0.113.1 'mark 0xa39')" != ioa ] \
        && ok "low-bit collision does not match IOA" \
        || bad "0xa39 incorrectly matched IOA business mark"
    iptables -t mangle -Z NETMODE_IOA
    ping -q -c 1 -W 1 21.34.11.74 >/dev/null 2>&1 || true
    [ "$(iptables -t mangle -L NETMODE_IOA -nvx |
        awk '$3 == "MARK" && /match-set ioa dst/ {print $1}')" -eq 1 ] \
        && ok "real unmarked SmartDNS-classified 21.x packet reaches the mark rule" \
        || bad "SmartDNS-classified 21.x packet did not reach the mark rule"
    ipset del ioa 21.34.11.74
    iptables -t mangle -Z NETMODE_IOA
    ping -q -c 1 -W 1 21.34.11.74 >/dev/null 2>&1 || true
    [ "$(iptables -t mangle -L NETMODE_IOA -nvx |
        awk '$3 == "MARK" && /match-set ioa dst/ {print $1}')" -eq 0 ] \
        && ok "unclassified 21.x packet remains unmarked" \
        || bad "unclassified 21.x packet reached the mark rule"
    ipset add ioa 21.34.11.74 -exist
    iptables -t mangle -Z NETMODE_IOA
    for mark in 2616 524288 2; do
        ping -q -c 1 -W 1 -m "$mark" 21.34.11.74 >/dev/null 2>&1 || true
    done
    nonzero_return=$(iptables -t mangle -L NETMODE_IOA -nvx |
        awk '$3 == "RETURN" {sum += $1} END {print sum + 0}')
    nonzero_mark=$(iptables -t mangle -L NETMODE_IOA -nvx |
        awk '$3 == "MARK" && /match-set ioa dst/ {print $1}')
    if [ "$nonzero_return" -eq 3 ] && [ "$nonzero_mark" -eq 0 ]; then
        ok "real packets with owner and arbitrary non-zero marks remain unchanged"
    else
        bad "non-zero packet counters: RETURN=$nonzero_return MARK=$nonzero_mark"
    fi

    for private in 192.168.200.1 172.31.200.1 169.254.200.1; do
        [ "$(route_table "$private")" = 52 ] \
            && ok "$private follows the Tailscale table" \
            || bad "$private bypassed the Tailscale table"
    done

    head_ "repository ownership boundaries"
    ip route add blackhole 192.0.2.0/24 table 500
    ip route add blackhole 198.51.100.0/24 table 501
    iptables -t nat -A POSTROUTING -m mark --mark 0x1 -o owner0 -j MASQUERADE
    iptables -t nat -A POSTROUTING -m mark --mark 0x1 -o tun0 -j SNAT --to-source 192.0.2.10
    iptables -t nat -A POSTROUTING -m mark --mark 0x1000000 -o owner0 -j MASQUERADE
    run_script 1 >/dev/null
    [ "$(routes 500)" -eq 1 ] && [ "$(routes 501)" -eq 1 ] \
        && ok "foreign tables 500/501 are untouched" || bad "foreign tables 500/501 were flushed"
    local nat_rules
    nat_rules=$(iptables -t nat -S POSTROUTING)
    grep -Fq -- '-o owner0 -m mark --mark 0x1 -j MASQUERADE' <<<"$nat_rules" &&
        grep -Fq -- '-o tun0 -m mark --mark 0x1 -j SNAT' <<<"$nat_rules" \
        && ok "similar foreign NAT rules are untouched" || bad "similar foreign NAT rules were removed"
    [ "$(grep -Fc -- '-o tun0 -m mark --mark 0x1 -j MASQUERADE' <<<"$nat_rules")" -eq 1 ] \
        && ok "owned IOA payload NAT rule exists exactly once" \
        || bad "owned IOA payload NAT rule is missing or duplicated"
    [ "$(grep -Fc -- '-o enp1s0 -m mark --mark 0x1000000 -j MASQUERADE' <<<"$nat_rules")" -eq 1 ] &&
        ! grep -Fq -- '-o owner0 -m mark --mark 0x1000000 -j MASQUERADE' <<<"$nat_rules" \
        && ok "IOA owner NAT follows the authorized office device exactly once" \
        || bad "IOA owner NAT is stale, missing, or duplicated"
    [ "$(grep -Fc -- '-o wlan0 -m mark --mark 0x2 -j MASQUERADE' <<<"$nat_rules")" -eq 1 ] \
        && ok "CN reroute source NAT follows the physical device exactly once" \
        || bad "CN reroute source NAT is stale, missing, or duplicated"
    local source_guard
    source_guard=$(nft -s list table ip serverconfig_source_guard 2>/dev/null)
    if grep -Fq 'priority srcnat + 10' <<<"$source_guard" &&
       grep -Fq 'elements = { 100.64.0.0/10, 192.168.255.0/24 }' <<<"$source_guard" &&
       [ "$(grep -c 'ip saddr @foreign_sources counter drop' <<<"$source_guard")" -eq 6 ]; then
        ok "physical source validity is enforced after source NAT"
    else
        bad "post-srcnat physical source guard is incomplete"
    fi
    iptables -t nat -A POSTROUTING -o owner0 -m mark --mark 0x2 -j MASQUERADE
    rm -f "$WORK/nat-gap"
    AUDIT_CN_NAT_GAP=1 NAT_GAP_FILE="$WORK/nat-gap" run_script 1 >/dev/null
    if [ ! -e "$WORK/nat-gap" ] &&
       [ "$(iptables -t nat -S POSTROUTING |
            grep -Ec -- '--mark 0x2(/0xffffffff)? -j MASQUERADE$')" -eq 1 ]; then
        ok "CN source NAT changes device without a zero-rule window"
    else
        bad "CN source NAT disappeared during reconciliation or kept a stale copy"
    fi

    head_ "early-exit fingerprint repairs owned firewall drift"
    iptables -t mangle -A NETMODE_IOA -j ACCEPT
    run_script 0 >/dev/null
    ! iptables -t mangle -S NETMODE_IOA | grep -q -- '-j ACCEPT' \
        && ok "chain content drift triggers reconciliation" || bad "chain drift survived early exit"
    while iptables -t nat -D POSTROUTING -o tun0 -m mark --mark 0x1 -j MASQUERADE 2>/dev/null; do :; done
    while iptables -t nat -D POSTROUTING -o enp1s0 -m mark --mark 0x1000000 -j MASQUERADE 2>/dev/null; do :; done
    while iptables -t nat -D POSTROUTING -o wlan0 -m mark --mark 0x2 -j MASQUERADE 2>/dev/null; do :; done
    run_script 0 >/dev/null
    nat_rules=$(iptables -t nat -S POSTROUTING)
    [ "$(grep -Fc -- '-o tun0 -m mark --mark 0x1 -j MASQUERADE' <<<"$nat_rules")" -eq 1 ] &&
        [ "$(grep -Fc -- '-o enp1s0 -m mark --mark 0x1000000 -j MASQUERADE' <<<"$nat_rules")" -eq 1 ] &&
        [ "$(grep -Fc -- '-o wlan0 -m mark --mark 0x2 -j MASQUERADE' <<<"$nat_rules")" -eq 1 ] \
        && ok "owned NAT drift triggers reconciliation" \
        || bad "owned NAT drift survived early exit"
    nft delete table ip serverconfig_source_guard
    run_script 0 >/dev/null
    source_guard=$(nft -s list table ip serverconfig_source_guard 2>/dev/null)
    [ "$(grep -c 'ip saddr @foreign_sources counter drop' <<<"$source_guard")" -eq 6 ] \
        && ok "source-guard deletion triggers reconciliation" \
        || bad "source-guard deletion survived early exit"

    head_ "equivalent kernel rule spelling converges exactly"
    ip rule add fwmark 0x1/0xffffffff lookup ioa pref 1150
    ip rule add to 8.8.8.8 lookup main pref 1150
    run_script 0 >/dev/null
    [ "$(band 1150)" = "$base1150" ] \
        && ok "equivalent duplicate and foreign stale rule are removed" \
        || bad "pref 1150 did not converge exactly: $(band 1150)"

    head_ "routefile failures preserve active policy"
    local cn_before marking_before malicious_owner_before table52_before invalid_route
    cn_before=$(cn_entries)
    marking_before=$(snapshot_marking)
    printf 'route add malformed via GATEWAY table cn\n' > "$WORK/routefile"
    [ "$(run_status 1)" -eq 0 ] \
        && ok "malformed optional routefile does not fail base reconciliation" \
        || bad "malformed optional routefile failed base reconciliation"
    [ "$(cn_entries)" = "$cn_before" ] \
        && ok "malformed routefile preserves the last known-good CN set" \
        || bad "malformed routefile changed the active CN set"
    [ "$(snapshot_marking)" = "$marking_before" ] \
        && ok "failed staging preserves the active marking chain and hook byte-for-byte" \
        || bad "failed staging changed the active marking chain or hook"
    printf '%s\n' \
        'route add 1.0.1.0/24 via GATEWAY table cn' \
        'route add 1.0.2.0/23 via GATEWAY table cn' \
        'route add 10.20.0.0/16 via GATEWAY table cn' \
        'route add 100.12.34.0/24 via GATEWAY table cn' > "$WORK/routefile"
    [ "$(FAIL_NEXT_MARK=1 run_status 1)" -ne 0 ] \
        && ok "mark-chain construction failure aborts the run" \
        || bad "mark-chain construction failure was ignored"
    [ "$(snapshot_marking)" = "$marking_before" ] \
        && ok "mark-chain construction failure preserves the active hook" \
        || bad "mark-chain construction failure changed the active hook"

    rm -f "$WORK/failed-restore-was-atomic"
    [ "$(FAIL_MARK_RESTORE=1 run_status 1)" -ne 0 ] \
        && ok "mark-chain switch failure aborts the run" \
        || bad "mark-chain switch failure was ignored"
    [ -e "$WORK/failed-restore-was-atomic" ] &&
        [ "$(snapshot_marking)" = "$marking_before" ] \
        && ok "failed mark-chain transaction preserves the complete pre-switch ruleset" \
        || bad "failed mark-chain transaction partially changed active marking"
    run_script 1 >/dev/null
    if ! iptables -t mangle -S NETMODE_IOA_NEXT >/dev/null 2>&1 &&
       [ "$(iptables -t mangle -S OUTPUT |
            grep -Fxc -- '-A OUTPUT -j NETMODE_IOA')" -eq 1 ]; then
        ok "successful mark-chain transaction leaves one hook and no staging chain"
    else
        bad "successful mark-chain transaction left a duplicate hook or staging chain"
    fi

    malicious_owner_before=$(snapshot_owner_state)
    table52_before=$(ip route show table 52 | sort)
    printf '%s\n' \
        'route add 1.0.3.0/24 via GATEWAY table cn' \
        'route flush table 52' > "$WORK/routefile"
    [ "$(run_status 1)" -eq 0 ] \
        && ok "out-of-grammar optional routefile does not fail base reconciliation" \
        || bad "out-of-grammar optional routefile failed base reconciliation"
    [ "$(snapshot_owner_state)" = "$malicious_owner_before" ] &&
        [ "$(ip route show table 52 | sort)" = "$table52_before" ] \
        && ok "rejected routefile preserves owner state and table 52" \
        || bad "rejected routefile changed owner state or table 52"
    [ "$(cn_entries)" = "$cn_before" ] \
        && ok "rejected routefile preserves old CN set" || bad "rejected routefile changed CN set"

    for invalid_route in \
        'route add 256.0.0.1/24 via GATEWAY table cn' \
        'route add 1.0.1.0/33 via GATEWAY table cn' \
        'route add 1.0.1.0/24 via 10.36.48.1 table cn' \
        'route add 1.0.1.0/24 via GATEWAY table main' \
        'route add 1.0.1.0/24 via GATEWAY table cn metric 1'; do
        printf '%s\n' "$invalid_route" > "$WORK/routefile"
        if [ "$(run_status 1)" -eq 0 ] && [ "$(cn_entries)" = "$cn_before" ]; then
            ok "invalid optional routefile is isolated: $invalid_route"
        else
            bad "invalid optional routefile changed base policy: $invalid_route"
        fi
    done

    printf '%s\n' \
        'route add 1.0.1.0/24 via GATEWAY table cn' \
        'route replace 1.0.1.0/24 via GATEWAY table cn' > "$WORK/routefile"
    if [ "$(run_status 1)" -eq 0 ] && [ "$(cn_entries)" = "$cn_before" ]; then
        ok "duplicate routefile prefixes preserve last known-good acceleration"
    else
        bad "duplicate routefile prefixes changed base policy"
    fi

    printf 'route add 1.0.1.0/24 via GATEWAY table cn\nroute add 1.0.2.0/23 via GATEWAY table cn\n' > "$WORK/routefile"
    run_script 1 >/dev/null

    head_ "missing and empty routefiles authoritatively disable cn"
    rm -f "$WORK/routefile"
    run_script 1 >/dev/null
    [ -z "$(cn_entries)" ] && [ "$(routes cn)" -eq 0 ] &&
        [ "$(count 1500)" -eq 0 ] && [ "$(count 1501)" -eq 0 ] \
        && ok "missing routefile disables optional CN acceleration" \
        || bad "missing routefile left CN acceleration active"
    : > "$WORK/routefile"
    ip route add blackhole 203.0.113.0/24 table 101 2>/dev/null || true
    ip rule add lookup cn pref 1500 2>/dev/null || true
    run_script 1 >/dev/null
    [ -z "$(cn_entries)" ] && [ "$(routes cn)" -eq 0 ] &&
        [ "$(count 1500)" -eq 0 ] && [ "$(count 1501)" -eq 0 ] \
        && ok "empty routefile disables optional CN acceleration and legacy policy" \
        || bad "empty routefile left CN acceleration active"
    printf '# only a comment\n\n' > "$WORK/routefile"
    run_script 1 >/dev/null
    [ -z "$(cn_entries)" ] && [ "$(count 1500)" -eq 0 ] && [ "$(count 1501)" -eq 0 ] \
        && ok "comment-only routefile disables optional CN acceleration" \
        || bad "comment-only routefile left CN acceleration active"
    printf 'route add 1.0.1.0/24 via GATEWAY table cn\nroute add 1.0.2.0/23 via GATEWAY table cn\n' > "$WORK/routefile"
    run_script 1 >/dev/null

    head_ "empty IOA table falls through to table 52"
    ip route flush table 400
    ip addr flush dev tun0
    ip route replace default dev owner0 table 52
    [ "$(route_table 203.0.113.1 'mark 0x1')" = 52 ] \
        && ok "marked IOA lookup follows the existing fallback when table ioa is absent" \
        || bad "empty IOA lookup did not fall through to table 52"
    ip route replace default dev tun0 table 52
    ip addr add 192.168.255.77/24 dev tun0
    ip route add default via 192.168.255.1 dev tun0 table 400
    roam_to 10.36.48.162/20 10.36.48.1 202.152.254.230 202.152.254.65
    run_script 1 >/dev/null
    base1000=$(band 1000)

    head_ "idempotence"
    run_script 1 >/dev/null
    [ "$(band 1150)" = "$base1150" ] && ok "1150 unchanged" || bad "1150 drifted"
    [ "$(band 1000)" = "$base1000" ] && ok "1000 unchanged" || bad "1000 drifted"

    head_ "foreign rule injected into a band we own"
    ip rule add to 8.8.8.8 lookup main pref 1150
    [ "$(count 1150)" -gt "$(wc -l <<<"$base1150")" ] || bad "injection did not take"
    run_script 0 >/dev/null
    if [ "$(band 1150)" = "$base1150" ]; then
        ok "foreign rule removed without FORCE"
    else
        bad "foreign rule survived: $(band 1150 | grep 8.8.8.8)"
    fi

    head_ "our rule deleted from a band we own"
    ip rule del to 10.0.0.0/8 fwmark 0x0/0xffffffff lookup ioa pref 1150 2>/dev/null
    run_script 0 >/dev/null
    [ "$(band 1150)" = "$base1150" ] && ok "missing rule restored without FORCE" \
                                     || bad "not restored"

    head_ "whole band deleted"
    while ip rule del pref 1000 2>/dev/null; do :; done
    run_script 0 >/dev/null
    [ "$(band 1000)" = "$base1000" ] && ok "band rebuilt without FORCE" \
                                     || bad "band not rebuilt"

    head_ "band is never empty during a rebuild"
    local zero=0 i
    ( for i in $(seq 1 400); do count 1150; sleep 0.01; done > "$WORK/samples" ) &
    local sampler=$!
    sleep 0.3
    run_script 1 >/dev/null
    wait $sampler
    zero=$(grep -cx 0 "$WORK/samples")
    [ "$zero" -eq 0 ] && ok "1150 never empty ($(wc -l < "$WORK/samples") samples)" \
                      || bad "1150 was empty $zero times"

    head_ "MASQUERADE survives a tun0 address change"
    ip addr flush dev tun0
    ip addr add 192.168.255.77/24 dev tun0
    local nat
    nat=$(iptables -t nat -S POSTROUTING 2>/dev/null | grep -c 'tun0.*MASQUERADE')
    [ "$nat" -eq 1 ] && ok "NAT rule needs no refresh (address-independent)" \
                     || bad "NAT rule count is $nat"

    # From here the AP changes, so nothing below may compare against the
    # baseline bands captured above.
    head_ "roam onto an AP with a different subnet, gateway and resolvers"
    owner_before=$(snapshot_owner_state)
    roam_to 10.36.43.250/21 10.36.40.1 202.152.254.230 202.152.254.65
    local rc
    rc=$(run_status 0)
    [ "$rc" -eq 0 ] && ok "reconfigure exits clean after a roam" \
                    || bad "reconfigure exited $rc after a roam"
    [ "$(snapshot_owner_state)" = "$owner_before" ] \
        && ok "roam preserves tunnel-owned tables and rules" \
        || bad "roam modified tunnel-owned tables or rules"

    local b1000
    b1000=$(band 1000)
    grep -q 'to 10.36.40.1 lookup main'      <<<"$b1000" \
        && ok "new gateway pinned at 1000" || bad "new gateway missing from 1000"
    grep -q 'to 10.36.40.0/21 lookup main'   <<<"$b1000" \
        && ok "new subnet pinned at 1000" || bad "new subnet missing from 1000"
    if ! grep -q 'to 202.152.254.230 lookup main' <<<"$b1000" &&
       ! grep -q 'to 202.152.254.65 lookup main'  <<<"$b1000"; then
        ok "ordinary public DHCP resolvers do not bypass the exit node"
    else
        bad "ordinary public DHCP resolver received a physical bypass: $b1000"
    fi
    # A rule that outlives the AP it was built for is a black hole, not a leftover.
    if grep -qE 'to (10\.36\.48\.|10\.36\.32\.0/20)' <<<"$b1000"; then
        bad "stale rules from the previous AP survived: $b1000"
    else
        ok "no rule from the previous AP survived"
    fi

    # CN classification contains no gateway. A roam must leave the set byte-identical while
    # retiring any injected legacy route-table state.
    cn_before=$(cn_entries)
    ip route replace blackhole 198.51.100.0/24 table 101 2>/dev/null || true
    owner_before=$(snapshot_owner_state)
    run_script 0 >/dev/null
    rc=$?
    [ "$rc" -eq 0 ] && ok "CN acceleration survives a gateway change" \
                    || bad "CN acceleration failed after gateway change (rc=$rc)"
    if [ "$(cn_entries)" = "$cn_before" ]; then
        ok "gateway change does not rebuild the gateway-independent CN set"
    else
        bad "gateway change mutated the gateway-independent CN set"
    fi
    if [ -n "$(ip route show table "$CN_TABLE" 2>/dev/null)" ] ||
       [ -n "$(ip route show table "$CN_STAGE_TABLE" 2>/dev/null)" ] ||
       [ -n "$(ip route show table 101 2>/dev/null)" ]; then
        bad "gateway change retained legacy CN routing-table state"
    else
        ok "gateway change leaves no stale CN gateway routes"
    fi
    [ "$(snapshot_owner_state)" = "$owner_before" ] \
        && ok "CN gateway change preserves tunnel-owned state" \
        || bad "CN gateway change modified tunnel-owned state"

    head_ "SmartGate underlay audit (iOA pref 1100/1200, tables 20/230)"

    # The healthy table 230 default matches the shim's choice: the wired
    # underlay advertisement (table 19) when present, else the main default.
    expected_default=$(ip -4 route show table 19 default 2>/dev/null)
    [ -n "$expected_default" ] ||
        expected_default=$(ip -4 route show table main default | awk '$5 ~ /^wl/')
    exp_gw=$(awk '{ for (i = 1; i < NF; i++) if ($i == "via") { print $(i + 1); exit } }' \
        <<<"$expected_default")
    exp_dev=$(awk '{ for (i = 1; i < NF; i++) if ($i == "dev") { print $(i + 1); exit } }' \
        <<<"$expected_default")
    ip route replace default via "$exp_gw" dev "$exp_dev" table 230
    ip route replace default via "$exp_gw" dev "$exp_dev" table 20
    ip rule add pref 1100 fwmark 0xa38 lookup 20
    ip rule add pref 1200 from 10.36.43.250 lookup 230
    rc=$(run_status 0)
    [ "$rc" -eq 0 ] && ok "SmartGate audit run exits clean" \
                    || bad "SmartGate audit run exited $rc"
    [ -n "$(ip -4 rule show pref 1200)" ] &&
    [ -n "$(ip -4 rule show pref 1100)" ] &&
    ip route show table 230 | grep -q "via $exp_gw" \
        && ok "healthy SmartGate policy preserved" \
        || bad "healthy SmartGate policy torn down"

    # Stale: table 230 points at the pre-roam gateway. Rule 1200 captures all
    # locally sourced traffic, so this blackholes the machine; the audit must
    # fail open so iOA redeploys against the settled main table.
    ip route replace default via 10.36.48.1 dev wlan0 onlink table 230
    rc=$(run_status 0)
    [ "$rc" -eq 0 ] && ok "stale SmartGate audit run exits clean" \
                    || bad "stale SmartGate audit run exited $rc"
    [ -z "$(ip -4 rule show pref 1200)" ] &&
    [ -z "$(ip -4 rule show pref 1100)" ] &&
    [ -z "$(ip route show table 230)" ] &&
    [ -z "$(ip route show table 20)" ] \
        && ok "stale SmartGate underlay torn down (fail open)" \
        || bad "stale SmartGate underlay left in place"

    # Service stopped: iOA's own teardown can be killed mid-cleanup; leftover
    # SmartGate policy is residue and must go.
    ip route replace default via "$exp_gw" dev "$exp_dev" table 230
    ip route replace default via "$exp_gw" dev "$exp_dev" table 20
    ip rule add pref 1100 fwmark 0xa38 lookup 20
    ip rule add pref 1200 from 10.36.43.250 lookup 230
    NGNCLIENT_ACTIVE_OVERRIDE=0 FORCE=0 NSTEST=1 NETWORK_RECONFIGURE_LOCKED=1 \
        IOA_CGROUP_PATHS_OVERRIDE= bash "$SCRIPT" wlan0 >/dev/null 2>&1
    rc=$?
    [ "$rc" -eq 0 ] && ok "inactive SmartGate cleanup exits clean" \
                    || bad "inactive SmartGate cleanup exited $rc"
    [ -z "$(ip -4 rule show pref 1200)" ] &&
    [ -z "$(ip -4 rule show pref 1100)" ] &&
    [ -z "$(ip route show table 230)" ] &&
    [ -z "$(ip route show table 20)" ] \
        && ok "inactive SmartGate residue cleaned" \
        || bad "inactive SmartGate residue left behind"

    # Hard isolation: owner-marked (iOA) packets must never leave via
    # tailscale0. The rule lives in mangle POSTROUTING (after the reroute
    # triggered by the NETMODE_IOA marking) and keys off the owner mark, not
    # the cgroup match, so it installs fine inside the namespace.
    FORCE=1 NSTEST=1 NETWORK_RECONFIGURE_LOCKED=1 \
        IOA_CGROUP_PATHS_OVERRIDE= bash "$SCRIPT" wlan0 >/dev/null 2>&1
    rc=$?
    [ "$rc" -eq 0 ] && ok "isolation run exits clean" \
                    || bad "isolation run exited $rc"
    iptables -t mangle -C POSTROUTING -m mark --mark 0x1000000/0xffffffff \
        -o tailscale0 -j DROP 2>/dev/null \
        && ok "owner-mark DROP on tailscale0 installed in mangle POSTROUTING" \
        || bad "owner-mark DROP on tailscale0 missing from mangle POSTROUTING"
    ! iptables -t filter -C OUTPUT -m mark --mark 0x1000000/0xffffffff \
        -o tailscale0 -j REJECT 2>/dev/null \
        && ok "no pre-reroute filter OUTPUT variant remains" \
        || bad "pre-reroute filter OUTPUT REJECT variant still present"

    # Restore the owner sentinels the cold-boot block expects to find.
    ip route replace default dev owner0 table 20
    ip route replace default dev owner0 table 230

    head_ "cold boot: no owned rules, no cn table, no state"
    # Restore owner sentinels independently before the cold-boot run.
    while ip rule del pref 490 2>/dev/null; do :; done
    while ip rule del pref 5210 2>/dev/null; do :; done
    while ip rule del pref 5270 2>/dev/null; do :; done
    ip rule add fwmark 0xa38 lookup 20 pref 490
    ip rule add fwmark 0x80000/0xff0000 lookup main pref 5210
    ip rule add lookup 52 pref 5270
    owner_before=$(snapshot_owner_state)
    while ip rule del pref 500  2>/dev/null; do :; done
    while ip rule del pref 1000 2>/dev/null; do :; done
    while ip rule del pref 1500 2>/dev/null; do :; done
    while ip rule del pref 1150 2>/dev/null; do :; done
    while ip rule del pref 3000 2>/dev/null; do :; done
    ip route flush table "$CN_TABLE" 2>/dev/null || true
    iptables -t mangle -F 2>/dev/null || true
    rm -f "$WORK/last-applied"
    # `ip route show` on an empty table exits 2, and under `pipefail` that used
    # to kill the run at the first count — after the reset, before a single rule
    # had gone back. A cold boot is exactly that state.
    rc=$(run_status 0)
    [ "$rc" -eq 0 ] && ok "reconfigure exits clean from a cold start" \
                    || bad "reconfigure exited $rc from a cold start"
    [ "$(count 1000)" -gt 0 ] && ok "1000 rebuilt from nothing ($(count 1000) rules)" \
                              || bad "1000 still empty after a cold start"
    [ "$(count 1150)" -gt 0 ] && ok "1150 rebuilt from nothing ($(count 1150) rules)" \
                              || bad "1150 still empty after a cold start"
    [ "$(snapshot_owner_state)" = "$owner_before" ] \
        && ok "cold boot preserves tunnel-owned tables and rules" \
        || bad "cold boot modified tunnel-owned tables or rules"
    [ -s "$WORK/last-applied" ] && ok "state written only after a clean run" \
                                || bad "no state file after a clean run"

    printf '\n\033[1m%s passed, %s failed\033[0m\n' "$pass" "$fail"
    [ "$fail" -eq 0 ]
}

# Never trust IN_NETNS by itself: a caller can forge it and otherwise run setup
# against the host. The outer process records both namespace identity forms and
# the inner process must differ in both before the first `ip` mutation.
current_netns_link=$(readlink /proc/self/ns/net) || exit 1
current_netns_inode=$(stat -Lc %i /proc/self/ns/net) || exit 1
if [ -n "${IN_NETNS:-}" ]; then
    host_netns_fd_link=$(readlink "/proc/self/fd/${HOST_NETNS_FD:-missing}" 2>/dev/null || true)
    host_netns_fd_inode=$(stat -Lc %i "/proc/self/fd/${HOST_NETNS_FD:-missing}" 2>/dev/null || true)
    if [ -z "${HOST_NETNS_LINK:-}" ] || [ -z "${HOST_NETNS_INODE:-}" ] ||
       [ "$host_netns_fd_link" != "$HOST_NETNS_LINK" ] ||
       [ "$host_netns_fd_inode" != "$HOST_NETNS_INODE" ] ||
       [ "$current_netns_link" = "$HOST_NETNS_LINK" ] ||
       [ "$current_netns_inode" = "$HOST_NETNS_INODE" ]; then
        echo "refusing to run without a verified network namespace" >&2
        exit 1
    fi
fi

host_test_state() {
    local table prefix
    for table in main cn cn_stage ioa 19 20 52 230 400; do
        for prefix in \
            10.20.0.0/16 100.12.34.0/24 192.0.2.0/24 \
            198.51.100.0/24 203.0.113.0/24
        do
            ip -4 route show table "$table" "$prefix" 2>/dev/null |
                sed "s|^|route $table |"
        done
    done
    ip -4 rule show 2>/dev/null |
        grep -E '10\.20|100\.12\.34|192\.0\.2|198\.51\.100|203\.0\.113' || true
    ip -br link show 2>/dev/null |
        grep -E '^(owner0|wired-peer|enp1s0|wlan0)[[:space:]]' || true
    ipset list ioa_intranet 2>/dev/null | sed 's/^/ipset /' || true
    iptables -t mangle -S NETMODE_IOA 2>/dev/null | sed 's/^/iptables /' || true
    iptables -t mangle -S NETMODE_IOA_NEXT 2>/dev/null | sed 's/^/iptables /' || true
}

# A fresh directory per run, owned by the invoking user and readable inside the
# namespace. Reusing a fixed path meant root-owned leftovers from an earlier run
# made the rebuild fail — and it failed *silently*, so the next run tested a
# stale copy and reported a pass for code that was never executed. A mutation
# test caught it: the delete path was disabled and all 12 checks still passed.
if [ -z "${IN_NETNS:-}" ]; then
    WORK=$(mktemp -d /tmp/nstest.XXXXXX) || exit 1
    export WORK
    if [ "${KEEP_WORK:-0}" = 1 ]; then
        trap 'printf "test artifacts: %s\n" "$WORK"' EXIT
    else
        trap 'rm -rf "$WORK"' EXIT
    fi
    # `unshare -r` maps the caller to nobody inside the namespace, so everything
    # it must read or execute has to be world-accessible from out here.
    chmod 755 "$WORK"
    build_under_test || { echo "could not build the script under test"; exit 1; }
    [ -s "$WORK/nr-under-test" ] || { echo "the built copy is empty"; exit 1; }
    # Copied out here, where the repository is still readable, so the namespace exercises the
    # fragment that actually ships instead of a stub written to match the assertions.
    cp /home/amos/git/serverconfig/network/smartdns/office.conf "$WORK/office.conf" || exit 1
    chmod 644 "$WORK/office.conf"
    # The base config for the same reason: the office overrides are derived from its tunnel-resolver
    # domains, so a stub would only prove the derivation works on whatever the stub happens to list.
    cp /home/amos/git/serverconfig/network/smartdns/smartdns.conf "$WORK/smartdns-base.conf" || exit 1
    chmod 644 "$WORK/smartdns-base.conf"
    exec 9</proc/self/ns/net
    export HOST_NETNS_FD=9
    export HOST_NETNS_LINK=$current_netns_link
    export HOST_NETNS_INODE=$current_netns_inode
    host_before=$(host_test_state)
    set +e
    unshare -rn --mount bash -c \
        "mount -t tmpfs none /run 2>/dev/null || true; IN_NETNS=1 WORK='$WORK' bash '$0'"
    child_rc=$?
    set -e
    host_after=$(host_test_state)
    if [ "$host_after" != "$host_before" ]; then
        echo 'network namespace test changed host networking state' >&2
        diff -u <(printf '%s\n' "$host_before") <(printf '%s\n' "$host_after") >&2 || true
        exit 2
    fi
    exit "$child_rc"
fi

SCRIPT="$WORK/nr-under-test"
mount --bind "$WORK" /etc/iproute2 || exit 1
main
