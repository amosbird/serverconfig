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
pass=0; fail=0

ok()   { printf '  \033[32mOK\033[0m   %s\n' "$*"; pass=$((pass+1)); }
bad()  { printf '  \033[31mFAIL\033[0m %s\n' "$*"; fail=$((fail+1)); }
head_() { printf '\n\033[1m%s\033[0m\n' "$*"; }

# A namespace that looks enough like the real machine for the script to run:
# a "physical" link with a gateway, a DHCP-style resolver, and tunnel-owner sentinels.
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
        bash "$SCRIPT" wlan0 >/dev/null 2>&1
    echo $?
}

# Move the namespace onto a different AP: new subnet, new gateway, new
# resolvers. This is the event that broke the live machine — the old AP's
# resolver was its own gateway and so was covered by the subnet rule by
# accident, and the new one hands out public addresses that are covered by
# nothing unless they are named.
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
systemctl()  { printf '%s\n' "$*" >> WORKDIR/systemctl-calls; return 0; }
tailscale()  { return 1; }
logger()     { return 0; }
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
    command iptables "$@"
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
    ('OFFICE_SRC="/home/amos/git/serverconfig/network/smartdns/office.conf"',
     'OFFICE_SRC="%s/office.conf"' % work),
    ('OFFICE_DST="/etc/smartdns/office.conf"', 'OFFICE_DST="%s/office-dst.conf"' % work),
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
    echo '# office' > "$WORK/office.conf"
    # The lease is a file so a test can hand out a different resolver, which is
    # what a roam onto another AP actually does. Two servers on a continuation
    # line, because that wrapping is what the awk state machine exists for.
    printf '   6 domain name server 202.152.254.230\n                        202.152.254.65\n 114 captive portal     https://login.hotel.test/api\n' \
        > "$WORK/lease"
    printf '   3 router 10.76.76.193\n' > "$WORK/wired-lease"
    chmod 644 "$WORK"/*
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
    check_fixed_table_conflict 'table 101 bound to foreign name' \
        '101 foreign' '400 ioa' '102 kwai'
    check_fixed_table_conflict 'cn bound to another ID' \
        '201 cn' '400 ioa' '102 kwai'
    check_fixed_table_conflict 'table 400 bound to foreign name' \
        '101 cn' '400 foreign' '102 kwai'
    local duplicate_owner_before duplicate_rc
    write_rt_tables '101 cn' '400 ioa' '400 ioa' '102 kwai'
    duplicate_owner_before=$(snapshot_owner_state)
    duplicate_rc=$(run_status 1)
    if [ "$duplicate_rc" -eq 0 ] &&
       [ "$(awk '$1 == 400 && $2 == "ioa" {count++} END {print count + 0}' \
           "$WORK/rt_tables")" -eq 1 ]; then
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
       [ "$(awk '$1 == 101 && $2 == "cn" {count++} END {print count + 0}' "$WORK/rt_tables")" -eq 1 ] &&
       [ "$(awk '$1 == 400 && $2 == "ioa" {count++} END {print count + 0}' "$WORK/rt_tables")" -eq 1 ]; then
        ok "unused fixed table IDs register once"
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
    if ip -4 rule show | grep -Eq 'lookup (19|wired_underlay)'; then
        bad "a policy rule incorrectly references table 19"
    else
        ok "no policy rule references table 19"
    fi
    for mapping in \
        'nameserver /smartgate.oa.tencent.com/ioa' \
        'nameserver /sgw.woa.com/ioa' \
        'nameserver /ioa.tencent.com/ioa'
    do
        if grep -Fqx "$mapping" "$WORK/office-dst.conf"; then
            bad "office DNS overrides bootstrap with $mapping"
        else
            ok "office DNS leaves bootstrap public: $mapping"
        fi
    done
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
    if grep -Fq 'nameserver /ioa.tencent.com/ioa' "$WORK/office-dst.conf"; then
        bad "wired loss retained office bootstrap DNS mappings"
    else
        ok "wired loss removes office bootstrap DNS mappings"
    fi
    ip link set enp1s0 up
    printf '   3 router 10.76.76.194\n' > "$WORK/wired-lease"
    run_script 1 >/dev/null
    if ip route show table 19 | sed -E 's/[[:space:]]+$//' | grep -Fqx \
            'default via 10.76.76.194 dev enp1s0 onlink'; then
        ok "wired gateway change replaces table 19 advertisement"
    else
        bad "wired gateway change did not converge: $(ip route show table 19)"
    fi
    printf '   3 router 10.76.76.193\n' > "$WORK/wired-lease"
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
    run_script 1 >/dev/null
    local stage_id
    stage_id=$(awk '$2 == "cn_stage" {print $1}' "$WORK/rt_tables")
    [ "$(awk '$2 == "cn_stage" {count++} END {print count + 0}' "$WORK/rt_tables")" -eq 1 ] &&
        [ "$stage_id" != 102 ] &&
        [ "$(awk -v id="$stage_id" '$1 == id {count++} END {print count + 0}' "$WORK/rt_tables")" -eq 1 ] \
        && ok "cn_stage has one unique non-conflicting table ID ($stage_id)" \
        || bad "cn_stage registration conflicts: $(grep -E '[[:space:]](cn_stage|kwai)$' "$WORK/rt_tables")"
    ip route show table 102 | grep -Fq 'blackhole 203.0.113.0/24' \
        && ok "foreign kwai staging sentinel is untouched" \
        || bad "foreign kwai staging sentinel was changed"
    run_script 1 >/dev/null
    [ "$(awk '$2 == "cn_stage" {print $1}' "$WORK/rt_tables")" = "$stage_id" ] \
        && ok "existing unique cn_stage ID is reused" \
        || bad "cn_stage ID changed after registration"

    head_ "CN rebuild gating"
    ip route replace 1.0.1.0/24 via 192.168.255.1 dev tun0 table cn
    : > "$WORK/ip-calls"
    run_script 0 >/dev/null
    if ip route show table cn | grep -Eq '^1\.0\.1\.0/24 via 10\.36\.48\.1 dev wlan0($| )' &&
       grep -q -- '-batch -' "$WORK/ip-calls"; then
        ok "wrong-device CN route triggers reconciliation"
    else
        bad "wrong-device CN route survived reconciliation: $(ip route show table cn)"
    fi
    : > "$WORK/ip-calls"
    run_script 1 >/dev/null
    if ! grep -Eq '(^| )route (flush|add|replace|del).*table (cn|cn_stage)|-batch -' \
            "$WORK/ip-calls"; then
        ok "ordinary FORCE leaves healthy CN tables untouched"
    else
        bad "ordinary FORCE mutated healthy CN tables"
    fi
    : > "$WORK/ip-calls"
    FORCE_CN=1 run_script 0 >/dev/null
    if grep -Eq '(^| )route (flush|add|replace|del).*table (cn|cn_stage)|-batch -' \
            "$WORK/ip-calls"; then
        ok "FORCE_CN rebuilds CN tables"
    else
        bad "FORCE_CN did not rebuild CN tables"
    fi
    local cn_state_before batch_calls standalone_promotions
    cn_state_before=$(cat "$WORK/cn-last-applied")
    ip route replace blackhole 198.18.0.0/15 table cn
    : > "$WORK/ip-calls"
    FORCE_CN=1 run_script 0 >/dev/null
    batch_calls=$(grep -c '^-batch ' "$WORK/ip-calls")
    standalone_promotions=$(grep -Ec '^route replace .* table cn$' "$WORK/ip-calls" || true)
    [ "$batch_calls" -eq 2 ] \
        && ok "CN rebuild uses one staging and one promotion batch" \
        || bad "CN rebuild used $batch_calls batch calls instead of two"
    [ "$standalone_promotions" -eq 0 ] \
        && ok "CN promotion launches no per-route ip process" \
        || bad "CN promotion launched $standalone_promotions standalone route replacements"
    if ip route show table cn | grep -Fq 'blackhole 198.18.0.0/15'; then
        bad "CN promotion retained a stale route"
    else
        ok "CN promotion removes stale routes"
    fi
    [ "$(cat "$WORK/cn-last-applied")" = "$cn_state_before" ] ||
        bad "forced convergent rebuild changed CN state content"

    if ip route show table cn | sed -E 's/[[:space:]]+$//' |
            grep -Fqx 'throw 193.112.78.32'; then
        ok "CN exclusions install a throw route"
    else
        bad "CN exclusion throw route is missing: $(ip route show table cn)"
    fi
    if [ "$(route_table 193.112.78.32)" = 52 ]; then
        ok "CN exclusion falls through to later policy rules"
    else
        bad "CN exclusion did not fall through: $(ip route get 193.112.78.32)"
    fi
    cn_state_before=$(cat "$WORK/cn-last-applied")
    printf '# Explicit CN bypasses\n193.112.78.33\n' > "$WORK/cn-exclude.conf"
    : > "$WORK/ip-calls"
    run_script 0 >/dev/null
    if grep -q -- '-batch -' "$WORK/ip-calls" &&
       ip route show table cn | sed -E 's/[[:space:]]+$//' |
           grep -Fqx 'throw 193.112.78.33' &&
       ! ip route show table cn | grep -Fq '193.112.78.32'; then
        ok "CN exclusion changes trigger reconciliation"
    else
        bad "CN exclusion change was not reconciled"
    fi
    [ "$(cat "$WORK/cn-last-applied")" != "$cn_state_before" ] \
        && ok "CN exclusion hash is recorded in state" \
        || bad "CN exclusion state did not change"

    cn_state_before=$(cat "$WORK/cn-last-applied")
    printf '193.112.78.33/32\n193.112.78.33/32\n' > "$WORK/cn-exclude.conf"
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

    local base500 base1000 base1400 base2500 chain duplicate_rc
    base500=$(band 500); base1000=$(band 1000); base1400=$(band 1400); base2500=$(band 2500)
    [ "$(count 2500)" -gt 0 ] && ok "2500 installed ($(count 2500) rules)" \
                              || bad "2500 empty"
    [ "$(count 1000)" -gt 0 ] && ok "1000 installed ($(count 1000) rules)" \
                              || bad "1000 empty"

    head_ "duplicate installed policy rule"
    if ip rule add fwmark 0x1/0xffffffff lookup ioa pref 1400 2>/dev/null; then
        [ "$(band 1400 | grep -Ec '^from all fwmark 0x1(/0xffffffff)? lookup ioa$')" -eq 2 ] \
            || bad "kernel accepted but did not expose the duplicate owned rule"
        run_script 0 >/dev/null
        duplicate_rc=$?
        if [ "$duplicate_rc" -eq 0 ] &&
           [ "$(band 1400 | grep -Ec '^from all fwmark 0x1(/0xffffffff)? lookup ioa$')" -eq 1 ]; then
            ok "duplicate owned rule triggers non-FORCE reconciliation"
        else
            bad "duplicate owned rule survived non-FORCE run (rc=$duplicate_rc): $(band 1400)"
        fi
    else
        ok "kernel rejects duplicate owned rules"
    fi

    head_ "policy ownership and ordering"
    [ "$(count 500)" -eq 1 ] && ok "Tailscale mark has one early escape rule" \
                               || bad "Tailscale mark escape is missing or duplicated"
    [ "$(count 1500)" -eq 1 ] && ok "routefile has one direct lookup rule" \
                                || bad "routefile direct rule missing"
    [ "$(band 1400 | grep -Ec '^from all fwmark 0x1(/0xffffffff)? lookup ioa$')" -eq 1 ] \
        && ok "IOA has exactly one early full-width mark rule" \
        || bad "IOA early exact mark rule is missing or duplicated"
    [ "$(band 2500 | grep -Fxc 'from all to 10.0.0.0/8 lookup ioa')" -eq 1 ] \
        && ok "IOA has exactly one 10/8 rule" \
        || bad "IOA 10/8 rule is missing or duplicated"
    [ "$(band 2500 | grep -Fxc 'from all to 100.12.0.0/16 lookup ioa')" -eq 1 ] \
        && ok "IOA has exactly one 100.12/16 rule" \
        || bad "IOA 100.12/16 rule is missing or duplicated"
    ! band 2500 | grep -q '9.0.0.0/8' && ok "9/8 is not statically routed to IOA" \
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
       [ "$(grep -Fxc 'restart smartdns' "$WORK/systemctl-calls")" -eq 1 ]; then
        ok "changed captive DNS is scoped to the portal hostname"
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
       ! grep -Fq 'server ' "$WORK/dhcp-dns.conf"; then
        ok "DHCP DNS without a captive portal is excluded from SmartDNS"
    else
        bad "ordinary DHCP DNS leaked into SmartDNS: $(cat "$WORK/dhcp-dns.conf")"
    fi
    printf '   6 domain name server 202.152.254.230\n                        202.152.254.65\n 114 captive portal     https://login.hotel.test/api\n' \
        > "$WORK/lease"
    run_script 1 >/dev/null

    head_ "direct routes override IOA business policy"
    [ "$(route_table 10.20.1.1)" = cn ] && ok "routefile overrides static 10/8 IOA" \
                                        || bad "routefile lost to static 10/8"
    [ "$(route_table 100.12.34.5)" = cn ] \
        && ok "routefile overrides static 100.12/16 IOA" \
        || bad "routefile lost to static 100.12/16"
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
    full_width_fwmark_rule 1400 0x1 ioa \
        && ok "IOA rule matches the exact full-width mark before CN" \
        || bad "IOA rule is not exact 0x1/0xffffffff before CN"
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
    nonzero_return=$(iptables -t mangle -L NETMODE_IOA -nvx | awk '$3 == "RETURN" {print $1}')
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
    [ "$(grep -Fc -- '-o wlan0 -m mark --mark 0x1000000 -j MASQUERADE' <<<"$nat_rules")" -eq 1 ] &&
        ! grep -Fq -- '-o owner0 -m mark --mark 0x1000000 -j MASQUERADE' <<<"$nat_rules" \
        && ok "IOA owner NAT follows the current physical device exactly once" \
        || bad "IOA owner NAT is stale, missing, or duplicated"

    head_ "early-exit fingerprint repairs owned firewall drift"
    iptables -t mangle -A NETMODE_IOA -j ACCEPT
    run_script 0 >/dev/null
    ! iptables -t mangle -S NETMODE_IOA | grep -q -- '-j ACCEPT' \
        && ok "chain content drift triggers reconciliation" || bad "chain drift survived early exit"
    while iptables -t nat -D POSTROUTING -o tun0 -m mark --mark 0x1 -j MASQUERADE 2>/dev/null; do :; done
    while iptables -t nat -D POSTROUTING -o wlan0 -m mark --mark 0x1000000 -j MASQUERADE 2>/dev/null; do :; done
    run_script 0 >/dev/null
    nat_rules=$(iptables -t nat -S POSTROUTING)
    [ "$(grep -Fc -- '-o tun0 -m mark --mark 0x1 -j MASQUERADE' <<<"$nat_rules")" -eq 1 ] &&
        [ "$(grep -Fc -- '-o wlan0 -m mark --mark 0x1000000 -j MASQUERADE' <<<"$nat_rules")" -eq 1 ] \
        && ok "owned NAT drift triggers reconciliation" \
        || bad "owned NAT drift survived early exit"

    head_ "equivalent kernel rule spelling converges exactly"
    ip rule add fwmark 0x1/0xffffffff lookup ioa pref 2500
    ip rule add to 8.8.8.8 lookup main pref 2500
    run_script 0 >/dev/null
    [ "$(band 2500)" = "$base2500" ] \
        && ok "equivalent duplicate and foreign stale rule are removed" \
        || bad "pref 2500 did not converge exactly: $(band 2500)"

    head_ "routefile failures preserve active policy"
    local cn_before marking_before malicious_owner_before table52_before invalid_route
    cn_before=$(ip route show table cn | sed -E 's/[[:space:]]+$//' | sort)
    marking_before=$(snapshot_marking)
    printf 'route add malformed via GATEWAY table cn\n' > "$WORK/routefile"
    [ "$(run_status 1)" -ne 0 ] \
        && ok "malformed routefile fails the run" || bad "malformed routefile was accepted"
    [ "$(ip route show table cn | sed -E 's/[[:space:]]+$//' | sort)" = "$cn_before" ] \
        && ok "failed staging batch preserves old cn" || bad "failed batch changed cn"
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
    [ "$(run_status 1)" -ne 0 ] \
        && ok "routefile rejects commands outside the route grammar" \
        || bad "routefile executed an out-of-grammar command"
    [ "$(snapshot_owner_state)" = "$malicious_owner_before" ] &&
        [ "$(ip route show table 52 | sort)" = "$table52_before" ] \
        && ok "rejected routefile preserves owner state and table 52" \
        || bad "rejected routefile changed owner state or table 52"
    [ "$(ip route show table cn | sed -E 's/[[:space:]]+$//' | sort)" = "$cn_before" ] \
        && ok "rejected routefile preserves old cn" || bad "rejected routefile changed cn"

    for invalid_route in \
        'route add 256.0.0.1/24 via GATEWAY table cn' \
        'route add 1.0.1.0/33 via GATEWAY table cn' \
        'route add 1.0.1.0/24 via 10.36.48.1 table cn' \
        'route add 1.0.1.0/24 via GATEWAY table main' \
        'route add 1.0.1.0/24 via GATEWAY table cn metric 1'; do
        printf '%s\n' "$invalid_route" > "$WORK/routefile"
        [ "$(run_status 1)" -ne 0 ] \
            && ok "routefile rejects: $invalid_route" \
            || bad "routefile accepted: $invalid_route"
    done

    printf '%s\n' \
        'route add 1.0.1.0/24 via GATEWAY table cn' \
        'route replace 1.0.1.0/24 via GATEWAY table cn' > "$WORK/routefile"
    [ "$(run_status 1)" -ne 0 ] \
        && ok "routefile rejects duplicate destination prefixes" \
        || bad "routefile accepted duplicate destination prefixes"

    printf 'route add 1.0.1.0/24 via GATEWAY table cn\nroute add 1.0.2.0/23 via GATEWAY table cn\n' > "$WORK/routefile"
    run_script 1 >/dev/null

    head_ "missing and empty routefiles authoritatively disable cn"
    rm -f "$WORK/routefile"
    run_script 1 >/dev/null
    [ "$(routes cn)" -eq 0 ] && [ "$(count 1500)" -eq 0 ] \
        && ok "missing routefile converges cn table and rule to empty" \
        || bad "missing routefile left cn policy"
    : > "$WORK/routefile"
    ip route add blackhole 203.0.113.0/24 table cn
    ip rule add lookup cn pref 1500
    run_script 1 >/dev/null
    [ "$(routes cn)" -eq 0 ] && [ "$(count 1500)" -eq 0 ] \
        && ok "empty routefile converges cn table and rule to empty" \
        || bad "empty routefile left cn policy"
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
    [ "$(band 2500)" = "$base2500" ] && ok "2500 unchanged" || bad "2500 drifted"
    [ "$(band 1000)" = "$base1000" ] && ok "1000 unchanged" || bad "1000 drifted"

    head_ "foreign rule injected into a band we own"
    ip rule add to 8.8.8.8 lookup main pref 2500
    [ "$(count 2500)" -gt "$(wc -l <<<"$base2500")" ] || bad "injection did not take"
    run_script 0 >/dev/null
    if [ "$(band 2500)" = "$base2500" ]; then
        ok "foreign rule removed without FORCE"
    else
        bad "foreign rule survived: $(band 2500 | grep 8.8.8.8)"
    fi

    head_ "our rule deleted from a band we own"
    ip rule del to 10.0.0.0/8 lookup ioa pref 2500 2>/dev/null
    run_script 0 >/dev/null
    [ "$(band 2500)" = "$base2500" ] && ok "missing rule restored without FORCE" \
                                     || bad "not restored"

    head_ "whole band deleted"
    while ip rule del pref 1000 2>/dev/null; do :; done
    run_script 0 >/dev/null
    [ "$(band 1000)" = "$base1000" ] && ok "band rebuilt without FORCE" \
                                     || bad "band not rebuilt"

    head_ "band is never empty during a rebuild"
    local zero=0 i
    ( for i in $(seq 1 400); do count 2500; sleep 0.01; done > "$WORK/samples" ) &
    local sampler=$!
    sleep 0.3
    run_script 1 >/dev/null
    wait $sampler
    zero=$(grep -cx 0 "$WORK/samples")
    [ "$zero" -eq 0 ] && ok "2500 never empty ($(wc -l < "$WORK/samples") samples)" \
                      || bad "2500 was empty $zero times"

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
    # The bug that took the machine offline: public DHCP resolvers are inside no
    # private range, so if they are not named here they follow the exit node.
    if grep -q 'to 202.152.254.230 lookup main' <<<"$b1000" &&
       grep -q 'to 202.152.254.65 lookup main'  <<<"$b1000"; then
        ok "both public DHCP resolvers pinned at 1000"
    else
        bad "public DHCP resolvers not pinned: $b1000"
    fi
    # A rule that outlives the AP it was built for is a black hole, not a leftover.
    if grep -qE 'to (10\.36\.48\.|10\.36\.32\.0/20)' <<<"$b1000"; then
        bad "stale rules from the previous AP survived: $b1000"
    else
        ok "no rule from the previous AP survived"
    fi

    # Tables carry the gateway; rules do not. Put desired prefixes back on the old
    # gateway and add one truly stale prefix, reproducing replace-then-delete failure.
    ip route add 10.36.48.1/32 dev wlan0 scope link 2>/dev/null || true
    ip route show table "$CN_TABLE" |
        sed -E 's/ via [^ ]+ dev [^ ]+/ via 10.36.48.1 dev wlan0/; s/$/ table cn/; s/^/route replace /' |
        ip -batch -
    ip route replace blackhole 198.51.100.0/24 table "$CN_TABLE"
    owner_before=$(snapshot_owner_state)
    rm -f "$WORK/cn-last-applied"
    : > "$WORK/ip-calls"
    : > "$WORK/ip-batch-commands"
    FORCE_CN=1 run_script 0 >/dev/null
    rc=$?
    [ "$rc" -eq 0 ] && ok "CN gateway-change promotion exits cleanly" \
                    || bad "CN gateway-change promotion exited $rc"
    if ip route show table "$CN_TABLE" 2>/dev/null | grep -q '10.36.48.1'; then
        bad "cn table still points at the previous gateway"
    elif ip route show table "$CN_TABLE" 2>/dev/null |
            grep -Eq 'via 10\.36\.40\.1 dev wlan0($| )'; then
        ok "cn table rebuilt onto the new physical gateway and device"
    else
        bad "cn table has no route via the new physical gateway and device"
    fi
    if ip route show table "$CN_TABLE" | grep -Fq '198.51.100.0/24'; then
        bad "CN gateway-change promotion retained a stale-only prefix"
    else
        ok "CN gateway-change promotion removes stale-only prefixes"
    fi
    if grep -Eq '^route replace (1\.0\.1\.0/24|1\.0\.2\.0/23|10\.20\.0\.0/16|100\.12\.34\.0/24) via 10\.36\.40\.1 dev wlan0 table cn_stage$' \
            "$WORK/ip-batch-commands"; then
        ok "CN staging pins the physical device explicitly"
    else
        bad "CN staging omitted or selected the wrong physical device"
    fi
    if grep -Eq '^route del (1\.0\.1\.0/24|1\.0\.2\.0/23|10\.20\.0\.0/16|100\.12\.34\.0/24)' \
            "$WORK/ip-batch-commands"; then
        bad "promotion deletes a desired prefix after replacing its gateway"
    else
        ok "promotion does not delete desired prefixes after gateway replacement"
    fi
    [ "$(snapshot_owner_state)" = "$owner_before" ] \
        && ok "CN gateway change preserves tunnel-owned state" \
        || bad "CN gateway change modified tunnel-owned state"

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
    while ip rule del pref 2500 2>/dev/null; do :; done
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
    [ "$(count 2500)" -gt 0 ] && ok "2500 rebuilt from nothing ($(count 2500) rules)" \
                              || bad "2500 still empty after a cold start"
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
    ip netns list 2>/dev/null | sed 's/^/netns /'
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
    trap 'rm -rf "$WORK"' EXIT
    # `unshare -r` maps the caller to nobody inside the namespace, so everything
    # it must read or execute has to be world-accessible from out here.
    chmod 755 "$WORK"
    build_under_test || { echo "could not build the script under test"; exit 1; }
    [ -s "$WORK/nr-under-test" ] || { echo "the built copy is empty"; exit 1; }
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
