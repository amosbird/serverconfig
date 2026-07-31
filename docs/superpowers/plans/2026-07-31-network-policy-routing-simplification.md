# Network Policy Routing Simplification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Simplify local policy routing so actual LAN and routefile destinations are direct, SmartDNS and confirmed intranet ranges select IOA, unmatched traffic remains on the selected Tailscale exit node, and both tunnels retain exclusive ownership of their bootstrap and recovery.

**Architecture:** Keep one reconciler for objects this repository owns: connected-LAN exceptions, the `cn` direct-route table, SmartDNS fragments, the `ioa_intranet` ipset, and a minimal mark chain. Tailscale owns mark `0x80000`, table 52, DERP, and exit-node recovery; SmartGateAgent owns mark `0xa38`, tables 20/230, and table `ioa` contents. Remove endpoint guessing, underlay tables, broad private-network exceptions, and all exit-node fallback behavior.

**Tech Stack:** Bash, iproute2 policy routing, iptables/ipset, systemd-networkd, SmartDNS, Tailscale, network namespaces, ShellCheck.

---

## File Structure

| Path | Responsibility after this change |
|---|---|
| `scripts/overrides/ip` | Rewrite only SmartGateAgent's unqualified tun0 default into table `ioa`; pass every other command to real `ip` unchanged. |
| `network/test-ip-override.sh` | Runnable contract test for the `ip` wrapper using a fake `ip` binary. |
| `scripts/network-reconfigure` | Reconcile only owned LAN/direct/IOA-business/tailnet policy and SmartDNS fragments. |
| `network/test-reconfigure.sh` | Namespace tests for ordering, mark isolation, ownership, AP roaming, cold boot, and mutation sensitivity. |
| `scripts/network-status` | Read-only status for the simplified policy, without fallback or endpoint-cache concepts. |
| `scripts/netfix` | Repair and diagnose owned policy only; never change Tailscale preferences. |
| `restore.sh` | Install/enable only `network-reconfigure` units. |
| `network/migrate.sh` | Install/remove the simplified policy without touching SmartGateAgent or Tailscale-owned tables/preferences. |
| `network/systemd/network-fallback.service` | Delete. |
| `scripts/network-fallback` | Delete. |
| `network/README.md` | Describe final ownership, ordering, failure semantics, and checks. |

Do not touch or commit the user's unrelated `scripts/flameshot`, `scripts/gremote2.sh`, or `.config/wireplumber/` changes.

### Intended owned priority bands

Use these constants consistently in code, tests, status, migration, and docs:

```bash
P_TS=500       # Tailscale-owned packets escape our business policy
P_LOCAL=1000   # current scope-link networks, physical gateway, DHCP DNS
P_CN=1500      # ~/.routefile; before every IOA business rule
P_IOA=2500     # exact mark 0x1, 10/8, 100.12/16
P_TAILNET=3000 # 100.64/10 -> table 52
```

SmartGateAgent chooses the priority of its own `0xa38 -> table 20` rule. Do not move or duplicate it. Routefile's “absolute priority” means it overrides all repository-owned IOA **business** rules, including mark `0x1`; owner-marked SmartGateAgent control packets remain SmartGateAgent's responsibility. Both `cn` and table 20 resolve to the physical network, so this ownership exception does not change their physical egress.

## Task 1: Make the SmartGateAgent `ip` Override Exact and Transparent

**Files:**
- Create: `network/test-ip-override.sh`
- Modify: `scripts/overrides/ip`

- [ ] **Step 1: Write the failing wrapper contract test**

Create `network/test-ip-override.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
LOG="$WORK/ip.log"
FAKE="$WORK/ip"

cat > "$FAKE" <<'FAKE'
#!/usr/bin/env bash
printf '%q ' "$@" >> "$IP_LOG"
printf '\n' >> "$IP_LOG"
FAKE
chmod +x "$FAKE"

run() {
    : > "$LOG"
    IP_REAL="$FAKE" IP_LOG="$LOG" "$ROOT/scripts/overrides/ip" "$@"
    cat "$LOG"
}

expect() {
    local name="$1" expected="$2"
    shift 2
    local actual
    actual=$(run "$@")
    if [ "$actual" != "$expected" ]; then
        printf 'FAIL %s\nexpected: %s\nactual:   %s\n' "$name" "$expected" "$actual" >&2
        return 1
    fi
    printf 'OK   %s\n' "$name"
}

expect 'tun default is isolated' \
    'route add default via 192.168.255.1 dev tun0 table ioa metric 101 ' \
    route add default via 192.168.255.1 dev tun0
expect 'SmartGateAgent mark rule passes through' \
    'rule add fwmark 2616 table 20 ' \
    rule add fwmark 2616 table 20
expect 'SmartGateAgent mark delete passes through' \
    'rule del fwmark 2616 table 20 ' \
    rule del fwmark 2616 table 20
expect 'SmartGateAgent source rule passes through' \
    'rule add from 192.0.2.10 table 230 ' \
    rule add from 192.0.2.10 table 230
expect 'SmartGateAgent source delete passes through' \
    'rule del from 192.0.2.10 table 230 ' \
    rule del from 192.0.2.10 table 230
expect 'unknown rule is not swallowed' \
    'rule add to 198.51.100.0/24 table 77 ' \
    rule add to 198.51.100.0/24 table 77
expect 'table flush passes through' \
    'route flush table 230 ' \
    route flush table 230
```

- [ ] **Step 2: Run it and confirm the source-rule case fails for the right reason**

Run:

```bash
bash network/test-ip-override.sh
```

Expected: FAIL at `SmartGateAgent source rule passes through` because the current wrapper silently exits for unrecognised `rule` commands and the fake log is empty.

- [ ] **Step 3: Replace the override with the minimum transparent implementation**

Replace `scripts/overrides/ip` with:

```bash
#!/usr/bin/env bash

IP_REAL=${IP_REAL:-/usr/bin/ip}

if [ "$*" = "route add default via 192.168.255.1 dev tun0" ]; then
    exec "$IP_REAL" route add default via 192.168.255.1 dev tun0 table ioa metric 101
fi

exec "$IP_REAL" "$@"
```

This deliberately has no generic `rule` branch. SmartGateAgent owns its table 20/230 lifecycle; unknown commands must reach real `ip` and return its real status.

- [ ] **Step 4: Restore executable mode and verify the contract**

Run:

```bash
chmod +x scripts/overrides/ip network/test-ip-override.sh
bash network/test-ip-override.sh
bash -n scripts/overrides/ip network/test-ip-override.sh
shellcheck -S error scripts/overrides/ip network/test-ip-override.sh
```

Expected: seven `OK` lines, then both syntax/static checks exit 0.

- [ ] **Step 5: Commit**

```bash
git add scripts/overrides/ip network/test-ip-override.sh
git commit -m "network: preserve SmartGateAgent routing ownership"
```

## Task 2: Specify the Simplified Policy in Namespace Tests

**Files:**
- Modify: `network/test-reconfigure.sh`

- [ ] **Step 1: Change the fixture so owner tables can be protected**

In `setup()`, keep table `ioa`, and add owner sentinels:

```bash
ip route add default via 192.168.255.1 dev tun0 table 400
ip route add default via 10.36.48.1 dev wlan0 table 20
ip route add default via 10.36.48.1 dev wlan0 table 230
ip route add blackhole 198.18.0.0/15 table 52
ip rule add fwmark 0xa38 lookup 20 pref 490
```

Add helpers after `routes()`:

```bash
snapshot_owner_tables() {
    for table in 20 230 52; do
        printf '%s|' "$table"
        ip route show table "$table" | sort | paste -sd';' -
    done
}

route_table() {
    ip route get "$1" ${2:-} 2>/dev/null |
        awk '{for (i=1; i<=NF; i++) if ($i == "table") {print $(i+1); exit}}
}
```

- [ ] **Step 2: Replace obsolete underlay/escape tests with failing ownership and ordering tests**

Delete these old sections:

- `IOA escape table stale after a roam`
- `duplicate IOA escape rule at a derived priority`
- all assertions that `underlay` is populated or follows a gateway

Before the first reconciler run, capture:

```bash
local owner_before
owner_before=$(snapshot_owner_tables)
```

After baseline, add assertions:

```bash
head_ "policy ownership and ordering"
[ "$(count 500)" -eq 1 ] && ok "Tailscale mark has one early escape rule" \
                           || bad "Tailscale mark escape is missing or duplicated"
[ "$(count 1500)" -eq 1 ] && ok "routefile has one direct lookup rule" \
                            || bad "routefile direct rule missing"
[ "$(count 2500)" -eq 3 ] && ok "IOA has exact mark and two static CIDRs" \
                            || bad "unexpected IOA business rule count: $(count 2500)"
! band 2500 | grep -q '9.0.0.0/8' && ok "9/8 is not statically routed to IOA" \
                                   || bad "9/8 still has a static IOA rule"
! ip -4 rule show | grep -qE 'to (192\.168\.0\.0/16|172\.16\.0\.0/12|169\.254\.0\.0/16)' \
    && ok "no broad private-network bypass remains" \
    || bad "broad private-network bypass remains"
[ "$(snapshot_owner_tables)" = "$owner_before" ] \
    && ok "tables 20, 230 and 52 are untouched" \
    || bad "a tunnel-owned table changed"
```

Extend the fixture routefile with overlapping direct prefixes:

```bash
printf 'route add 10.20.0.0/16 via GATEWAY table cn\n' >> "$WORK/routefile"
printf 'route add 100.12.34.0/24 via GATEWAY table cn\n' >> "$WORK/routefile"
```

Then assert actual routing after baseline:

```bash
head_ "direct routes override IOA business policy"
[ "$(route_table 10.20.1.1)" = cn ] && ok "routefile overrides static 10/8 IOA" \
                                    || bad "routefile lost to static 10/8"
[ "$(route_table 100.12.34.5)" = cn ] && ok "routefile overrides static 100.12/16 IOA" \
                                       || bad "routefile lost to static 100.12/16"
[ "$(route_table 10.20.1.1 'mark 0x1')" = cn ] \
    && ok "routefile overrides SmartDNS business mark" \
    || bad "routefile lost to SmartDNS mark"
[ "$(route_table 10.36.48.1)" = main ] && ok "connected 10/8 LAN overrides IOA" \
                                        || bad "connected LAN routed into IOA"
```

- [ ] **Step 3: Add mark-isolation structure checks**

After the reconciler has created `NETMODE_IOA`, create test addresses and inspect the exact kernel-facing chain/rule representation:

```bash
head_ "NETMODE_IOA only classifies unmarked business packets"
ipset add ioa 9.1.2.3 -exist
ipset add ioa 10.20.1.1 -exist

chain=$(iptables -t mangle -S NETMODE_IOA)
grep -q -- '! --mark 0x0/0xffffffff -j RETURN' <<<"$chain" \
    && ok "all non-zero marks are preserved" || bad "non-zero mark guard missing"
grep -q -- '--set-xmark 0x1/0xffffffff' <<<"$chain" \
    && ok "IOA business mark is written exactly" || bad "IOA mark write is not exact"
grep -q 'fwmark 0x1 lookup ioa' <<<"$(band 2500)" \
    && ok "IOA rule matches exact mark" || bad "IOA rule still matches only bit zero"
```

Add route assertions proving owner marks are not confused by the exact policy-rule mask:

```bash
[ "$(route_table 10.30.1.1 'mark 0xa38')" = 20 ] \
    && ok "SmartGateAgent mark remains owner-routed" \
    || bad "SmartGateAgent mark was captured"
[ "$(route_table 10.30.1.1 'mark 0x80000')" = main ] \
    && ok "Tailscale mark remains owner-routed" \
    || bad "Tailscale mark was captured"
[ "$(route_table 10.30.1.1 'mark 0xa39')" != ioa ] \
    && ok "low-bit collision does not match IOA" \
    || bad "0xa39 incorrectly matched IOA business mark"
```

- [ ] **Step 4: Make roaming prove owner tables remain unchanged**

Remove the line in `roam_to()` that rewrites table 20. Capture owner tables immediately before roaming and assert byte equality after reconfigure:

```bash
owner_before=$(snapshot_owner_tables)
roam_to 10.36.43.250/21 10.36.40.1 202.152.254.230 202.152.254.65
rc=$(run_status 0)
[ "$(snapshot_owner_tables)" = "$owner_before" ] \
    && ok "roam leaves tables 20, 230 and 52 to tunnel owners" \
    || bad "roam modified a tunnel-owned table"
```

Keep existing assertions for new LAN, gateway, both public DHCP resolvers, stale local rules, `cn` gateway, and cold boot. Cold boot must flush only owned bands and `cn`; do not flush 20, 230, 52, or `ioa`.

- [ ] **Step 5: Run the new test and verify RED failures**

Run:

```bash
sudo -n bash network/test-reconfigure.sh
```

Expected failures include at least:

- routefile direct rule missing at pref 1500;
- static `9/8` still present;
- broad private bypass remains;
- owner table 20 changed or its rule was reconciled;
- old IOA mark is not exact.

The old production script may produce additional failures. Record them before implementation; do not weaken assertions to accommodate current behavior.

- [ ] **Step 6: Commit the failing specification**

```bash
git add network/test-reconfigure.sh
git commit -m "test: specify independent IOA and Tailscale policy"
```

## Task 3: Simplify `network-reconfigure` to Owned Policy Only

**Files:**
- Modify: `scripts/network-reconfigure`
- Test: `network/test-reconfigure.sh`

- [ ] **Step 1: Replace constants and priority bands**

Use this policy constant block:

```bash
IOA_MARK="0x1/0xffffffff"
TS_MARK="0x80000/0xff0000"
CHAIN="NETMODE_IOA"
IOA_INTRANET_SET="ioa_intranet"
IOA_INTRANET_CIDRS=("9.0.0.0/8" "10.0.0.0/8" "100.12.0.0/16")
IOA_STATIC_CIDRS=("10.0.0.0/8" "100.12.0.0/16")
TAILNET_NETS=("100.64.0.0/10")
CN_TABLE="cn"
IOA_TABLE="ioa"
IOA_RESOLVER="192.168.255.10"
ROUTEFILE="/home/amos/.routefile"
P_TS=500
P_LOCAL=1000
P_CN=1500
P_IOA=2500
P_TAILNET=3000
```

Delete constants for:

```text
UNDERLAY_TABLE, UNDERLAY_STAGE, TS_CONTROL_NETS, DNS_UPSTREAMS,
IOA_BOOTSTRAP_HOSTS, DERP_CACHE, IOA_ENDPOINT_CACHE, PRIVATE_NETS,
IOA_ESCAPE_MARK, IOA_ESCAPE_TABLE, P_IOA_UNDERLAY, P_UNDERLAY,
P_IOA_EXCLUDE, P_PRIVATE
```

Keep `CACHE_DIR` and `STATE_FILE` only for the applied physical-link fingerprint.

- [ ] **Step 2: Remove tunnel-owned discovery and reconciliation**

Delete complete code paths that:

- call `tailscale debug derp-map`;
- resolve `IOA_BOOTSTRAP_HOSTS`;
- scan `ss` for SmartGateAgent endpoints;
- build/stage/swap `underlay` table 500/501;
- inspect, delete, re-pin, or validate mark `0xa38` or table 20;
- generate broad private-network rules;
- invoke `tailscale set --exit-node-allow-lan-access=true`;
- remove Tailscale's pref 5270 rule;
- validate cached endpoint or DERP route decisions.

Simplify `abort()` to remove only this repository's marking jump and log the failure:

```bash
abort() {
    local rc=$1 line=$2
    logger -t network-reconfigure -p daemon.err \
        "aborted at line $line (exit $rc); removing the incomplete IOA mark hook"
    iptables -t mangle -D OUTPUT -j "${CHAIN:-NETMODE_IOA}" 2>/dev/null || true
}
```

Do not modify Tailscale or SmartGateAgent state in error handling.

- [ ] **Step 3: Build the desired policy from one source of truth**

Populate `DESIRED_BANDS` as follows:

```bash
declare -A DESIRED_BANDS

DESIRED_BANDS[$P_TS]="from all fwmark $TS_MARK lookup main"

local_band=()
while read -r subnet _; do
    [ -n "$subnet" ] && local_band+=("from all to $subnet lookup main")
done < <({ ip route show table main scope link 2>/dev/null || true; })
for addr in "$(physical_gateway)" ${dns:-}; do
    [ -n "$addr" ] && local_band+=("from all to $addr lookup main")
done
DESIRED_BANDS[$P_LOCAL]=$(band_of "${local_band[@]}")

if has_routes "$CN_TABLE"; then
    DESIRED_BANDS[$P_CN]="from all lookup $CN_TABLE"
else
    DESIRED_BANDS[$P_CN]=""
fi

ioa_band=("from all fwmark $IOA_MARK lookup $IOA_TABLE")
for cidr in "${IOA_STATIC_CIDRS[@]}"; do
    ioa_band+=("from all to $cidr lookup $IOA_TABLE")
done
DESIRED_BANDS[$P_IOA]=$(band_of "${ioa_band[@]}")

tailnet_band=()
for net in "${TAILNET_NETS[@]}"; do
    tailnet_band+=("from all to $net lookup 52")
done
DESIRED_BANDS[$P_TAILNET]=$(band_of "${tailnet_band[@]}")
```

Because `cn` is rebuilt later, update `DESIRED_BANDS[$P_CN]` immediately after that rebuild before the common apply loop, as the existing code already does for pref 4000.

- [ ] **Step 4: Make the mark chain preserve every existing owner mark**

Create the chain with these exact semantics:

```bash
iptables -t mangle -N "$CHAIN" 2>/dev/null || true
iptables -t mangle -F "$CHAIN"
iptables -t mangle -A "$CHAIN" ! -m mark --mark 0x0/0xffffffff -j RETURN
ensure_ipset ioa
ensure_ipset "$IOA_INTRANET_SET" hash:net
ipset flush "$IOA_INTRANET_SET"
for cidr in "${IOA_INTRANET_CIDRS[@]}"; do
    ipset add "$IOA_INTRANET_SET" "$cidr"
done
iptables -t mangle -A "$CHAIN" \
    -m set --match-set ioa dst \
    -m set --match-set "$IOA_INTRANET_SET" dst \
    -j MARK --set-xmark 0x1/0xffffffff
iptables -t mangle -C OUTPUT -j "$CHAIN" 2>/dev/null ||
    iptables -t mangle -I OUTPUT 1 -j "$CHAIN"
```

Keep the current Tailscale mark restoration in PREROUTING/OUTPUT only if it is Tailscale-installed state that the existing script merely preserves; do not add new ownership of Tailscale chains.

- [ ] **Step 5: Keep only physical-link change detection**

The applied fingerprint remains:

```text
physical interface | physical gateway | sorted DHCP resolvers
```

The early exit requires:

- state fingerprint unchanged;
- `cn` has routes via current physical gateway;
- owned `NETMODE_IOA` jump exists;
- desired owned rule fingerprint equals actual owned rule fingerprint.

It must not require underlay, endpoint caches, table 20, table 230, table 52 contents, tun0 state, or Tailscale state.

- [ ] **Step 6: Remove obsolete runtime state safely**

At successful convergence, remove only obsolete repository state and tables:

```bash
rm -f "$CACHE_DIR/derp-ips" "$CACHE_DIR/ioa-endpoints"
ip route flush table 500 2>/dev/null || true
ip route flush table 501 2>/dev/null || true
```

Remove the `500 underlay` rt_tables line, but retain `101 cn` and `400 ioa`. Do not flush table 20, 230, 52, or `ioa`.

- [ ] **Step 7: Run focused GREEN verification**

Run:

```bash
bash -n scripts/network-reconfigure
shellcheck -S error scripts/network-reconfigure
sudo -n bash network/test-reconfigure.sh
```

Expected: syntax/static checks pass and all namespace assertions pass.

- [ ] **Step 8: Mutation-check the new assertions**

Save and restore the file around each mutation. At minimum prove these mutations fail:

```text
1. change P_CN=1500 to P_CN=3500
   -> routefile override checks fail
2. add 9.0.0.0/8 to IOA_STATIC_CIDRS
   -> static 9/8 check fails
3. change 0x1/0xffffffff to 0x1/0x1
   -> exact-mark / low-bit collision check fails
4. remove the non-zero-mark RETURN
   -> mark preservation check fails
5. add any route flush for table 20
   -> owner-table snapshot check fails
```

Use a temporary copy and restore `scripts/network-reconfigure` after every mutation. Never run mutations on the live network; invoke only `network/test-reconfigure.sh`, which enters its namespace.

- [ ] **Step 9: Commit**

```bash
git add scripts/network-reconfigure network/test-reconfigure.sh
git commit -m "network: reduce routing to explicit business policy"
```

## Task 4: Delete Exit-Node Fallback and Remove Deployment Hooks

**Files:**
- Delete: `scripts/network-fallback`
- Delete: `network/systemd/network-fallback.service`
- Modify: `restore.sh`
- Modify: `network/migrate.sh`
- Test: `network/test-static-policy.sh`

- [ ] **Step 1: Write a failing static ownership test**

Create `network/test-static-policy.sh`:

```bash
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
reject 'no fallback deployment references' 'network-fallback' restore.sh network/migrate.sh
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

exit "$fail"
```

- [ ] **Step 2: Run it and confirm it fails on existing fallback artifacts**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: failures for existing script/unit, deployment references, `netfix` exit-node mutation, endpoint/DERP caches, and underlay references.

- [ ] **Step 3: Delete fallback and remove install/enable references**

Run:

```bash
rm scripts/network-fallback network/systemd/network-fallback.service
```

In `restore.sh`, copy only:

```bash
sudo cp "$DIR"/network/systemd/network-{reconfigure.path,reconfigure.service} \
    /etc/systemd/system/
```

Enable only:

```bash
sudo systemctl enable network-reconfigure.path
```

In `network/migrate.sh`:

- stop copying/enabling `network-fallback.service`;
- during install/activation, disable and remove an obsolete installed fallback unit without touching Tailscale:

```bash
systemctl disable --now network-fallback.service 2>/dev/null || true
rm -f /etc/systemd/system/network-fallback.service
rm -rf /var/lib/network-fallback
systemctl daemon-reload
```

- rollback removes only reconfigure units;
- rollback flushes owned priorities `500 1000 1500 2500 3000`, table `cn`, the owned chain, and `ioa_intranet`;
- rollback may flush obsolete tables 500/501 as repository leftovers;
- rollback must not flush table `20`, `230`, `52`, or `ioa`, and must not change Tailscale preferences.

- [ ] **Step 4: Run syntax and the still-partial static test**

Run:

```bash
chmod +x network/test-static-policy.sh
bash -n restore.sh network/migrate.sh network/test-static-policy.sh
shellcheck -S error network/migrate.sh network/test-static-policy.sh
bash network/test-static-policy.sh
```

Expected: fallback deployment checks now pass; tool/status checks can remain red until Task 5.

- [ ] **Step 5: Commit the deletion and deployment cleanup**

```bash
git add restore.sh network/migrate.sh network/test-static-policy.sh
git rm scripts/network-fallback network/systemd/network-fallback.service
git commit -m "network: leave exit-node recovery to Tailscale"
```

## Task 5: Make Operator Tools Read-Only Toward Both Tunnels

**Files:**
- Modify: `scripts/netfix`
- Modify: `scripts/network-status`
- Test: `network/test-static-policy.sh`

- [ ] **Step 1: Reduce `netfix` to owned-policy repair**

Remove the detached watchdog, `/run/netfix-restore`, every `tailscale set/down/up`, the 60-second exit-node soak, fallback state display, DERP endpoint probes, endpoint-cache validation, and underlay route-count checks.

The command path should be:

```bash
say "RECONFIGURE"
if timeout 120 env FORCE=1 "$RECONFIGURE" "$iface"; then
    ok "network policy reconciled"
else
    bad "network policy reconcile failed"
    fail=1
fi
```

Checks should cover only:

```bash
n=$(ip route show table cn 2>/dev/null | wc -l)
[ "$n" -gt 1000 ] && ok "direct table: $n routes" || {
    bad "direct table nearly empty ($n routes)"; fail=1;
}

for spec in \
    '500:fwmark 0x80000/0xff0000 lookup main' \
    '1500:lookup cn' \
    '2500:fwmark 0x1 lookup ioa' \
    '2500:to 10.0.0.0/8 lookup ioa' \
    '2500:to 100.12.0.0/16 lookup ioa' \
    '3000:to 100.64.0.0/10 lookup 52'
do
    pref=${spec%%:*}; text=${spec#*:}
    ip rule show pref "$pref" | grep -Fq "$text" && ok "$text" || {
        bad "missing: $text"; fail=1;
    }
done
```

Keep connectivity probes as diagnostics only. In fail-closed mode, an unavailable exit node is not repaired by bypassing it. Report Tailscale status and health read-only:

```bash
tailscale status --json 2>/dev/null | jq -r '
    "exit node: " +
    (if .ExitNodeStatus == null then "none"
     else "\(.ExitNodeStatus.ID) online=\(.ExitNodeStatus.Online)" end),
    (if (.Health | length) > 0 then "health: \(.Health | join("; "))" else empty end)'
```

- [ ] **Step 2: Rewrite `network-status` around final ownership**

Use sections:

```text
interfaces
local direct
routefile direct
IOA business
Tailscale
```

Report:

- current physical default and scope-link routes;
- pref 1000 local rules;
- `cn` route count and pref 1500 presence;
- table `ioa` availability, pref 2500 rules, `tun0` state, and SmartGateAgent's `0xa38` rule as read-only owner state;
- table 52 population, pref 3000 tailnet rule, selected exit node and health.

Delete `DETACHED_FILE`, underlay routes, DERP/control-plane counts, fallback messages, and any suggestion that this repository repairs table 20.

- [ ] **Step 3: Run static and script checks**

Run:

```bash
chmod +x scripts/netfix scripts/network-status
bash -n scripts/netfix scripts/network-status
shellcheck -S error scripts/netfix scripts/network-status
bash network/test-static-policy.sh
```

Expected: all static checks pass.

- [ ] **Step 4: Verify status is read-only and useful**

Run:

```bash
before=$(sha256sum /var/lib/tailscale/tailscaled.state 2>/dev/null || true)
scripts/network-status
after=$(sha256sum /var/lib/tailscale/tailscaled.state 2>/dev/null || true)
[ "$before" = "$after" ]
```

Expected: status reports all five sections, exits 0, and Tailscale state hash is unchanged.

- [ ] **Step 5: Commit**

```bash
git add scripts/netfix scripts/network-status network/test-static-policy.sh
git commit -m "network: make repair tools respect tunnel ownership"
```

## Task 6: Update Architecture Documentation

**Files:**
- Modify: `network/README.md`
- Modify: `docs/superpowers/specs/2026-07-31-network-policy-routing-simplification-design.md` only if implementation exposed a concrete contradiction; do not rewrite approved decisions.

- [ ] **Step 1: Replace the old architecture narrative**

Rewrite `network/README.md` to contain these sections and facts:

```markdown
# Network Stack

## Components and ownership
- networkd/kernel: main and connected routes
- network-reconfigure: local exceptions, cn, IOA business selection, SmartDNS fragments
- SmartDNS: ipset ioa
- SmartGateAgent: tun0, table ioa contents, mark 0xa38, tables 20/230
- Tailscale: tailscale0, mark 0x80000, table 52, DERP, exit node and recovery

## Effective policy
1. Tailscale owner mark -> main
2. actual LAN/gateway/DHCP DNS -> main
3. routefile -> cn/direct
4. SmartGateAgent owner mark -> table 20
5. exact mark 0x1, 10/8, 100.12/16 -> ioa
6. 100.64/10 -> table 52
7. unmatched -> selected exit node

## SmartDNS IOA classification
- ipset ioa is domain-derived
- ioa_intranet is the safety boundary
- only unmarked packets can become exact mark 0x1
- 9/8 is domain-derived only, never a static IOA route

## Tunnel independence
- no DERP cache or IOA endpoint guessing
- no local exit-node health state machine
- fail closed
- no local manipulation of tables 20/230/52

## Reconciliation and AP changes
- physical identity only
- add-before-delete
- cn gateway rebuild
- tunnel events do not trigger routing rebuilds

## Testing
- exact commands for the three test scripts
- namespace-only mutation testing
- live read-only status command

## Migration and rollback
- owned objects only
```

Delete the historical fallback, underlay, DERP-cache, IOA endpoint-cache, table 20 repair, and known-limitation narratives that no longer describe the code. Keep concise root-cause context only where it explains a retained invariant (public DHCP DNS pinning, mark ownership, add-before-delete).

- [ ] **Step 2: Check docs against code mechanically**

Run:

```bash
grep -RInE 'network-fallback|derp-ips|ioa-endpoints|lookup underlay|P_PRIVATE|P_IOA_UNDERLAY' \
    network/README.md scripts/network-reconfigure scripts/netfix scripts/network-status \
    restore.sh network/migrate.sh && exit 1 || true

grep -q 'routefile.*IOA' network/README.md
grep -q 'fail.closed\|fail-closed' network/README.md
grep -q 'SmartDNS' network/README.md
grep -q 'table.*230' network/README.md
```

Expected: no obsolete active references; all retained architecture facts are documented.

- [ ] **Step 3: Commit**

```bash
git add network/README.md
git commit -m "docs: describe independent tunnel ownership"
```

## Task 7: Full Verification and Live Read-Only Audit

**Files:**
- No production changes unless a verification failure identifies a root cause; any fix must start with a failing test and a separate commit.

- [ ] **Step 1: Run all static checks**

```bash
bash -n \
    scripts/overrides/ip \
    scripts/network-reconfigure \
    scripts/netfix \
    scripts/network-status \
    network/test-ip-override.sh \
    network/test-reconfigure.sh \
    network/test-static-policy.sh \
    network/migrate.sh \
    restore.sh

shellcheck -S error \
    scripts/overrides/ip \
    scripts/network-reconfigure \
    scripts/netfix \
    scripts/network-status \
    network/test-ip-override.sh \
    network/test-reconfigure.sh \
    network/test-static-policy.sh \
    network/migrate.sh
```

Expected: all commands exit 0.

- [ ] **Step 2: Run all isolated tests fresh**

```bash
bash network/test-ip-override.sh
sudo -n bash network/test-reconfigure.sh
bash network/test-static-policy.sh
```

Expected: every check passes, with zero failures.

- [ ] **Step 3: Confirm removed artifacts are absent from the final diff**

```bash
[ ! -e scripts/network-fallback ]
[ ! -e network/systemd/network-fallback.service ]
git diff --check origin/master...HEAD
git status --short
```

Expected: only intentional project changes plus the user's pre-existing unrelated files; no generated cache, test work directory, debug log, or temporary script.

- [ ] **Step 4: Apply only the reconciler on the live system**

The implementation changes live symlinked scripts, but the current kernel still has old policy until reconcile. Run:

```bash
sudo -n env FORCE=1 /home/amos/scripts/network-reconfigure wlan0
```

Do not stop/restart `tailscaled`, `ngnclient`, or `smartdns`. The reconciler may restart SmartDNS only if its generated fragment content changed, per existing `write_if_changed` behavior.

Expected: exit 0.

- [ ] **Step 5: Perform a read-only route audit**

Run:

```bash
scripts/network-status
ip rule show
ip route get 10.20.1.1
ip route get 100.12.0.10
ip route get 100.100.100.100
ip route get 1.2.3.4
ip route get 10.30.1.1 mark 0xa38
ip route get 10.30.1.1 mark 0x80000
```

Expected, allowing current routefile contents and physical addresses to vary:

- current connected LAN uses `main`;
- a known routefile destination uses `cn`;
- non-routefile `100.12/16` uses `ioa` while IOA is up;
- `100.100.100.100` uses table 52;
- generic public traffic uses Tailscale's exit-node route when selected;
- `0xa38` follows SmartGateAgent table 20;
- `0x80000` follows main;
- no `underlay` rule remains.

- [ ] **Step 6: Verify tunnel preferences and owner tables were not mutated**

Capture before/after around the reconcile if repeating this check:

```bash
sudo -n sha256sum /var/lib/tailscale/tailscaled.state
ip route show table 20
ip route show table 230
ip route show table 52
systemctl is-active tailscaled ngnclient smartdns network-reconfigure.path
```

Expected: Tailscale state unchanged by reconcile; owner tables remain present as their daemons currently define them; all services active. Do not require a specific SmartGateAgent gateway in repository logic.

- [ ] **Step 7: Commit any final test-only adjustments, then request code review**

If no adjustment is needed, do not create an empty commit. Obtain base/head SHAs:

```bash
BASE_SHA=$(git merge-base HEAD origin/master)
HEAD_SHA=$(git rev-parse HEAD)
```

Request review against the approved design and fix every Critical/Important finding with a failing test first. Re-run Steps 1–6 after fixes.

- [ ] **Step 8: Finish the branch**

Invoke `superpowers:finishing-a-development-branch`. Do not push, merge, or discard without the user's selected option.
