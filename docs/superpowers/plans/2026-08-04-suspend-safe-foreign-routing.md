# Suspend-Safe Foreign Routing Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Preserve foreign routes and policy rules across networkd link transitions and make SmartGateAgent's IOA default route ineligible for tailscaled underlay detection.

**Architecture:** A global networkd drop-in disables garbage collection of routing objects owned by other daemons. The existing exact SmartGateAgent `ip` wrapper emits a gateway-free TUN default, which still routes IOA-selected packets but is skipped by tailscaled's Linux netlink fallback.

**Tech Stack:** Bash, systemd-networkd, iproute2, Linux network namespaces, existing shell test harnesses

---

## File Structure

| Path | Responsibility |
|---|---|
| `network/systemd-networkd.conf.d/foreign-routing.conf` | Declare global networkd ownership boundaries for foreign routes and rules. |
| `scripts/overrides/ip` | Rewrite only SmartGateAgent's exact tun0 default command to the gateway-free `ioa` route. |
| `network/test-ip-override.sh` | Contract-test the exact wrapper rewrite and transparent pass-through behavior. |
| `network/test-tun-underlay.sh` | Prove in a network namespace that the gateway-free TUN route works and has no gateway attribute. |
| `network/test-static-policy.sh` | Check networkd config deployment, rollback cleanup, and prohibition of tunnel preference mutations. |
| `restore.sh` | Install the networkd drop-in during GUI network deployment. |
| `network/migrate.sh` | Install the drop-in and remove it during rollback. |
| `network/README.md` | Document networkd foreign-object ownership and the no-gateway tun0 invariant. |

### Task 1: Specify and Implement the Gateway-Free IOA Route

**Files:**
- Modify: `network/test-ip-override.sh:35-37`
- Modify: `scripts/overrides/ip:5-7`

- [ ] **Step 1: Change the wrapper contract to require a gateway-free route**

Replace the first expectation with:

```bash
expect 'tun default is isolated and has no gateway' \
    'route add default dev tun0 table ioa metric 101 ' \
    route add default via 192.168.255.1 dev tun0
```

- [ ] **Step 2: Run the focused test and verify RED**

Run:

```bash
bash network/test-ip-override.sh
```

Expected: FAIL for `tun default is isolated and has no gateway`; actual output still contains
`via 192.168.255.1`.

- [ ] **Step 3: Make the exact wrapper rewrite gateway-free**

Change only the matching branch in `scripts/overrides/ip` to:

```bash
if [ "$*" = "route add default via 192.168.255.1 dev tun0" ]; then
    exec "$IP_REAL" route add default dev tun0 table ioa metric 101
fi
```

Keep the final transparent pass-through unchanged:

```bash
exec "$IP_REAL" "$@"
```

- [ ] **Step 4: Run the focused test and verify GREEN**

Run:

```bash
bash network/test-ip-override.sh
```

Expected: all 7 checks print `OK`, including the gateway-free expectation.

- [ ] **Step 5: Commit the wrapper behavior**

```bash
git add scripts/overrides/ip network/test-ip-override.sh
git commit -m "network: keep IOA default out of Tailscale underlay detection"
```

### Task 2: Characterize Gateway-Free TUN Routing in a Namespace

**Files:**
- Create: `network/test-tun-underlay.sh`

- [ ] **Step 1: Write the namespace check**

Create this executable script:

```bash
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
```

The script requires the caller to supply both forms of the host namespace identity and refuses to
run when either still matches. All mutations then occur directly in the disposable namespace.

- [ ] **Step 2: Run the check inside a verified outer namespace**

Run:

```bash
host_netns_link=$(readlink /proc/self/ns/net)
host_netns_inode=$(stat -Lc %i /proc/self/ns/net)
sudo -n env HOST_NETNS_LINK="$host_netns_link" HOST_NETNS_INODE="$host_netns_inode" \
    unshare --net --mount-proc bash network/test-tun-underlay.sh
```

Expected:

```text
OK   marked IOA traffic uses gateway-free tun0 default
OK   IOA default exposes no gateway to netlink consumers
```

- [ ] **Step 3: Verify the host guard**

Run:

```bash
sudo -n env HOST_NETNS_LINK="$(readlink /proc/self/ns/net)" \
    HOST_NETNS_INODE="$(stat -Lc %i /proc/self/ns/net)" \
    bash network/test-tun-underlay.sh
```

Expected: exit 1 and `REFUSE verified host network namespace`. No interfaces or routes are changed.

- [ ] **Step 4: Commit the runnable invariant**

```bash
git add network/test-tun-underlay.sh
git commit -m "test: verify tun0 cannot look like a gateway underlay"
```

### Task 3: Preserve Foreign Routing Objects in networkd

**Files:**
- Create: `network/systemd-networkd.conf.d/foreign-routing.conf`
- Modify: `network/test-static-policy.sh:421-465,735-755`
- Modify: `restore.sh:131-150`
- Modify: `network/migrate.sh:135-143,380-429`

- [ ] **Step 1: Add failing static deployment assertions**

Add after the SmartDNS rollback contract in `network/test-static-policy.sh`:

```bash
foreign_routing_config=network/systemd-networkd.conf.d/foreign-routing.conf
if [ ! -f "$foreign_routing_config" ] ||
   [ "$(grep -Fxc 'ManageForeignRoutingPolicyRules=no' "$foreign_routing_config")" -ne 1 ] ||
   [ "$(grep -Fxc 'ManageForeignRoutes=no' "$foreign_routing_config")" -ne 1 ]; then
    echo 'FAIL networkd does not preserve foreign routes and rules' >&2
    fail=1
else
    echo 'OK   networkd preserves foreign routes and rules'
fi
if ! grep -Fq 'systemd-networkd.conf.d/foreign-routing.conf' restore.sh ||
   ! grep -Fq 'systemd-networkd.conf.d/foreign-routing.conf' network/migrate.sh; then
    echo 'FAIL foreign-routing drop-in is not installed by both deployment paths' >&2
    fail=1
fi
if ! sed -n '/^rollback() {/,/^}/p' network/migrate.sh |
        grep -Fq 'rm -f /etc/systemd/networkd.conf.d/foreign-routing.conf'; then
    echo 'FAIL migration rollback does not remove the foreign-routing drop-in' >&2
    fail=1
fi
```

- [ ] **Step 2: Run static checks and verify RED**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: FAIL because the drop-in is absent and neither deployment path installs it.

- [ ] **Step 3: Add the networkd drop-in**

Create `network/systemd-networkd.conf.d/foreign-routing.conf`:

```ini
# Other routing owners: Tailscale, SmartGateAgent, and network-reconfigure.
[Network]
ManageForeignRoutingPolicyRules=no
ManageForeignRoutes=no
```

- [ ] **Step 4: Install it from restore.sh**

Immediately after creating `/etc/systemd/network`, add:

```bash
sudo mkdir -p /etc/systemd/networkd.conf.d
sudo cp "$DIR"/network/systemd-networkd.conf.d/foreign-routing.conf \
    /etc/systemd/networkd.conf.d/foreign-routing.conf
```

This is config installation only; do not restart networkd in `restore.sh`.

- [ ] **Step 5: Install and roll it back from migrate.sh**

In `install_configs()`, after copying `.network` files, add:

```bash
mkdir -p /etc/systemd/networkd.conf.d
cp "$DIR/systemd-networkd.conf.d/foreign-routing.conf" \
    /etc/systemd/networkd.conf.d/foreign-routing.conf
echo "  [ok] /etc/systemd/networkd.conf.d/foreign-routing.conf"
```

In `rollback()`, alongside removal of new networkd configs, add:

```bash
rm -f /etc/systemd/networkd.conf.d/foreign-routing.conf
```

- [ ] **Step 6: Run static checks and verify GREEN**

Run:

```bash
bash network/test-static-policy.sh
bash -n restore.sh network/migrate.sh
```

Expected: the new ownership checks print `OK`; syntax checks exit 0.

- [ ] **Step 7: Commit networkd ownership**

```bash
git add network/systemd-networkd.conf.d/foreign-routing.conf \
    network/test-static-policy.sh restore.sh network/migrate.sh
git commit -m "network: preserve routes owned outside networkd"
```

### Task 4: Document and Run the Complete Offline Verification

**Files:**
- Modify: `network/README.md:16-29,106-135`

- [ ] **Step 1: Document the ownership and underlay invariants**

Add to the ownership section:

```markdown
- **systemd-networkd** has `ManageForeignRoutingPolicyRules=no` and
  `ManageForeignRoutes=no`: it continues to own its addresses and DHCP routes in `main`, but does
  not garbage-collect route/rule objects owned by Tailscale, SmartGateAgent, or this repository.
```

Add to the tunnel-independence section:

```markdown
SmartGateAgent's table `ioa` default is `default dev tun0` without a gateway. Marked IOA traffic
still enters the TUN device, while tailscaled's Linux all-table fallback ignores the route because
it has no gateway. During a physical-link outage Tailscale must wait for a physical underlay; it
must never select `tun0`.
```

- [ ] **Step 2: Run syntax and focused tests**

```bash
bash -n scripts/overrides/ip network/test-ip-override.sh \
    network/test-tun-underlay.sh network/test-static-policy.sh \
    network/migrate.sh restore.sh
bash network/test-ip-override.sh
host_netns_link=$(readlink /proc/self/ns/net)
host_netns_inode=$(stat -Lc %i /proc/self/ns/net)
sudo -n env HOST_NETNS_LINK="$host_netns_link" HOST_NETNS_INODE="$host_netns_inode" \
    unshare --net --mount-proc bash network/test-tun-underlay.sh
bash network/test-static-policy.sh
```

Expected: syntax exits 0, 7 wrapper checks pass, 2 namespace checks pass, and the static suite exits
0.

- [ ] **Step 3: Run existing network regression suites**

```bash
sudo -n bash network/test-reconfigure.sh
bash network/test-smartdns-no-cache.sh
bash network/test-debug-capture.sh
```

Expected: all existing checks pass. These commands do not alter the live network;
`test-reconfigure.sh` verifies and uses its own namespace.

- [ ] **Step 4: Run shellcheck on changed shell files**

```bash
shellcheck -S error scripts/overrides/ip network/test-ip-override.sh \
    network/test-tun-underlay.sh network/test-static-policy.sh \
    network/migrate.sh restore.sh
```

Expected: exit 0.

- [ ] **Step 5: Commit documentation**

```bash
git add network/README.md
git commit -m "docs: explain suspend-safe routing ownership"
```

### Task 5: Controlled Live Activation and Read-Only Verification

**Files:**
- No repository changes

- [ ] **Step 1: Record owner and Tailscale state before activation**

```bash
mkdir -p /tmp/suspend-safe-routing-before
ip -4 rule show > /tmp/suspend-safe-routing-before/rules
for table in main 20 230 52 ioa cn; do
    ip -4 route show table "$table" > "/tmp/suspend-safe-routing-before/routes-$table"
done
tailscale debug prefs > /tmp/suspend-safe-routing-before/tailscale-prefs
```

Expected: all commands exit 0. If Tailscale is stopped, stop and ask before activation rather than
starting it.

- [ ] **Step 2: Install the networkd drop-in atomically**

```bash
sudo -n install -d -m 755 /etc/systemd/networkd.conf.d
sudo -n install -o root -g root -m 644 \
    network/systemd-networkd.conf.d/foreign-routing.conf \
    /etc/systemd/networkd.conf.d/foreign-routing.conf
sudo -n networkctl reload
```

Expected: commands exit 0. Do not restart `systemd-networkd`, `tailscaled`, or SmartGateAgent.

- [ ] **Step 3: Replace only the existing IOA default in place**

```bash
sudo -n ip -4 route replace default dev tun0 table ioa metric 101
```

Expected: exit 0. Do not restart SmartGateAgent; its next lifecycle command will pass through the
updated wrapper and recreate the same route shape.

- [ ] **Step 4: Verify the effective live invariants**

```bash
systemd-analyze cat-config systemd/networkd.conf |
    grep -E '^ManageForeign(RoutingPolicyRules|Routes)=no$'
ip -4 route show table ioa default
ip -4 route get 100.12.0.2
ip -4 route get 1.1.1.1 mark 0x80000
tailscale debug prefs > /tmp/suspend-safe-routing-after-prefs
cmp /tmp/suspend-safe-routing-before/tailscale-prefs \
    /tmp/suspend-safe-routing-after-prefs
systemctl is-active systemd-networkd iwd tailscaled smartdns
```

Expected:

- both effective networkd settings print once;
- table `ioa` prints `default dev tun0 metric 101` with no `via`;
- `100.12.0.2` resolves through `tun0`/table `ioa`;
- Tailscale-marked public traffic resolves through the physical `main` route;
- Tailscale preferences are byte-identical;
- all listed services are active.

- [ ] **Step 5: Verify ordinary and IOA connectivity without mutating tunnel preferences**

```bash
curl -fsS --max-time 10 -o /dev/null \
    http://connectivitycheck.gstatic.com/generate_204
curl -fsS --max-time 10 -o /dev/null http://ioa.tencent.com
```

Expected: both commands exit 0. If either fails, collect `network-debug-capture` before making any
additional routing change.

- [ ] **Step 6: Check repository cleanliness**

```bash
git status --short
git --no-pager log -5 --oneline
```

Expected: only the user's pre-existing unrelated changes remain uncommitted; the implementation is
represented by focused commits.
