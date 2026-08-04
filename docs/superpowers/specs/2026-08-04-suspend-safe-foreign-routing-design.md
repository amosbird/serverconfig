# Suspend-Safe Foreign Routing Ownership Design

## Goal

Prevent suspend/resume and physical-link transitions from making Tailscale select SmartGateAgent's
`tun0` as its underlay, while preserving the existing independent ownership of systemd-networkd,
Tailscale, SmartGateAgent, and repository policy.

## Confirmed root causes

Two independent ownership/detection problems occur during a physical-link transition:

1. systemd-networkd defaults `ManageForeignRoutingPolicyRules=` and `ManageForeignRoutes=` to
   `yes`. A networkd reconfiguration can therefore remove policy rules and routes created by
   Tailscale, SmartGateAgent, and `network-reconfigure`. The owners subsequently race to restore
   their objects.
2. When the physical default route temporarily disappears, tailscaled cannot find a default in
   `/proc/net/route` and falls back to a netlink dump of all route tables. Its fallback accepts the
   first default route that has a gateway, without considering its table or effective policy.
   SmartGateAgent's current `default via 192.168.255.1 dev tun0 table ioa` therefore makes `tun0`
   eligible even though only IOA-selected traffic should use that table.

This design does not attribute the wider Tailscale transport failures solely to either problem.
Public Wi-Fi, NAT, or upstream filtering can still fail independently. It removes two deterministic
local failure modes visible in the resume logs.

## Ownership configuration

Install a global networkd drop-in:

```ini
# /etc/systemd/networkd.conf.d/foreign-routing.conf
[Network]
ManageForeignRoutingPolicyRules=no
ManageForeignRoutes=no
```

`ManageForeignRoutingPolicyRules=no` preserves foreign `ip rule` entries, including:

- Tailscale's 52xx rules;
- SmartGateAgent's mark and source rules;
- repository rules at priorities 500, 1000, 1500, 2500, and 3000.

`ManageForeignRoutes=no` preserves foreign routes in tables 20, 230, 52, `ioa`, `cn`, and the
staging table. Networkd continues to own and update the addresses, connected routes, gateway, and
DHCP routes that it creates itself in `main`; the setting only stops garbage collection of routes
owned by other components.

The repository stores the drop-in under `network/systemd-network/` and both migration and GUI
restore deployment install it. Rollback removes the installed drop-in when returning to the old
network stack.

## Making `tun0` ineligible as a Tailscale underlay

Change the exact SmartGateAgent `ip` override from:

```text
default via 192.168.255.1 dev tun0 table ioa metric 101
```

to:

```text
default dev tun0 table ioa metric 101
```

A TUN device can route selected packets directly without a next-hop gateway. The existing policy
remains:

```text
exact mark 0x1 / static IOA business destination
    -> lookup table ioa
    -> default dev tun0
```

Tailscale's Linux netlink fallback explicitly ignores default routes whose gateway attribute is
absent. Consequently this route cannot make tailscaled report `defaultRoute=tun0`. If no physical
underlay exists, no-route/waiting behavior is preferable to nesting Tailscale into IOA.

The wrapper remains deliberately narrow: it rewrites only SmartGateAgent's exact unqualified
`route add default via 192.168.255.1 dev tun0` command. All other commands and their exit statuses
continue to pass through unchanged. The repository does not modify Tailscale preferences or add a
Tailscale recovery daemon.

## Deployment safety

Implementation and functional verification happen in a network namespace first. Production
activation is a separate, explicit step because restarting/reloading networkd or restarting
SmartGateAgent can interrupt live connectivity.

Deployment order:

1. install the networkd drop-in atomically;
2. reload networkd configuration without stopping Tailscale or SmartGateAgent;
3. update the existing live `ioa` default route to its gateway-free form, or restart
   SmartGateAgent only if an in-place owner-compatible replacement is unavailable;
4. verify owner rules/routes, physical default, Tailscale preferences, and IOA reachability.

The deployment must not run `tailscale down`, `tailscale up`, mutate exit-node selection, or create
any fallback to `tun0`. Failure leaves the selected Tailscale exit node fail-closed.

## Error handling

- Installation creates `/etc/systemd/networkd.conf.d` before atomically replacing the drop-in.
- A configuration validation failure leaves the active file unchanged.
- If live activation is not explicitly requested or `sudo -n` is unavailable, implementation stops
  after repository tests and reports the exact manual activation commands.
- Verification checks object ownership independently: preserving rules does not imply routes were
  preserved, and vice versa.
- A missing physical default is reported as an underlay outage; it is never repaired by routing
  Tailscale through `tun0`.

## Tests and checks

Add the smallest runnable checks that cover the two invariants:

1. A namespace test with `default dev tun0 table ioa` and an IOA policy rule proves marked traffic
   still resolves to `tun0`.
2. The same test proves the route has no gateway attribute, matching tailscaled's exclusion
   condition.
3. The `ip` wrapper contract expects the gateway-free rewritten command and confirms unrelated
   commands still pass through.
4. Static deployment tests require both networkd settings, installation through restore/migration,
   and rollback cleanup.
5. Existing namespace and static network suites remain green.

A post-deployment read-only check confirms:

```text
networkd effective config: both ManageForeign settings are no
table ioa: default dev tun0, no via/gateway
main: physical DHCP default remains present
Tailscale and SmartGateAgent owner rules remain present
Tailscale selected exit-node preference is unchanged
```

## Non-goals

- No change to CN route semantics or representation.
- No change to SmartDNS classification or caching.
- No Tailscale fork or generic exclusion of every `tun*` interface.
- No repository ownership of SmartGateAgent tables 20/230 or Tailscale table 52.
- No automatic tunnel stop/start or exit-node detach/re-attach behavior.
