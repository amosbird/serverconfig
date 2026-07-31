# Network Stack

This directory defines local policy routing around two independent tunnels: SmartGateAgent
(IOA) and Tailscale. The repository selects traffic for those tunnels, but it does not own
their bootstrap, health, or recovery.

The intended outcomes are:

- the current LAN and its infrastructure use the physical network;
- destinations in `~/.routefile` use the physical network;
- Tencent intranet traffic selected by static ranges or SmartDNS uses IOA;
- all unmatched traffic uses the selected Tailscale exit node.

## Components and ownership

Ownership is deliberately narrow:

- **systemd-networkd and the kernel** own `main`, interface addresses, connected routes, and
  the physical default route.
- **`scripts/network-reconfigure`** owns the repository's rules at priorities 500, 1000,
  1500, 2500, and 3000; table `cn`; the dynamically registered `cn_stage` table;
  `ipset ioa_intranet`; the `NETMODE_IOA` chain and its OUTPUT hook; the exact IOA
  MASQUERADE rule; and generated SmartDNS fragments.
- **SmartDNS** owns the domain-derived `ipset ioa` membership.
- **SmartGateAgent** owns `tun0`, mark `0xa38`, tables `20` and `230`, and the contents and
  lifetime of table `ioa`. The `scripts/overrides/ip` wrapper only rewrites its unqualified
  `tun0` default into table `ioa`; every other `ip` operation passes through unchanged.
- **Tailscale** owns `tailscale0`, mark `0x80000/0xff0000`, table `52`, DERP and control-plane
  selection, the exit-node preference, and tunnel recovery.

Operator tools respect the same boundary. `scripts/network-status` observes repository and
owner state without changing it. `scripts/netfix` may force reconciliation of repository-owned
objects, but it does not change tunnel preferences or owner-managed tables.

## Effective policy

Linux evaluates lower numeric priorities first. The repository-owned rules are:

| Priority | Match | Lookup | Purpose |
|---:|---|---|---|
| 500 | `fwmark 0x80000/0xff0000` | `main` | Let Tailscale-owned transport packets reach the physical network. |
| 1000 | current `scope link` routes, physical gateway, and DHCP resolvers | `main` | Keep the actual LAN and its infrastructure direct. |
| 1500 | all destinations with a route in `cn` | `cn` | Make `~/.routefile` authoritative for physical egress. |
| 2500 | exact mark `0x1`, `10.0.0.0/8`, and `100.12.0.0/16` | `ioa` | Select IOA business traffic. |
| 3000 | `100.64.0.0/10` | `52` | Reach tailnet peers through Tailscale. |

SmartGateAgent chooses the priority of its own `0xa38 -> table 20` rule. The repository does
not move, duplicate, repair, or delete that rule. Tailscale's remaining rules, including the
selected exit-node default, follow the repository-owned bands.

The resulting order is:

1. Tailscale owner-marked packets use `main`.
2. Actual connected LAN destinations, the physical gateway, and DHCP resolvers use `main`.
3. A destination present in `~/.routefile` uses `cn` and the physical gateway.
4. SmartGateAgent owner-marked packets follow its table `20` rule.
5. Exact mark `0x1`, `10.0.0.0/8`, and `100.12.0.0/16` use table `ioa`.
6. `100.64.0.0/10` uses Tailscale table `52`.
7. Unmatched traffic follows Tailscale's selected exit node.

### Routefile is authoritative

`scripts/updateroutes` generates `~/.routefile`; `network-reconfigure` loads those prefixes
into table `cn` with the current physical gateway. Priority 1500 is before every
repository-owned IOA business rule, so routefile destinations have absolute priority over IOA
business selection. This remains true when a routefile prefix overlaps static `10/8` or
`100.12/16`, and when a packet already has the exact SmartDNS business mark `0x1`.

This authority does not take ownership of SmartGateAgent's `0xa38` control traffic. That mark
and its table `20` path remain SmartGateAgent's responsibility.

Only networks currently present as kernel `scope link` routes are considered local. Broad
RFC1918 assumptions are intentionally absent: an unrelated private destination follows the
normal unmatched policy. The physical gateway and DHCP resolvers are explicit priority-1000
exceptions because a LAN can overlap `10/8` and a DHCP resolver can be a public address.

## SmartDNS IOA classification

SmartDNS adds addresses resolved for configured IOA domains to dynamic `ipset ioa`.
`network-reconfigure` maintains a second set, `ioa_intranet`, containing:

- `9.0.0.0/8`;
- `10.0.0.0/8`;
- `100.12.0.0/16`.

`NETMODE_IOA` first returns every packet whose mark is non-zero. A packet with mark `0` is set
to exact full-width mark `0x1` only when its destination belongs to **both** `ioa` and
`ioa_intranet`. The priority-2500 rule also matches `0x1` exactly, so marks such as `0xa38`,
`0x80000`, or another value whose low bit is set cannot be captured as IOA business traffic.

`9.0.0.0/8` is never a static IOA route. It is only a safety boundary for dynamic,
domain-derived SmartDNS classification. By contrast, `10.0.0.0/8` and `100.12.0.0/16` are
static IOA business ranges, subject to the earlier actual-LAN and routefile rules.

## Tunnel independence and failure semantics

There is no local tunnel-coordination state machine. Tailscale discovers and selects its own
DERP/control paths and recovers its own link. SmartGateAgent establishes `tun0`, maintains its
physical escapes in tables `20` and `230`, and installs or removes the IOA route in table
`ioa`. Repository code does not discover tunnel endpoints or construct alternate transport
paths.

The selected Tailscale exit node is intentionally **fail-closed**. If the exit node is
unavailable, unmatched/default traffic may stop until Tailscale recovers. Local code must not
detach the exit node, bypass it through `main`, call `tailscale down` or `tailscale up`, or
restart `tailscaled` as recovery. This prevents an outage from silently leaking default
traffic onto the physical network.

Other failures stay within ownership boundaries:

- without a physical network, external traffic can fail;
- LAN, routefile, and IOA traffic continue through their explicit higher-priority paths when
  those paths are available;
- if SmartGateAgent removes the route from table `ioa`, an IOA lookup misses and continues to
  later policy; the reconciler does not infer IOA liveness;
- a reconciliation failure must not change SmartGateAgent tables `20`, `230`, or `ioa`, or
  Tailscale table `52`, preferences, and recovery state.

## Reconciliation and AP changes

`network-reconfigure.path` watches networkd link state. On a physical link or DHCP change,
`network-reconfigure` derives a physical identity from the interface, physical device,
gateway, DHCP resolvers, and routefile hash. Tunnel interfaces are not routing-rebuild inputs.
Tailscale and SmartGateAgent receive link changes independently and repair their own state.

The reconciler then:

1. atomically updates generated SmartDNS fragments when their content changes;
2. derives actual connected-LAN, gateway, and DHCP DNS rules;
3. stages routefile routes, validates them, and updates `cn` for the current gateway;
4. reconciles only the five repository-owned priority bands;
5. restores the dual-ipset, mark-0-only firewall policy and exact IOA NAT rule;
6. records the physical state only after a successful run.

### Failure-safe route staging

`cn_stage` is assigned a free ID in the private range 10000-10999 after checking all existing
`rt_tables` registrations. An existing unique, non-conflicting ID is reused. Exhaustion or a
conflict fails visibly rather than borrowing another table's ID.

A non-empty routefile is first rewritten for the current gateway and loaded into `cn_stage`.
The route count must match before the active `cn` table is changed. New active routes are
installed before stale routes are deleted, so malformed input or a netlink failure preserves
the last valid `cn` policy and normal reconciliation avoids an empty-routing window. A missing
or empty routefile is authoritative and safely disables the `cn` rule and routes.

### Fingerprints and convergence

The saved physical identity is only an early-exit hint. Before skipping work, the reconciler
also compares desired and actual fingerprints for every owned priority band, checks the active
`cn` gateway, and verifies the complete firewall, ipset, and NAT shape. Comparison is
bidirectional: missing owned objects and unexpected objects inside an owned band both trigger
repair. Desired rules are added before stale rules are removed, and the final band must match
exactly.

This retains three important invariants across AP changes: public DHCP DNS remains reachable,
`cn` routes use the current gateway, and tunnel-owned tables and marks remain untouched.

## Testing and diagnostics

Run the three repository checks:

```bash
bash network/test-ip-override.sh
sudo -n bash network/test-reconfigure.sh
bash network/test-static-policy.sh
```

`network/test-reconfigure.sh` runs destructive routing, firewall, roaming, cold-start, and
mutation checks inside an isolated network namespace. Its mutation checks ensure assertions
actually fail when reconciliation safeguards are removed. The override and static-policy
checks use local fixtures and command contracts.

Live inspection is read-only:

```bash
scripts/network-status
```

It reports interfaces, connected routes, repository priority bands, `cn` and `ioa` state,
SmartGateAgent's owner mark, Tailscale table `52`, and exit-node health without repairing or
mutating them.

## Migration and rollback

Migration retires obsolete repository artifacts only after the replacement policy is active.
It must not stop SmartDNS, SmartGateAgent, or Tailscale, and it must not mutate either tunnel's
preferences or owner-managed routes.

Rollback/removal is ownership-based rather than priority-wide: it deletes only exact
repository-owned rule shapes at priorities 500, 1000, 1500, 2500, and 3000, table `cn`, the
dynamically registered `cn_stage` table, `NETMODE_IOA`, `ioa_intranet`, generated SmartDNS
fragments, and repository-owned legacy mark-`0x1` NAT state. Unrelated rules sharing those
priorities survive. SmartGateAgent tables `20`, `230`, and `ioa` and Tailscale table `52` and
exit-node preferences are never flushed or changed by rollback.
