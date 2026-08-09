# China DNS Physical Underlay Design

## Goal

Ensure SmartDNS's fixed China upstream resolvers use the current physical network, so IOA bootstrap
names are resolved from the local China network rather than through a foreign Tailscale exit node.

## Root cause

The priority-1000 physical-network policy already includes the connected LAN, physical gateway, and
DHCP-provided resolvers. It does not include the fixed SmartDNS `china` group upstreams
`114.114.114.114` and `223.5.5.5`. These public resolver addresses therefore follow Tailscale's
independently managed default route.

With the selected exit node in Warsaw, IOA bootstrap queries returned overseas endpoints. IOA's own
mark `0xa38` correctly sent the resulting connections through physical table 20, but that meant a
China physical connection was dialing distant endpoints. Measured TLS requests took roughly
0.7–3.7 seconds against those endpoints versus 0.1–0.2 seconds against answers from the local DHCP
resolver.

## Design

Add a repository-owned list matching SmartDNS's fixed China upstreams:

```bash
CHINA_DNS_SERVERS=(114.114.114.114 223.5.5.5)
```

Include these addresses in the existing priority-1000 `lookup main` band alongside the gateway and
DHCP resolvers. This changes only transport routing for DNS requests sent to the fixed China
upstreams. It does not bypass the Tailscale exit node for general traffic and does not alter IOA or
Tailscale owner marks, tables, processes, or recovery.

The addresses remain fixed because they are already explicit repository configuration, not learned
runtime endpoints. A static test keeps the SmartDNS declarations and routing list synchronized so a
resolver cannot be added to one side without the other.

## Data flow

After reconciliation:

```text
SmartDNS query to 114.114.114.114 or 223.5.5.5
  -> priority 1000 lookup main
  -> current physical gateway
  -> China-local DNS answer for IOA bootstrap name
  -> SmartGateAgent mark 0xa38 / table 20 dials that local endpoint physically
```

DHCP DNS remains pinned as before. Ordinary default-group DNS and unmatched application traffic keep
the existing Tailscale exit-node behavior.

## Failure handling and ownership

If no physical gateway exists, these DNS requests fail rather than escaping through another route.
That is consistent with their role as physical underlay discovery. The repository does not restart
SmartGateAgent or change its connections. Existing IOA connections retain their current endpoints
until SmartGateAgent naturally reconnects or the operator restarts it manually.

## Testing and deployment

Namespace tests first verify that both fixed China resolvers resolve through table `main`, including
a cold start and an AP/gateway change, while tunnel-owner state remains byte-identical. A mutation
check removes the new policy input and proves the assertion fails. Static checks verify exact set
equality between SmartDNS's `-group china` server declarations and the routing list.

Deployment runs the existing `network-reconfigure` once. It changes only repository-owned policy
priority 1000. No network daemon, tunnel, SmartDNS, or Tailscale process is restarted. Read-only live
verification checks `ip route get` for both resolvers and compares bootstrap DNS answers and latency.
