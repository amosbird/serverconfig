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
  the physical default route. The repository installs `ManageForeignRoutingPolicyRules=no` and
  `ManageForeignRoutes=no`, so networkd does not garbage-collect route/rule objects owned by
  Tailscale, SmartGateAgent, or `network-reconfigure`.
- **`scripts/network-reconfigure`** owns the repository's rules at priorities 400, 401, 500, 1000,
  1500, 2500, and 3000; the atomic `cn_direct`/`cn_direct_next` ipsets; the `NETMODE_IOA`
  chain and its OUTPUT hook; the exact CN and IOA MASQUERADE rules; and the generated DHCP and
  office SmartDNS fragments. It owner-marks traffic from both iOA cgroups —
  `ngnclient.service` and the `ioagui.service` user unit — as `0x1000000` before its first route
  lookup and masquerades that traffic on the current physical device, correcting iOA control
  sockets that bind to a Tailscale source address.
- **SmartDNS** owns the domain-derived `ipset ioa` membership.
- **SmartGateAgent** owns `tun0`, mark `0xa38`, tables `20` and `230`, and the contents and
  lifetime of table `ioa`. When table `wired_underlay` advertises the authenticated Tencent USB
  Ethernet route, the `scripts/overrides/ip` wrapper forces SmartGateAgent's unqualified table-20/230
  defaults onto that gateway and device, regardless of `main` route metrics. Without that
  advertisement it uses SmartGateAgent's requested gateway only when it is a physical default in
  `main`. The wrapper gives SmartGateAgent's mark and source rules explicit priorities 1100 and 1200,
  and isolates its unqualified `tun0` default in table `ioa`; every other `ip` operation passes
  through unchanged.
- **Tailscale** owns `tailscale0`, mark `0x80000/0xff0000`, table `52`, DERP and control-plane
  selection, the exit-node preference, and tunnel recovery. No local policy forces DERP or disables
  direct UDP.

Operator tools respect the same boundary. `scripts/network-status` observes repository and
owner state without changing it. `scripts/netfix` may force reconciliation of repository-owned
objects, but it does not change tunnel preferences or owner-managed tables.

## Effective policy

The repository-owned rules use priorities 400, 401, 500, 1000, 1500, 2500, and 3000;
the exact SmartGate command wrapper assigns priorities 1100 and 1200 to its owner-managed rules.

| Priority | Match | Lookup | Purpose |
|---:|---|---|---|
| 400 | `fwmark 0x1000000` | `main` | Force the system IOA underlay packets onto the physical route. |
| 401 | `fwmark 0x1000000` | `prohibit` | Fail closed if no physical route exists; never fall through to Tailscale. |
| 500 | `fwmark 0x80000/0xff0000` | `main` | Let Tailscale-owned transport packets reach the physical network. |
| 1000 | `main` with `suppress_prefixlength 0`, plus current `scope link` routes, physical gateway, captive-portal resolvers, and active office resolvers | `main` | Keep the actual LAN and its required infrastructure direct. |
| 1100 | SmartGateAgent-owned `fwmark 0xa38` | `20` | Send SmartGate control traffic through its current physical underlay. |
| 1200 | SmartGateAgent-owned physical source address | `230` | Keep its source-bound sockets on the current physical underlay. |
| 1500 | exact `fwmark 0x2` | `main` | Send optional CN-accelerated traffic through the current physical default. |
| 2500 | exact `fwmark 0x1`, then `10.0.0.0/8` | `ioa` | Select DNS-classified IOA traffic and retain a literal-address fallback for private Tencent destinations. |
| 3000 | `100.64.0.0/10` | `52` | Reach tailnet peers through Tailscale. |

SmartGateAgent owns the `0xa38 -> table 20` and physical-source `-> table 230` rules. The exact
command wrapper assigns priorities 1100 and 1200, so Tailscale's transport escape at priority 500
always wins. Tables 20 and 230 remain SmartGateAgent-owned. An authenticated Tencent USB Ethernet
route advertised in table `wired_underlay` has absolute preference for those two defaults, even when
Wi-Fi has the lower `main` metric. Otherwise, the wrapper validates SmartGateAgent's requested gateway
against physical defaults in `main`. This policy is based on the registered adapter advertisement,
not on hard-coding `wlan0` or assuming every Ethernet interface is Tencent Ethernet.

Linux evaluates lower numeric priorities first:

1. Every packet created by an IOA cgroup is owner-marked before its first route lookup, uses
   `main`, and is masqueraded on the current physical device. The source rewrite is required
   because iOA can bind control sockets to `tailscale0` after Tailscale starts.
2. Tailscale owner-marked packets use `main`.
3. Any destination that currently has a non-default route in `main` — the connected LAN and the
   physical gateway — uses `main`, as do the explicitly pinned physical gateway, a DHCP resolver
   while an RFC 8910 captive portal is advertised, and office DNS servers enabled for the
   authenticated wired link.
4. SmartGateAgent owner-marked packets use owner table `20`.
5. SmartGateAgent physical-source sockets use owner table `230`.
6. An unmarked destination present in `~/.routefile` receives mark `0x2`, is rerouted through
   the current `main` default, and is masqueraded on that physical device.
7. IOA business payload uses table `ioa`; domain-classified payload is distinct from IOA
   underlay traffic.
8. `100.64.0.0/10` uses Tailscale table `52`.
9. Unmatched traffic follows Tailscale's independently managed selected exit node.

### Routefile is optional acceleration

`scripts/updateroutes` generates `~/.routefile` as a canonical CIDR-per-line data file.
`network-reconfigure` validates it into the inactive `cn_direct_next` hash:net set and swaps that
set with `cn_direct` atomically. `NETMODE_IOA` classifies a matching unmarked destination as exact
mark `0x2` before considering SmartDNS's `ioa` set; priority 1500 reroutes that mark through
`main`. This makes routefile acceleration authoritative over IOA business classification while
the file is healthy, including overlaps with static `10/8` or domain-derived addresses.

The routefile is never required for connectivity. Missing, empty, or comment-only input
atomically disables CN acceleration. Malformed input preserves the last known-good set and
does not fail reconciliation of LAN, IOA, Tailscale, DNS, or firewall policy. A stale set can
only choose direct physical egress for additional destinations; it contains no interface or
gateway and therefore cannot blackhole traffic after an AP change. The retired tables `cn`
and `cn_stage` are flushed, their names are removed from `rt_tables`, and no policy rule
consults them.

`updateroutes` accepts only exact APNIC `CN|ipv4` records whose status is `allocated` or
`assigned`, validates power-of-two counts and network alignment, collapses the result, and rejects
an implausibly small response. It writes and fsyncs a temporary file beside `~/.routefile`, then
uses `os.replace`; an interrupted download, parse failure, short response, or write failure leaves
the previous file byte-for-byte intact. Identical output is a true no-op. The path unit watches
the routefile, so only a successful changed replacement requests reconciliation.

This authority does not take ownership of SmartGateAgent's `0xa38` control traffic. That mark
and its table `20` path remain SmartGateAgent's responsibility.

Only networks currently present as kernel `scope link` routes are considered local. Broad
RFC1918 assumptions are intentionally absent: an unrelated private destination follows the
normal unmatched policy. The physical gateway is an explicit priority-1000 exception because a
LAN can overlap `10/8`. DHCP resolvers receive an exception only while the lease advertises an
RFC 8910 captive portal; without that portal SmartDNS does not use them and sending arbitrary
traffic to their possibly public addresses outside the exit node would violate fail-closed.
While an authenticated wired link has both an address and DHCP gateway, `network-reconfigure`
also extracts the office resolver addresses from `network/smartdns/office.conf` and pins those
exact addresses to `main`; it removes the pins with the office fragment when the link disappears.

Those enumerated pins are a snapshot, so priority 1000 also carries `from all lookup main
suppress_prefixlength 0`. It asks the kernel for `main` without its default route, which is exactly
the set of destinations that are on-link at this instant, and needs no snapshot to stay correct.
This matters because the priority-2500 `10.0.0.0/8` rule and table `ioa` both outlive any single
reconciliation: roaming between two different `10/8` subnets leaves the pins naming the previous
LAN while the new gateway is itself inside `10/8`, so without the suppressed lookup the new gateway
is routed into `tun0` until the next run finishes — observed at up to 21 seconds. The interface
holds an address and a default route while nothing beyond it is reachable, which is
indistinguishable from a dead network. Suppressing prefix length 0 is deliberately not a bypass of
IOA selection: a `10/8` destination that is not on-link still has no route in `main` other than the
suppressed default, so it falls through to priority 2500 and table `ioa` as before.

The registered Tencent USB Ethernet adapter is path-matched by
`network/systemd-network/10-tencent-wired.link`. A hardware-restricted udev rule matches USB identity
`0b95:1790:00000EC65DE788` on every non-remove net event and requests
`wpa_supplicant@enp9s0u2u1u2.service`; handling the rename `move` event preserves the request after
udev changes `eth0` to its persistent name. Other computers and Ethernet adapters do not start
Tencent EAP-TLS. `BindsTo=` stops the supplicant when the USB adapter is removed, and reinsertion
triggers a fresh authentication session. `restore.sh` installs this policy but never enables or
starts a machine-specific instance.

## SmartDNS IOA classification

The IOA upstream is permanently declared in the base SmartDNS configuration as
`server 192.168.255.10 -group ioa -exclude-default-group`. It is not generated from
`tun0` state: link down/up and address-change events neither rewrite an IOA fragment nor restart
SmartDNS. SmartDNS caching is disabled globally and on both listeners: every client query reaches
the selected upstream, expired answers are never served, and no prefetch runs. `rr-ttl-min 0`
disables SmartDNS's built-in 600-second TTL floor so clients receive the upstream TTL unchanged.
When IOA is unavailable, IOA-group names fail closed and may wait for the upstream timeout; they do
not fall back to a public resolver. Ordinary default-group DNS remains independent and continues
through its default and DHCP upstreams.

SmartDNS adds addresses resolved for configured IOA business domains to dynamic `ipset ioa`.
That set is authoritative for domain-derived IOA classification regardless of the answer's prefix.
`NETMODE_IOA` first returns every packet whose mark is non-zero. An unmarked packet whose
destination is in `ioa` receives exact full-width mark `0x1`; the priority-2500 rule routes that mark
through table `ioa`. Marks such as `0xa38`, `0x80000`, or another non-zero value remain untouched.

Broad business suffixes can contain SmartGate's own bootstrap or proxy transport names. SmartDNS
therefore applies more-specific exclusions:

```text
ipset /sgw.woa.com/-
ipset /smartgate.oa.tencent.com/-
ipset /*-smartgate.oa.tencent.com/-
ipset /cloud-smartvpn.oa.tencent.com/-
ipset /http-cloud-smartvpn.oa.tencent.com/-
ipset /ioa.tencent.com/-
```

The `sgw.woa.com` exclusion includes its proxy subdomains. The wildcard and `cloud-smartvpn`
exclusions cover the regional SmartGate discovery names and scene/policy control plane observed in
SmartGateAgent logs. This keeps transport endpoints out of `ioa` without guessing their changing IP
addresses. No static `9/8` or `21/8` IOA route exists:
those prefixes use IOA only when a configured business-domain query adds the exact answer to `ioa`.
`100.12/16` is also domain-derived only: observed services in that range are already classified by
SmartDNS, so the former broad static rule was redundant. `10.0.0.0/8` remains the sole static IOA
fallback for literal-address clients that make no DNS query, subject to the earlier actual-LAN and
routefile rules.

## Tunnel independence and failure semantics

There is no local tunnel-coordination state machine. Tailscale discovers and selects its own
DERP/control paths and recovers its own link. SmartGateAgent establishes `tun0`, maintains its
physical escapes in tables `20` and `230`, and installs or removes the IOA route in table
`ioa`. Repository code does not discover tunnel endpoints or construct alternate transport
paths.

SmartGateAgent's table `ioa` default is `default dev tun0` without a gateway. Marked IOA traffic
still enters the TUN device, while tailscaled's Linux all-table fallback ignores the route because
it has no gateway. During a physical-link outage Tailscale waits for a physical underlay; it must
never select `tun0`.

The selected Tailscale exit node is intentionally **fail-closed**. If the exit node is
unavailable, unmatched/default traffic stops rather than leaking onto the physical network.
Local code must not detach the exit node, bypass it through `main`, call `tailscale down` or
`tailscale up`, or edit the exit-node preference.

One captured outage showed that Tailscale does not always initiate recovery. A roam between
BSSIDs on one subnet flaps the carrier for ~137ms;
`IgnoreCarrierLoss=3s` deliberately keeps the address and both routes, so the roam produces no
netlink change, tailscaled's link monitor sees no interface delta, and it never rebinds. Its
magicsock socket and DERP sessions stay bound to the pre-roam path and stop passing data while
control keeps reporting the exit node as online, so no failover occurs: seven minutes of
continuous `open-conn-track` timeouts followed, spanning two full Wi-Fi reconnects that did
rebind and did regain direct contact. Restarting `tailscaled` restored it.

The wedge is not exclusive to roaming. On 2026-09-14 18:53:52, an operator-triggered iOA GUI
restart killed SmartGateAgent and removed `tun0`; tailscaled did see that and rebound twice, and
the exit-node path wedged anyway — 43 `open-conn-track` timeouts in 100 seconds, `online=yes`,
no roam involved. This second incident was self-inflicted and is not evidence for automatically
repairing every link change. It only proves that the detector must recognize a failed path after
non-roam changes too.

`scripts/network-exit-watchdog` therefore repairs the path instead of routing around it.
Detection is event-driven, not roam-scoped: `network-exit-watchdog.path` runs it on link-state
changes and it does nothing unless the link fingerprint moved — BSSID, the physical default
route, or addresses on the physical device, `tun0`, and `tailscale0`, so a roam or either tunnel
changing qualifies without reacting to unrelated Docker interfaces. After a short settling
delay it first performs a read-only probe. A healthy path is left completely untouched. Only a
failed path causes a magicsock rebind and bounded verification; there is deliberately no standing
timer. If
the path is still dead it restarts `tailscaled` once, unless this watchdog already restarted it
within the last two minutes. Manual and package restarts do not consume that cooldown.
Escalation stays inside Tailscale: if it fails the watchdog logs and
**stays fail-closed**, never installing a bypass and never touching the exit-node preference. It
also refuses to blame the tunnel unless the physical gateway resolves and the kernel confirms
the probe selects `tailscale0`, so an ordinary local-link outage cannot trigger a restart.
`network-status` reports the last verdict.

Other failures stay within ownership boundaries:

- without a physical network, external traffic can fail;
- LAN, routefile, and IOA traffic continue through their explicit higher-priority paths when
  those paths are available;
- if SmartGateAgent removes the route from table `ioa`, its business lookup follows the existing
  later policy; IOA underlay traffic remains physically pinned independently;
- if the physical default disappears, traffic from an IOA cgroup fails with `prohibit`
  instead of using either tunnel; when it exists, owner-marked packets are source-NATed only on
  that physical device;
- a reconciliation failure must not change SmartGateAgent tables `20`, `230`, or `ioa`, or
  Tailscale table `52`, preferences, and recovery state.

## Wi-Fi roaming and MAC identity

The generic `25-wireless.network` applies `IgnoreCarrierLoss=3s` to every WLAN. A carrier gap shorter
than the grace period retains the address, connected route, and physical default route; a longer loss
expires the grace and performs normal DHCP teardown, so a roam to a BSSID on another IP subnet cannot
keep a stale gateway. The value is deliberately finite: `yes` and `infinite` are rejected by the
static checks because either could preserve an old lease indefinitely after leaving a network.

The grace covers two gaps that are not real network changes. iwd roaming between access points
briefly drops carrier, and the kernel deauthenticates (`Reason: 3=DEAUTH_LEAVING`) before entering
S3 suspend. Without the grace, resume produced an observable failure: networkd processed the
carrier loss only after resume, iwd reconnected and DHCP reacquired the same lease within the same
second, and networkd's subsequent `Reconfiguring with 25-wireless.network` deleted the just-installed
default route without reinstalling it. The interface stayed associated with its address, so the
machine looked connected while `main` had no physical default until the Wi-Fi was manually
reconnected. `ManageForeignRoutes=no` does not prevent this: it stops networkd from collecting
routes owned by others, not from tearing down its own DHCP default route.

`network/iwd/main.conf` sets `AddressRandomization=network`, so ordinary SSIDs receive a stable
per-network MAC. The local secret profile `/var/lib/iwd/Tencent-WiFi.8021x` additionally contains
`AddressOverride=1e:dc:46:00:66:1b`, the MAC registered for the Android identity. That profile and its
EAP-TLS secrets are not repository-owned. `Tencent-WiFi` uses the normal wireless DHCP gateway and
physical routing; there is no special no-gateway `.network` file.

The iwd global setting takes effect after the next natural iwd start. Do not restart iwd merely to
apply it during an active remote session. Validate the MAC and routing on the next natural
`Tencent-WiFi` connection, and validate DHCP reconfiguration on the next natural room-to-room roam.

## Reconciliation and AP changes

`network-reconfigure.path` watches networkd link state and `~/.routefile`. On a physical link or DHCP change,
`network-reconfigure` derives a physical identity from the interface, physical device,
gateway, DHCP resolvers, and routefile hash. Tunnel interfaces are not routing-rebuild inputs.
Tailscale and SmartGateAgent receive link changes independently and repair their own state.

The reconciler then:

1. atomically updates the generated DHCP and office SmartDNS fragments when their content changes;
2. derives actual connected-LAN and gateway rules, plus portal-scoped DHCP DNS rules when needed;
3. validates routefile prefixes into an inactive hash:net set and atomically swaps it active;
4. reconciles only the seven active repository-owned priority bands and removes the retired
   priority-1400 mark rule;
5. restores the dual-ipset, mark-0-only firewall policy and exact IOA NAT rule;
6. records the physical state only after a successful run.

### Failure-safe CN classification

The preferred routefile grammar is one IPv4 CIDR per non-comment line. The legacy
`route add|replace IPv4[/prefix] via GATEWAY table cn` representation remains read-only compatible
so an existing file can migrate without an outage; neither form is executed as shell or `ip`
input. Duplicate or invalid prefixes reject the candidate.

The candidate is loaded into `cn_direct_next`, exclusions are added as hash:net `nomatch`
entries, and one `ipset swap` publishes the complete set. Failed parsing or loading never changes
the active set. The saved state records both input hashes and a hash of the actual active entries,
so same-count drift is detected without coupling the classifier to a gateway. Missing or empty
input swaps an empty set active and removes priority 1500.

### Fingerprints and convergence

The saved physical identity is only an early-exit hint. Before skipping work, the reconciler
also compares desired and actual fingerprints for every owned priority band, checks the active
CN set hash, and verifies the complete firewall, ipset, and NAT shape. Comparison is
bidirectional: missing owned objects and unexpected objects inside an owned band both trigger
repair. Desired rules are added before stale rules are removed, and the final band must match
exactly.

This retains three important invariants across AP changes: captive-portal DNS remains reachable
without creating a permanent public bypass, CN acceleration immediately follows the current
`main` default without rebuilding, and tunnel-owned tables and marks remain untouched.

## Manual incident capture

There is no constant packet recorder. Run the manual command when an incident is occurring to
collect before/after state and parallel 30-second captures on `wlan0` and `tailscale0`:

```bash
scripts/network-debug-capture "optional incident note"
```

The command uses non-interactive `sudo -n`, writes a root-only incident under
`/var/log/network-debug/incidents`, and retains the newest five incidents. It never changes routes,
firewall rules, tunnel preferences, or network services. `tailscale netcheck` does perform active
connectivity probes while collecting diagnostics. Individual diagnostic failures and timeout return
codes are recorded in `manifest.tsv` instead of aborting collection. Text command output is capped at
1 MiB per command and marked `truncated=true`; each deep pcap is capped at 8 MB. The route-event
summary is bounded separately: it keeps counts and first/last events, then retains the latest matching
events in a 900 KB byte ring (at most 5,000, with each line capped at 512 bytes), so it remains below
1 MiB without discarding the newest timeline tail. The snapshots include TCP/UDP sockets, TCP/UDP
conntrack state, kernel network counters, interface statistics, bounded tailscaled goroutines, and this
route-event timeline. Do not put passwords, tokens, or other secrets in the incident note or command
line.

`--bugreport` additionally runs `tailscale bugreport --diagnose`. This can upload diagnostic logs
to Tailscale and return a shareable identifier, so it is never run by default:

```bash
scripts/network-debug-capture --bugreport "incident note"
```

## Testing and diagnostics

Run the network checks:

```bash
bash network/test-ip-override.sh
bash network/test-smartgate-underlay.sh
bash network/test-ioa-fail-closed.sh
bash network/test-install-ioa-62.sh
sudo -n bash network/test-ioa-static-lan-overlap.sh
sudo -n bash network/test-reconfigure.sh
bash network/test-exit-watchdog.sh
bash network/test-static-policy.sh
bash network/test-debug-capture.sh
python3 network/test-updateroutes.py
```

`network/test-reconfigure.sh` runs destructive routing, firewall, roaming, cold-start, and
mutation checks inside an isolated network namespace. Its mutation checks ensure assertions
actually fail when reconciliation safeguards are removed. The override and static-policy
checks use local fixtures and command contracts.

Live inspection is read-only:

```bash
scripts/network-status
```

It reports interfaces, connected routes, repository priority bands, optional CN-set and IOA state,
SmartGateAgent's owner mark, Tailscale table `52`, and exit-node health without repairing or
mutating them.

## Deployment

`restore.sh` is the only repository deployment entry point. With `GUI=1`, it installs the iwd,
networkd, udev, and systemd configuration, removes obsolete installed repository files, validates
SmartDNS before replacement, and reloads configuration without taking ownership of tunnel recovery.
`restore.sh` also disables any stale `netctl@*.service` boot links without stopping the active
connection, so an old interface-specific profile cannot race iwd after the next reboot. It does
not copy ordinary Wi-Fi credentials: iwd profiles under `/var/lib/iwd` remain local secret state.
There is no supported return to the retired pre-iwd network stack.

For a focused network-only update, copy the changed repository files to their matching `/etc` paths,
remove obsolete installed files explicitly, and use `networkctl reload`. A changed `.network` causes
networkd to reconfigure matching links; it should retain or immediately reacquire the current lease, and
the new carrier-loss behavior applies to the next roam. Do not restart iwd, systemd-networkd,
Tailscale, SmartGateAgent, or SmartDNS merely to apply the Wi-Fi roaming policy. Installed pre-iwd
files under `/etc` are intentionally left for a separate inventoried cleanup so the wired 802.1X
credential path is not removed accidentally.
