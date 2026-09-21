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
  1150, 1500, 1501, and 3000; the atomic `cn_direct`/`cn_direct_next` ipsets; the `NETMODE_IOA`
  chain and its OUTPUT hook; the exact CN and IOA MASQUERADE rules; the post-srcnat source guard;
  and the generated DHCP and office SmartDNS fragments. It owner-marks traffic from both iOA cgroups —
  `ngnclient.service` and the `ioagui.service` user unit — as `0x1000000` before its first route
  lookup and masquerades that traffic on the selected underlay device, correcting iOA control
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

The `/usr/lib/iOA/bin/iOA` entrypoint has two callers with different authority. The root-owned
`ngnclient.service` waits for a physical default, reconciles policy, and then starts `iOA.bin`.
`iOALinux` also invokes the same entrypoint as the desktop user when it cannot see the daemon.
That path only checks whether `ngnclient.service` is active; it never attempts reconciliation or
starts a second daemon. `ioagui.service` has the same condition, so starting the GUI while the daemon
is absent is skipped rather than entering its restart loop. This boundary was added after the GUI
repeatedly invoked the root-only reconciler as UID 1000, failed on its first write to `/etc`, and
produced misleading paired line-191 errors every 21 seconds.

## Effective policy

The repository-owned rules use priorities 400, 401, 500, 1000, 1150, 1500, 1501, and 3000;
the exact SmartGate command wrapper assigns priorities 1100 and 1200 to its owner-managed rules.

| Priority | Match | Lookup | Purpose |
|---:|---|---|---|
| 400 | `fwmark 0x1000000` | `wired_underlay` while office-authorized, otherwise `main` | Prefer the authenticated office wire for iOA control traffic and fall back to the ordinary physical route everywhere else. |
| 401 | `fwmark 0x1000000` | `prohibit` | Fail closed if no physical route exists; never fall through to Tailscale. |
| 500 | `fwmark 0x80000/0xff0000` | `main` | Let Tailscale-owned transport packets reach the physical network. |
| 1000 | `main` with `suppress_prefixlength 0`, plus current `scope link` routes, physical gateway, captive-portal resolvers, and the office resolvers from the wired lease | `main` | Keep the actual LAN and its required infrastructure direct. |
| 1100 | SmartGateAgent-owned `fwmark 0xa38` | `20` | Send SmartGate control traffic through its current physical underlay. |
| 1150 | exact `fwmark 0x1`, then unmarked `10.0.0.0/8` | `ioa` | Select DNS-classified IOA payload and retain a literal-address fallback for private Tencent destinations. |
| 1200 | SmartGateAgent-owned physical source address | `230` | Keep its source-bound sockets on the current physical underlay. |
| 1500 | exact `fwmark 0x2` | `main` | Send optional CN-accelerated traffic through the current physical default. |
| 1501 | exact `fwmark 0x2` | `prohibit` | Fail closed if main has no route; never fall through to Tailscale. |
| 3000 | `100.64.0.0/10` | `52` | Reach tailnet peers through Tailscale. |

SmartGateAgent owns the `0xa38 -> table 20` and physical-source `-> table 230` rules. The exact
command wrapper assigns priorities 1100 and 1200, so Tailscale's transport escape at priority 500
always wins. Tables 20 and 230 remain SmartGateAgent-owned. An authenticated Tencent USB Ethernet
route advertised in table `wired_underlay` has absolute preference for those two defaults, even when
Wi-Fi has the lower `main` metric. Otherwise, the wrapper validates SmartGateAgent's requested gateway
against physical defaults in `main`. This policy is based on the underlay advertisement made for an
802.1X-authorized link, not on hard-coding `wlan0` or assuming every Ethernet interface is Tencent
Ethernet.

Linux evaluates lower numeric priorities first:

1. Every packet created by an IOA cgroup is owner-marked before its first route lookup. It uses
   `wired_underlay` and is masqueraded on the authenticated wire in the office; otherwise it uses
   `main` and the current physical device. The source rewrite is required because iOA can bind
   control sockets to `tailscale0` after Tailscale starts.
2. Tailscale owner-marked packets use `main`.
3. Any destination that currently has a non-default route in `main` — the connected LAN and the
   physical gateway — uses `main`, as do the explicitly pinned physical gateway, a DHCP resolver
   while an RFC 8910 captive portal is advertised, and office DNS servers enabled for the
   authenticated wired link.
4. SmartGateAgent owner-marked packets use owner table `20`.
5. IOA business payload uses table `ioa`; the tunnel's own underlay uses the authenticated office
   wire through tables 20 and 230. This precedes SmartGateAgent's source rule, for the reason below.
6. SmartGateAgent physical-source sockets use owner table `230`.
7. An unmarked destination present in `~/.routefile` receives mark `0x2`, is rerouted through
   the current `main` default, and is masqueraded on that physical device. If that lookup finds
   no route, priority 1501 prohibits the packet instead of falling through to Tailscale.
8. `100.64.0.0/10` uses Tailscale table `52`.
9. Unmatched traffic follows Tailscale's independently managed selected exit node.

### The IOA band has to outrank SmartGateAgent's source rule

SmartGateAgent installs `from <physical address> lookup 230` at priority 1200, and table 230 is a
default through the physical gateway. That matches on the source address, which is the part that makes
it dangerous: an unbound socket gets its source assigned during the routing decision, and that
decision happens *before* OUTPUT mangle applies any mark. The mark-triggered reroute reuses the source
already chosen, so rule 1200 matches every locally originated packet regardless of its mark.

The IOA band therefore has to sit above it. It did not until 2026-09-18, when it lived at priority
2500, and the result was that iOA appeared to log in while no internal host was reachable. The failure
is completely silent: `ipset` classification is correct, the mangle rule's counters increment, and
`ip route get <dst> mark 0x1` reports `dev tun0` — because that form supplies no source and so never
consults rule 1200. Adding the real source is what shows it:

```console
$ ip -4 route get 100.12.0.10 mark 0x1
100.12.0.10 dev tun0 table ioa src 192.168.255.10
$ ip -4 route get 100.12.0.10 mark 0x1 from 10.36.49.154
100.12.0.10 from 10.36.49.154 via 10.36.48.1 dev wlan0 table ioa_src
```

`tun0` had carried zero packets in either direction since it came up, and `tcpdump -i tun0` saw
nothing while internal hosts timed out. Priority 1150 fixes it: above 1200, below 1100 so
SmartGateAgent's own `0xa38` band keeps precedence, and below 1000 so the on-link protections stay in
front. `network/test-static-policy.sh` fails if `P_IOA` stops preceding `P_IOA_SMART_SRC`.

Note that `reconcile_ioa_smartgate` deliberately leaves priorities 1100 and 1200 alone while
`ngnclient` is active and table 230's default matches the current underlay. That is still right —
iOA reinstalls them on every address change, so removing them is a race that cannot be won. Winning
on priority instead of on ownership is what makes this stable.

Priority 2500 was not a mistake when it was chosen on 2026-07-28; the invariant it relied on was
removed on 2026-09-05 and never rechecked. Until then the wrapper did not intercept `ip rule` at all,
so iOA's `rule add from <addr> table 230` carried no priority and the kernel assigned one through
`fib_default_rule_pref`, which returns the *second* existing rule's priority minus one. That makes the
result depend on load order, which is measurable in a namespace:

```console
# iOA adds its rule while only the default rules exist
32765:  from 10.36.49.154 lookup ioa_src
# iOA adds its rule after this repository's bands are installed
399:    from 10.36.49.154 lookup ioa_src
```

In the usual boot order iOA got 32765, below 2500, and the IOA band worked. The other branch is a
whole-machine outage, since 399 preempts even the owner-mark escape at 400, and eliminating it is why
`fix: isolate IOA underlay from Tailscale` pinned the rule to 1200. Pinning also converted "usually
below 2500" into "always above 2500", which is the silent failure above. The lesson is not about the
number: giving a foreign rule a fixed priority changes every ordering relationship it previously had
by accident, so each of those relationships has to be restated deliberately.

Two adjacencies are worth stating for the same reason. The literal `10.0.0.0/8` entry at 1150
explicitly matches full-width mark zero. A routefile destination receives mark `0x2` before the
reroute and therefore passes it to the CN band at 1500; SmartGate's `0xa38` has already used table
20 at 1100. Every iOA process — `iOA.bin` and `SmartGateAgent` in
`system.slice/ngnclient.service`, `iOALinux` in `ioagui.service` — receives the owner mark, so its
transport leaves at 400 and fails closed at 401 without ever reaching either business band. That is
what keeps the transport from looping into its own tunnel while still allowing the whole cgroup to
prefer the office wire.

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
While an 802.1X-authorized wired link has both an address and a DHCP gateway, `network-reconfigure`
takes the office resolver addresses from that link's DHCP lease, pins those exact addresses to
`main`, and gives each one a host route through the wired gateway; it removes all of that with the
office fragment when the link disappears. Both halves are necessary. The addresses come from the
lease rather than a checked-in list because each office advertises its own resolvers — this site
hands out `21.7.193.132`, `21.7.193.156` and `10.76.9.15`, none of which were among the three
addresses the repository hardcoded until 2026-09-20, so the pins named resolvers that do not exist
here. The host routes exist because the office link deliberately contributes no default route to
`main`, so a pin to `main` had nothing to resolve the address through: all three timed out while the
same queries answered in milliseconds once routed through the wired gateway.

Those enumerated pins are a snapshot, so priority 1000 also carries `from all lookup main
suppress_prefixlength 0`. It asks the kernel for `main` without its default route, which is exactly
the set of destinations that are on-link at this instant, and needs no snapshot to stay correct.
This matters because the priority-1150 `10.0.0.0/8` rule and table `ioa` both outlive any single
reconciliation: roaming between two different `10/8` subnets leaves the pins naming the previous
LAN while the new gateway is itself inside `10/8`, so without the suppressed lookup the new gateway
is routed into `tun0` until the next run finishes — observed at up to 21 seconds. The interface
holds an address and a default route while nothing beyond it is reachable, which is
indistinguishable from a dead network. Suppressing prefix length 0 is deliberately not a bypass of
IOA selection: a `10/8` destination that is not on-link still has no route in `main` other than the
suppressed default, so it falls through to priority 1150 and table `ioa` as before.

### Which Ethernet link is the office LAN

The office LAN is the link that the office LAN authenticated. `office_wired_authorized` asks the
802.1X supplicant, and the office SmartDNS fragment, the resolver pins and the table 19
advertisement all hang off that one answer.

Two weaker judgements were tried on 2026-09-20 and both were wrong on the same axis. Matching any
Ethernet name hands all three to a phone tethered over USB, mapping intranet domains at the phone's
resolver and pointing SmartGateAgent's underlay at the phone's gateway. Matching the registered
adapter's hardware address identifies the dongle rather than the network: the same dongle in a hotel
port claims to be the office LAN, and any other card in a real office port is missed.

Rejecting the adapter as a way of classifying the network says nothing about whether that adapter's
registered address matters, and conflating the two is what broke authentication — see the registered
wired identity below.

Authorization is also the honest reading of a port that hands out a lease and then refuses to
authorize us. This one sat at `HELD`/`suppPortStatus=Unauthorized` after EAP-TLS failure: no
internet, and its resolvers answered some intranet names and not others. Declining to call that the
office LAN is what keeps iOA's bootstrap on the public endpoint it can actually reach. It has one
visible cost — while such a port is up and iOA's tunnel is not, intranet names time out in the `ioa`
group instead of being answered by resolvers that happen to work. That group is
`-exclude-default-group` on purpose, so there is no public fallback, and the state resolves itself
when the tunnel comes up.

A link with no supplicant at all — a tether, a hotel port — fails the same check for the same reason.

### Which wired gateway deserves the default route

`network/systemd-network/20-wired.network` matches every Ethernet link and takes the address and
nothing else. networkd cannot tell an office port from a tether, because what distinguishes them is
what the network does and `[Match]` cannot express that.

So `reconcile_wired_default_route` decides, and it installs nothing until one TCP handshake through
the offered gateway has crossed to the public anchor the debug captures already use. That direction
of fail-closed is the one that matters: the office gateway reaches the intranet and not the internet,
and a default route from it at metric 100 outranks Wi-Fi and blackholes the host, whereas a tether
that does reach the internet gets its route a moment after the lease instead of never. Accepting the
gateway from the lease is what made USB tethering look dead on 2026-09-18 — the address arrived and
nothing else did.

The probe needs a route to the anchor before any default route exists, which is what its temporary
host route is for; it is withdrawn on both outcomes. Exit 2 from `network-probe-tcp` means the probe
never reached the path and measured nothing, and is not read as success — installing a default route
on that would be installing one on no evidence. The route is removed by device rather than by
gateway, because a link that moved to another network leaves a default route naming the old gateway,
which is exactly the blackhole this exists to avoid.

### The registered wired identity

`network/systemd-network/10-tencent-wired.link` presents the registered address
`08:3a:88:5a:b5:37` on the office adapter. Tencent's NAC has that address on file for this adapter,
and its EAP-TLS authorization depends on it: a cryptographically valid session is still refused
without it. This is device identity and decides nothing about which network a link is on, which is
the separate question the section above answers.

The match is the adapter's `PermanentMACAddress=00:0e:c6:5d:e7:88`, not the USB port it occupies.
Until 2026-09-20 it was `Path=pci-0000:00:14.0-usb-0:1:1.0`; the adapter moved to `usb-0:6:1.0`, the
override silently stopped applying, and EAP-TLS failed from then on. The journal has both states,
readable from the link-local address each MAC produces:

| Date | Link-local | Result | Lease |
|---|---|---|---|
| 2026-08-24 | `fe80::a3a:88ff:fe5a:b537` (registered) | `EAP-SUCCESS` | `10.76.165.31/24` office |
| 2026-09-20 | `fe80::20e:c6ff:fe5d:e788` (hardware) | `EAP-FAILURE` | `10.76.76.210/26` quarantine |

Restoring the override on 2026-08-10 flipped the same machine from the quarantine lease to the office
one within a minute, which is the causal half of the same evidence.

The file was deleted on 2026-09-20 on the grounds that the override was not applying, which was the
wrong reading: an override that is not applying is one to repair. Deleting it did not cause that
day's failures — the port move had already disabled it, and the first failure at 14:41 predates the
deletion — but it would have made the regression permanent. Matching the permanent address cannot
fail the same way, because it survives moving the adapter.

`NamePolicy=` and `AlternativeNamesPolicy=` are copied verbatim from `99-default.link`. The first
matching `.link` replaces that file outright for the device, so omitting them would hand naming to a
different policy, and `keep` is what preserves the `enp9s0u2u1u2` name that the udev rule below and
the `wpa_supplicant@` instance are both written against. That name comes from
`/etc/udev/rules.d/70-persistent-net.rules`, which is not owned by this repository and lists both the
hardware and the registered address, so the name holds whether or not the override has been applied.

A hardware-restricted udev rule matches USB identity `0b95:1790:00000EC65DE788` on every non-remove
net event and requests `wpa_supplicant@enp9s0u2u1u2.service`; handling the rename `move` event
preserves the request after udev changes `eth0` to its persistent name. Other computers and Ethernet
adapters do not start Tencent EAP-TLS. `BindsTo=` stops the supplicant when the USB adapter is
removed, and reinsertion triggers a fresh authentication session. `restore.sh` installs this policy
but never enables or starts a machine-specific instance.

### iOA bootstrap follows office authorization

The base SmartDNS configuration maps `smartgate.oa.tencent.com`, `sgw.woa.com` and
`ioa.tencent.com` to the public `china` group. Away from the office, priority 400 sends the owner
mark to `main`, so iOA can bootstrap over any ordinary Wi-Fi, tether, or hotel Ethernet without
entering Tailscale.

The office fragment overrides those same names to the lease-derived `office` resolver group only while
the wired supplicant reports `Authorized` and the link has an address and DHCP gateway. The same
condition installs table 19, changes priority 400 to `wired_underlay`, and puts owner MASQUERADE on
that wired device. The DNS answer and its control-plane route therefore move together. On this site
`freeconnect.ioa.tencent.com = 10.88.202.158`; a marked TCP
probe from `10.76.165.30` through `enp9s0u2u1u2` reaches its port 443.

This coupling fixes the two half-configured states observed on 2026-09-20. Internal DNS with owner
routing still on Wi-Fi produced unreachable SYNs forever. Public DNS after wired authorization let
iOA log in but forced its network-location check to the public endpoint, so it remained `outer net`
while office DNS and table 19 were otherwise active. Authorization loss removes the whole fragment,
flushes table 19, and moves owner routing/NAT back to `main`; business payload remains on table
`ioa` throughout. A phone tether or foreign Ethernet has no authorized
supplicant, so it can never enable any office half.

The transition order is asymmetric by design. Entering office installs table 19, stages wired owner
NAT alongside the old NAT, switches priority 400, prunes the old NAT, and only then publishes office
DNS. Leaving publishes public DNS first, stages the new `main` NAT, switches priority 400, prunes the
wired NAT, and only then flushes table 19. Thus neither direction has a zero-NAT window or an internal
bootstrap answer routed onto Wi-Fi; priority 401 remains a crash backstop rather than a normal
transition step.

The root iOA daemon caches its outer/inner mode at startup. It does open new connections to a changed
DNS answer, but that alone did not clear its once-per-minute `Set outer net timeout` state.
`network-reconfigure` therefore records `office` or `external` in `/run` after publishing the
matching routes, NAT, and DNS, then asynchronously `try-restart`s `ngnclient` only when that value
changes. An invocation running inside `ngnclient.service` records the state but never requests a
restart, so service startup cannot loop. Repeated link events in the same environment do nothing.

The `office` DNS group and `ipset ioa` deliberately have different names and jobs. `office` contains
only resolvers from the authorized wired lease, so those queries cannot race the tunnel resolver.
The `ioa` ipset classifies the resulting business addresses for priority-1150 tunnel routing. Reusing
the tunnel's `ioa` resolver group for office DNS would make “wired preferred” nondeterministic.

### The tunnel resolver exists only off the office LAN

`192.168.255.10` is `tun0`'s own address, and the DNS server answering there is iOA's own: its access
log records SmartDNS querying `192.168.255.10:53` and SmartGateAgent forwarding to intranet resolvers
such as `10.221.106.95:53` or, for public names, `114.114.114.114:53`.

That server is conditional. SmartGateAgent fetches a scene from SmartGate and starts its DNS only for
scenes 2 (`EXTRA`) and 3 (`OVERSEA`). On the office LAN the answer is `sceneID:1, sceneName:INTRA`,
and every network change logs `[DNS] start dns server fail : scene 1 is not supported` followed by
`toggleDNS to start dnsFail to start Local DNS`. The local switch is on (`Local DNS enable : true`);
the scene gate is what refuses. Archived agent logs match the laptop's travel exactly: `OVERSEA` and
`EXTRA` throughout 2026-09-16..19 away from the office with the DNS server running, both scenes on
2026-09-20 in transit, `INTRA` only for every sample since arriving. So on the intranet iOA expects
the site resolvers to answer, and a connection refused on `192.168.255.10:53` is correct behaviour
rather than a fault.

The consequence is that on the office LAN the `ioa` group has no upstream at all, and because it
carries `-exclude-default-group`, each domain aimed at it resolves nowhere. Every such domain must be
claimed by the office fragment for as long as the LAN is authorized, so `network-reconfigure` derives
those `nameserver /<domain>/office` lines from the base config's own `/ioa` mappings instead of
repeating them in the fragment. The previously hand-kept list had drifted to one domain out of ten,
which is why `mirrors.tencent.com`, `oa.tencent.com`, `m.tencent.com`, `es.tencentyun.com`,
`tencentelasticsearch.com`, `mnet2.com`, `mytsearch.com`, `production.polaris` and `tco-es.polaris`
all failed to resolve from an office desk on 2026-09-21 while `woa.com` worked. The `ipset /<domain>/ioa`
mappings stay in the base config untouched, so which resolver answers never changes how the resulting
addresses are routed.

## SmartDNS IOA classification

The IOA upstream is permanently declared in the base SmartDNS configuration as
`server 192.168.255.10 -group ioa -exclude-default-group`. It is not generated from
`tun0` state: link down/up and address-change events neither rewrite an IOA fragment nor restart
SmartDNS. SmartDNS caching is disabled globally and on both listeners: every client query reaches
the selected upstream, expired answers are never served, and no prefetch runs. `rr-ttl-min 0`
disables SmartDNS's built-in 600-second TTL floor so clients receive the upstream TTL unchanged.
When IOA is unavailable, IOA-group names fail closed and may wait for the upstream timeout; they do
not fall back to a public resolver. Ordinary default-group DNS remains independent and continues
through its default and DHCP upstreams. On the office LAN that upstream is not merely unavailable but
absent by iOA's own design, which is why the office fragment re-points those domains at the lease
resolvers — see “The tunnel resolver exists only off the office LAN”.

SmartDNS adds addresses resolved for configured IOA business domains to dynamic `ipset ioa`.
That set is authoritative for domain-derived IOA classification regardless of the answer's prefix.
`NETMODE_IOA` first returns every packet whose mark is non-zero. An unmarked packet whose
destination is in `ioa` receives exact full-width mark `0x1`; priority 1150 routes that mark through
table `ioa`. Its MASQUERADE is tied to `tun0` because the first lookup may have selected a Tailscale
source before OUTPUT applied the mark. The tunnel underlay, not the payload route, moves to office
wired. Marks such as `0xa38`, `0x80000`, or another non-zero value remain untouched.

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

### The 2026-09-14 17:46 outage is unexplained, and nothing here automates a recovery

An earlier revision of this file claimed the roam produced no netlink change, that tailscaled's
link monitor saw no delta, and that it therefore never rebound. The journal contradicts all
three, so `scripts/network-exit-watchdog` and its path unit were removed rather than kept on a
false premise. What the journal actually shows:

| Time | Event | Effect on the outage |
|---|---|---|
| 17:46:04 | roam to BSSID `f8:c6:50:58:b8:0b`, carrier flaps | timeouts begin at 17:46:09 |
| 17:48:47 | `DEAUTH_LEAVING`, wlan0 down 41s, address and default withdrawn | continues |
| 17:49:30 | tailscaled logs `LinkChange: major, rebinding` (`default-if-changed,ips-changed`) | continues |
| 17:50:02 | `systemctl stop ngnclient`, SmartGate policy and `tun0` torn down | continues |
| 17:50:09, 17:53:32 | two further Wi-Fi reconnects | continues |
| 17:53:48 | `systemctl stop tailscaled` | ends; last timeout 17:53:44 |

So tailscaled did rebind, and rebinding did not help. Stopping iOA did not help either: the
timeouts ran for another three and a half minutes at 14–24 per minute. A stale SmartGate
underlay is also ruled out for this window — the gateway stayed `10.36.48.1` and the address
stayed `10.36.50.129` throughout, so the table `230` default installed at 17:36:23 still matched
`main`. By 17:53:30 tailscaled reported `netcheck: UDP is blocked` with every DERP region
unreachable and a 45s control map POST failure, meaning its own physical-path sockets were
failing, not merely its peer path.

The one defect with hard evidence is on this side: `network-reconfigure` aborted on **every**
run for the whole window (`aborted at line 103/402 (exit 1); active IOA marking left in place`,
at 17:46:05, 17:48:24, 17:48:48, 17:49:11, 17:50:03, 17:53:33). Bands are reconciled in a single
final loop, so an abort leaves the remaining bands untouched — including pref 500, the Tailscale
transport escape. Policy was not converging while the only thing responsible for converging it
exited non-zero each time. That was the `sort: write failed: Broken pipe` pipefail bug, now
fixed and covered by tests.

Until the wedge is reproduced with that bug absent, there is deliberately **no automatic
recovery**: no repository code restarts `tailscaled`, rebinds magicsock, or reacts to link
changes on the tunnel's behalf, and the static checks enforce that. Capture the next occurrence
with `network-debug-capture` and diagnose it from evidence instead.

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

### The carrier grace can keep a lease that does not belong to the attached segment

XLSMART access points sit on different VLANs. `f8:c6:50:59:08:0b` leases `10.36.43.23/21` via
`10.36.40.1`; `f8:c6:50:58:b8:0b` leases `10.36.55.44/20` via `10.36.48.1`. A reassociation inside the
grace period keeps the current lease and sends no DHCP, and nothing else checks whether that lease is
valid behind the new access point. When it is not, the machine holds an address and a gateway from a
segment it is no longer on, which is a total blackhole that looks exactly like a physical fault.

Resume is the reliable way to hit this. The kernel deauthenticates with reason 3 before suspending,
networkd processes that carrier loss only after resume, and iwd reassociates within the same second.
On 2026-09-18 the 12:20:14 suspend resumed at 12:24:10 onto an access point in the other VLAN with no
DHCP run at all, and the `/21` address with gateway `10.36.40.1` survived. The recovery at 12:25:35
worked only because that reconnect outlasted the grace and forced a full DHCP, which is why stopping
`ngnclient` and `tailscaled` appeared to be part of the cure and was not. A fast roam between access
points in different VLANs is the same failure without the suspend.

`scripts/network-dhcp-refresh` closes this: on every new association it renews, then confirms the
gateway named by the lease answers ARP on this segment, and replaces the lease with a full DHCP
(`networkctl reconfigure`) when it does not. A forced reassociation on 2026-09-18 at 12:34:41
reproduced the kept-lease state and the hook repaired it in eight seconds unattended.

### Roaming leaves the station unforwarded, and only reassociating rebuilds it

A valid lease is not sufficient. On 2026-09-18 the roams at 12:41:04, 12:41:35, and 12:42:08 each kept
a valid lease, and the full DHCP the hook ran after each one was answered immediately with the same
`10.36.55.44/20` via `10.36.48.1`. The gateway nonetheless stayed silent to ARP for about 100 seconds.
DHCP is precisely the traffic the controller still relays while it withholds forwarding, so no amount
of it recovers this state. What did recover it was iwd going `connected → disconnecting →
disconnected → connecting → connected` at 12:42:40: the gateway answered seven seconds later, and the
roam at 12:42:52 was immediately healthy. The controller's per-station authorisation is what has to be
rebuilt, and a roam does not rebuild it.

The hook therefore repairs this with a forced reassociation through `iwctl`, measured at two seconds to
a working gateway. `iwctl station disconnect` also clears iwd's autoconnect, so the reconnect is
explicit and uses the SSID read from the current link.

Reassociating comes before DHCP in the ladder, which is the opposite of the obvious order and is
deliberate. Roaming is the common case while walking and DHCP provably cannot repair it, so the twelve
seconds an earlier revision spent on DHCP first were twelve seconds of downtime in the case that
happens most. DHCP stays in the ladder because it is the only thing that replaces a lease from another
VLAN, which reassociating inside the carrier grace does not.

The full ladder per association, at most once each: renew, confirm; reassociate, confirm; full DHCP,
confirm. The effective repair now lands around five seconds, with a worst case near nineteen.

`REASSOCIATE_COOLDOWN` defaults to 20 seconds and is the only thing preventing a reassociation loop: a
forced reassociation produces a new association, which retriggers the hook. The floor is deliberately
short, because walking through the building produced a roam every ten to twenty-five seconds and a
repair rate-limited below the roam rate never catches up. One reassociation costs two seconds, so a
20-second floor bounds it under a tenth of the time even in the worst case.

### Do not try to stop iwd from roaming

This was tried on 2026-09-18 and reverted the same day. It is the wrong instinct twice over.

Moving `RoamThreshold`/`RoamThreshold5G` does not even change the behaviour that was observed. The
`signal:` field in iwd's `event: roam-info` line is the *candidate's* signal, not the current link's, and
reading it as the latter produced a false conclusion that RSSI was not the trigger. It was: each of the
day's eleven roams was preceded by exactly one `event: roam-scan`, which iwd only activates once the
current signal falls below the threshold, and none were `beacon-loss-roam` or `packet-loss-roam`.

Eleven `roam-scan` events producing eleven roams is a station walking through a building and correctly
following the signal. `DisableRoamingScan=true` suppresses that, and iwd documents the cost: the
station cannot roam properly, so walking out of range means holding a dying access point until it drops
rather than moving to a live one. Suppressing correct client behaviour to work around a controller that
stops forwarding trades a repairable two-second outage for an unrepairable one.

`network/test-static-policy.sh` fails if any of these directives reappears in `iwd/main.conf`.

### Do not aim the repair at a chosen BSSID either

The 2026-09-18 capture is genuine and unexplained by anything above: three 5GHz BSSIDs accepted
association, ARP and DHCP while dropping all 51 pinned SYNs, the controller eventually deauthenticated
the station, iwd selected the same AP's 2437MHz BSS, and the first SYN-ACK came back one second later.
The obvious repair is to reassociate to a specific 2.4GHz BSS rather than to the SSID.

It is not available. `iwctl station <dev> connect` takes an SSID; only `net.connman.iwd.StationDebug`
accepts a BSSID, and that interface exists solely in developer mode, which needs a drop-in replacing
iwd's `ExecStart`. Deploying that cost the machine all wireless: developer mode also drops
autoconnect, the `ExecStartPost` added to restore it exited non-zero, systemd therefore tore iwd down
after each start, and five restarts later the unit sat in `start-limit-hit` with no station at all.

Two things make that a bad trade even if the `ExecStartPost` were fixed. Owning the `ExecStart` of the
daemon that carries the only link means every iwd upgrade can strand the machine, and the failure mode
is total rather than the repairable outage it was meant to shorten. And the repair burned its own
reassociation cooldown slot before calling the unavailable command, so the generic reassociation that
does work was skipped entirely — the workaround was strictly worse than no workaround.

So the repair reconnects to the SSID and lets iwd choose. On a 5GHz blackhole that can reselect the
broken plane, and then the next association triggers the hook again. `network/test-static-policy.sh`
fails if any `iwd.service.d` drop-in reappears.

### The repair and the policy reconciliation raced, which is why roaming only broke with the tunnels up

On 2026-09-18 roaming recovered by itself with `tailscaled` and `ngnclient` stopped, and did not with
them running. That asymmetry is the evidence, and it points at neither service being special: it points
at how much work `network-reconfigure` has to do.

`network-reconfigure.path` and `network-dhcp-refresh.path` both watch `/run/systemd/netif/links`, so
one association starts both of them at the same instant. `network-reconfigure` serialises itself with
`flock -w 60 /run/lock/network-reconfigure.lock`, and the journal shows it taking one to five seconds
per run. `network-dhcp-refresh` took no lock at all, and its repair for an unforwarded station is
`iwctl station disconnect` followed by `connect`. So the repair removed the link while reconciliation
was building policy against it, and the result is `tun0`, tables `20`, `230`, `ioa`, Tailscale table
`52` and the pref 500 transport escape all referring to an address that had just been withdrawn.

With both tunnels stopped there is almost nothing for reconciliation to build, so the same race is
harmless and the repair looks reliable. Starting them does not introduce a new fault; it makes an
existing one reachable.

The fix is that `network-dhcp-refresh` now takes the same lock. Nothing else needs coordinating,
because a reassociation changes `/run/systemd/netif/links` and reconciliation therefore runs again
against the repaired link. `network-reconfigure.service` also gained `StartLimitIntervalSec=0`: it is
triggered far more often than the repair is, so a roam every ten to twenty-five seconds would exceed
the default five-in-ten allowance, fail its path unit, and stop all policy reconciliation silently —
the same trap that had already disabled the repair hook.

`network/test-dhcp-refresh.sh` holds the lock and asserts the repair blocks on it, measured by elapsed
time so that removing the `flock` fails the test rather than passing it.

This also puts the 2026-09-14 17:46 outage back in play as **an unconfirmed hypothesis**, not a
conclusion. That window is attributed above to stopping `tailscaled` at 17:53:48, but there was a
Wi-Fi reconnect at 17:53:32, sixteen seconds earlier, and today's measurements show the unforwarded
state clears only on a reassociation and takes about seven seconds to do so, with timeouts trailing.
The reconnect is therefore a candidate cure that stopping `tailscaled` may have been credited for.
`netcheck: UDP is blocked` with every DERP region unreachable is also what an unforwarded station looks
like from inside `tailscaled`. Confirming this needs a capture where the gateway is ARPed throughout;
until then the window stays listed as unexplained.

### ARP is necessary and not sufficient, and treating it as sufficient hid a whole outage

ARP proves the gateway is on this segment and that the lease names the right one. It does **not** prove
the station is being forwarded, because the controller answers ARP at layer two while still refusing to
route — which is the exact state this hook exists to repair.

That was not a theoretical gap. On 2026-09-18 the roam at 13:20:16 was followed one second later by
`association f8:c6:50:13:d4:44 on wlan0: lease confirmed on this segment`, and the network was dead for
another 78 seconds until `tailscaled` was stopped by hand at 13:21:35. The physical path was not
working during that window: `tailscaled`'s bootstrap DNS dials, which go direct and not through the
tunnel, timed out against every DERP address, and `open-conn-track` reported `lastRecv` to the exit node
climbing 3s, 12s, 19s, 23s, 31s, 39s, 48s, 58s. ARP answered throughout, so the hook saw a healthy
segment and stopped escalating in the middle of the outage it was written to fix.

Forwarding is now measured by `scripts/network-probe-tcp`, one TCP handshake that has to cross the
gateway, against the same `216.239.32.117:80` the debug captures use and on the same `0x80000` probe
mark. It must stay pinned to the interface, source and mark, and the probe sets all three before
`connect()`: marking an already-connected socket emits the tailnet source address on `wlan0` and can get
the host blocked. Exit code 2 means the probe never reached the path and measured nothing, so it is
treated as inconclusive rather than as failure — otherwise a broken probe would reassociate on every
association.

ARP is still checked first, because it is cheap and fails fast when the lease belongs to another VLAN,
and because ICMP is not an alternative for either job: this network drops it outright, to the gateway
and beyond. `network/test-dhcp-refresh.sh` covers an answering gateway that does not forward, and
removing the probe fails it rather than passing.

`arping` sends a real request rather
than reading the neighbour cache, so a stale entry cannot vouch for a gateway that is gone, and a
gateway from another VLAN cannot answer at all.

The hook records the association before acting, because reasserting a lease changes link state and
retriggers the hook. One renew and at most one full DHCP per association; a segment that stays broken
gets one attempt and the next association gets its own, so this converges instead of polling.

`network-dhcp-refresh.service` sets `StartLimitIntervalSec=0`. The path unit watches a directory
networkd rewrites on every link transition, and a burst exceeds systemd's default five starts per ten
seconds. Hitting that limit fails the *path* unit rather than just the service, which silently
disables the hook: that happened at 2026-09-17 20:38:29 and is why the 2026-09-18 resume went
unrepaired.

### The hook's state is uptime-based, so it has to die with the boot

Both stamps the hook keeps — the association start and the last reassociation — are derived from
`/proc/uptime`, which restarts at zero on every boot. The state directory was `/var/lib`, which does
not, so a stamp written during a long uptime outlived it and sat permanently in the future of a shorter
one. The cooldown compares `now - last` against 20 seconds, and with `last` in the future that
difference is negative forever: every reassociation was refused, silently, for the whole boot.

That is what the 2026-09-18 19:10 blackhole actually was. The roam at 19:10:49 took a correct lease on
the new segment, the gateway answered ARP once at 19:10:51 and then went quiet, and the hook ran on
that association and could do nothing but renew — the one step that provably does not help, because
DHCP is precisely the traffic the controller still relays. The stamp on disk read 84670 against an
uptime of 5718. The outage ran until 19:13:53, when the access point deauthenticated the station on its
own and reassociation to another BSS restored forwarding in a second: the same repair the hook was
holding off, three minutes late and not because of anything on this machine.

The state now lives in `/run/network-dhcp-refresh`, which is tmpfs and is therefore scoped to exactly
the same boot as the clock it stores. A stamp ahead of the current uptime is additionally treated as
absent rather than as a cooldown, so no future move of this state can reintroduce a silent refusal.
`restore.sh` deletes the old `/var/lib` directory, `network/test-static-policy.sh` fails if the state
leaves `/run`, and `network/test-dhcp-refresh.sh` covers a stamp from a previous boot.

### The three-minute roam outage is a source-address leak, and it is this machine's fault

The 2026-09-18 19:10 capture was read once as a controller-side problem on the grounds that the
blackhole began at the instant of the roam. That reading is wrong, and it missed that forwarding came
back in the middle and was destroyed again:

| time | event |
|---|---|
| 19:10:45–46 | roam to `f8:c6:50:58:b6:eb`; uplink dropped immediately, downlink never stopped |
| 19:10:49–50 | the repair reaches `networkctl reconfigure`, a full DHCP succeeds, `10.36.49.154` reacquired |
| 19:10:51.762651 | the gateway answers ARP — uplink is working again |
| 19:10:51.762787 | 136µs later, eight packets with source `100.88.203.53` leave `wlan0` |
| 19:10:56 onward | the gateway never answers again |
| 19:13:51 | the access point deauthenticates (reason 1); reassociation plus a full DHCP recovers in 9ms |

So there are two outages, not one. The first is about five seconds and is the known IP-learn story —
a full DHCP clears it, which is exactly what a roam looks like with both tunnels stopped. The second
is three minutes and starts 136µs after this station put an address it was never leased on the wire.

The leak is entirely repository-owned. `NETMODE_IOA` marks `cn_direct` destinations `0x2`, and
priority 1500 sends `0x2` to table main. All four leaked destinations were in the set
(`113.240.0.0/13`, `123.112.0.0/12`, `116.128.0.0/10`). The first route lookup happens before OUTPUT
marking and selects Tailscale's exit table, so the sockets themselves retain `100.88.203.53`; normal
operation reroutes them to main and source NAT presents `10.36.49.154` on wlan0.

The original diagnosis blamed an ipset refresh for changing that classification. The capture
disproves it: the before/after `cn_direct` files have identical SHA-256 hashes, and the before/after
mark chains are identical. The actual change was networkd removing the DHCP address and main default
at 19:10:49. Removing a MASQUERADE address also removes its connection state. A marked FIN then looked
up an empty main table; policy lookup failure continues at the next rule, so it fell into Tailscale
table 52 and created new no-NAT state. When main returned, the same exact `0x2` lookup selected wlan0,
but the ESTABLISHED no-NAT decision bypassed source NAT and exposed the tailnet source.

`-t nat -A POSTROUTING -o <physical> -m mark --mark 0x2 -j MASQUERADE` exists for precisely this and
works: with it in place and nothing else, `tcpdump 'src net 100.64.0.0/10'` on `wlan0` captures
nothing over 20 seconds while the rule's counter climbs by 19. What it cannot do is help an
already-established flow. The nat table is traversed only for a connection's first packet; once
conntrack has recorded that a flow needs no NAT, every later packet skips it. The eight leaked packets
were `FIN`s of long-lived connections, so nat was never consulted for them.

#### CN direct is one fail-closed lookup/stop band

Priority 1500 is now immediately followed by the exact same mark at priority 1501 with `prohibit`.
When main has a route, 1500 succeeds and nothing changes. During DHCP replacement, it cannot continue
into table 52: the socket gets `EACCES`, and no packet or no-NAT state reaches Tailscale. The lookup
and stop are installed and removed as one band — stop first when enabling, lookup first when disabling
— so an interrupted reconciliation can at worst leave CN traffic safely prohibited.

This mirrors the existing iOA owner band at 400/401. It is not connection pinning: fixing a connection
at `0x2` would still let an unsuccessful main lookup fall through, so it cannot repair this incident.

NAT reconciliation also used to delete every owned rule before adding the desired one. It now ensures
the desired output-device rule first and removes stale or duplicate copies afterward. During DHCP's
no-default interval it retains the rule for the requested physical interface, ready for the address
to return.

#### Final source validity belongs after srcnat

The obvious backstop — drop anything leaving the physical device with a tailnet source — was tried on
2026-09-18 and is wrong. `mangle POSTROUTING` runs at priority -150 and `srcnat` at 100, so at that
hook no packet has been through source NAT yet, including the ones about to be masqueraded correctly.
Installed live it dropped 19 healthy new CN connections per 20 seconds, visible as repeated `SYN`
retransmissions of the same source port. `network/test-static-policy.sh` now fails if such a rule
reappears there.

The final invariant is instead a native nftables filter chain at postrouting priority 110, ten after
srcnat. Healthy `0x2` and owner packets have already been masqueraded and do not match. Any stale
ESTABLISHED packet still carrying a tailnet (`100.64.0.0/10`) or iOA tunnel
(`192.168.255.0/24`) source is dropped before a physical interface sees it. The rule covers the same
wireless and wired name families accepted by `physical_device()`. Forwarded Tailscale traffic is
normally source-NATed before this guard as well; forwarding a raw tunnel source onto a
source-guarded LAN is invalid for the same reason as the local leak.

`network/test-cn-fail-closed.sh` contains a required negative control, removes the physical address
and route, and proves all three layers: without 1501 a tailnet FIN enters table 52; with it table 52
sees nothing; after main returns the post-srcnat guard catches stale retransmissions while new
connections acquire physical MASQUERADE state.

### The carrier grace also suppresses the DHCP a Cisco WLC requires

The grace period has a second cost, behind a controller enforcing IP learning. A Cisco WLC
forwards a station's traffic only once it has learned that station's IP address, and it
deauthenticates with `reason: 108` (`CLIENT_DEAUTH_REASON_STA_NO_IP`) when it has not. The deauth
discards the client context, so the controller must learn the address again after the reassociation —
but a reassociation inside the grace period keeps the lease, so networkd sends no DHCP. Neither side
moves, and the controller deauthenticates again one IP-learn timeout later.

On `XLSMART Public` on 2026-09-17 this closed into an exact 2m01s loop. All three BSSIDs involved
(`f8:c6:50`, `88:9c:ad`, `5c:64:f1`) are Cisco; the default gateway `00:09:0f:09:00:12` is a
FortiGate. The correlation across that evening is exact:

| Reassociation | Next event |
|---|---|
| kept the lease, sent no DHCP (18:42, 18:44, 18:46, 18:48, 18:50, 19:03, 19:06) | `reason: 108` 2m01s later |
| ran a full DHCP, after a carrier gap outlasting the grace (19:00:49, 19:09:10) | loop ends |

Both `tailscaled` and `ngnclient` were stopped from 18:01:20 through 19:04:50, so neither takes part
in this. While the controller holds no address for a station it does not forward that station's data
either, which is what makes the state look like a physical fault: associated, correct routes, gateway
ARP unanswered.

The renew that `scripts/network-dhcp-refresh` already sends per association is what the controller
needs: DHCP is the one exchange it keeps forwarding while it holds no address for the station, and the
exchange is one request and one reply answered in about 4 ms. When the renew alone does not restore
forwarding, the gateway check fails and the same escalation to a full DHCP applies, which is a fresh
DORA and therefore relearns the address unconditionally.

An association is identified by BSSID plus the association start derived from `connected time`, so
`tun0` and `tailscale0` transitions on the same watched directory do not cause a renew.

`reason: 108` appears nowhere in the journal before 2026-09-17, and the 09-14, 09-15, and 09-16
incidents each show a completed DHCP transaction, so this particular loop does not explain them.

## Reconciliation and AP changes

`network-reconfigure.path` watches networkd link state and `~/.routefile`. Link-state changes,
including creation or removal of `tailscale0` and `tun0`, trigger an integrity reconciliation. Only
physical-link and DHCP data contribute routes: tunnel interfaces are not routing-rebuild inputs.
Tailscale and SmartGateAgent still receive link changes independently and repair their own state.
The extra reconciliation only restores repository-owned rules if another transition disturbed them.

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

There is no constant packet recorder. Arm the manual command before a planned roam to collect
before/after state and parallel captures on `wlan0` and `tailscale0`. The default window is 180
seconds:

```bash
scripts/network-debug-capture --duration 240 \
    "XLSMART planned roam: room A to room B"
```

The command uses non-interactive `sudo -n`, writes a root-only incident under
`/var/log/network-debug/incidents`, and retains the newest five incidents. It never changes routes,
firewall rules, tunnel preferences, or network services. `tailscale netcheck` does perform active
connectivity probes while collecting diagnostics. Individual diagnostic failures and timeout return
codes are recorded in `manifest.tsv` instead of aborting collection. Text command output is capped at
1 MiB per command and marked `truncated=true`; each deep pcap is capped at 8 MB. The route-event
summary is bounded separately: it keeps counts and first/last events, then retains the latest matching
events in a 900 KB byte ring (at most 5,000, with each line capped at 512 bytes), so it remains below
1 MiB without discarding the newest timeline tail. The snapshots include TCP/UDP sockets and
conntrack state, kernel network counters, interface statistics, packet counters, the `ioa` classifier,
table `ioa_src`, iOA vendor logs, policy-owner logs, bounded tailscaled goroutines, and this
route-event timeline. During the capture,
`connectivity-timeline.tsv` probes four paths in parallel and records all three kernel route decisions
and the current BSSID on every sample, so a physical, Tailscale, iOA, or policy-routing failure can be
separated without restarting anything. Do not put passwords, tokens, or other secrets in the incident
note or command line.

| Column | Path it proves | How it stays independent |
|---|---|---|
| `ac` | whether mains power was connected at that sample | read from sysfs, so plugging in mid-capture is visible |
| `gateway_tcp` | TCP handshake with the default gateway | priority 1000 pins the gateway to `main` |
| `underlay_tcp` | TCP handshake with the Internet over the physical link | mark, device, and physical source are fixed before `connect()` |
| `exit_http` | the normal path, whatever policy currently selects | unmarked, so it follows the exit node |
| `ioa_http` | `ioa.tencent.com` over the corporate tunnel | unmarked, matched by the `ioa` classifier |

Only TCP is a liveness signal. Networks in use here answer ARP unreliably and drop ICMP outright, so
`underlay_icmp` and the `*-shape` snapshot probes record shape only: a failure there proves nothing,
and a success does not clear the path either. Treating a failed `arping` or `ping` as an outage
produced three wrong root causes on 2026-09-15; the marked TCP probes exist to prevent that.

The TCP columns and `manifest.tsv` carry the probe's exit code, and the three values mean different
things. Conflating the last two is what made the earlier captures unreadable:

| Code | Meaning | How to read it |
|---|---|---|
| 0 | the path carried a handshake | that path was alive at that instant; a success writes no output |
| 1 | timeout, refused, unreachable, or blocked | that path was dead, and the column identifies which one |
| 2 | the probe never reached the wire | it measured nothing, so it is not evidence either way |

Code 2 is expected during a roam: the sampler recomputes the physical source every second, and for a
moment after the address is withdrawn there is no source to bind. Reading that as an outage would
invent a failure the network did not have.

`scripts/network-probe-tcp` sets `SO_MARK` 0x80000, `SO_BINDTODEVICE`, and the current physical IPv4
source before calling `connect()`. All three are required, and the ordering is the whole point: the
first marked-TCP implementation applied the generic socat socket option after connecting, so the
kernel dropped the cached route, rerouted an established `100.88.203.53` socket onto `wlan0`, and
kept the source the first route had chosen. In incident `20260915T033547Z.YuwSeJ` the gateway's
final response was 19 ms before that leak and the physical path stopped responding on the next
sample. That capture is therefore probe-induced and cannot attribute the original outage to
Tailscale or iOA.

Two checks keep that defect from returning. `network/test-probe-source-isolation.sh` builds a
namespace with a physical and a tailnet interface, runs a deliberately wrong probe that marks an
already-connected socket, and requires it to leak the tailnet source onto the physical device before
requiring the real probe not to. A check that cannot detect the defect proves nothing, so the wrong
implementation is part of the test rather than a comment about it. `network/test-debug-capture.sh`
then rejects any capture that builds a marked socket itself, invokes the probe without pinning
device and source, or marks a `ping` without binding an interface.
`curl` cannot set `SO_MARK`, so an interface-bound `curl` is not an underlay probe — Tailscale's
fail-closed exit routing captures it and it times out whenever `tailscaled` is up, regardless of
physical health.

For a planned test, wait until the command prints `Capture armed` before walking to the other AP.
Keep one ordinary Internet request and one iOA page active during the move. If connectivity fails,
do not disconnect Wi-Fi and do not stop `ngnclient` or `tailscaled` until the command prints
`Network debug capture complete`; those actions destroy the state needed to identify the owner.
After completion, manual recovery is safe. Run one capture per roam direction.

`--bugreport` additionally runs `tailscale bugreport --diagnose`. This can upload diagnostic logs
to Tailscale and return a shareable identifier, so it is never run by default:

```bash
scripts/network-debug-capture --bugreport "incident note"
```

## Ruled out explanations for the XLSMART outages

Each of these was proposed, tested against recorded evidence, and refuted. They are listed so they
do not get re-proposed: a hypothesis that already has a counterexample is not worth another capture.

| Explanation | Refuted by |
|---|---|
| mains power versus battery | `tailscaled` started at 2026-09-14 17:18:40 while charging, on `XLSMART Public`, and the 17:46 outage still happened; the 18:14:20 start was also on mains and wedged around 18:53 |
| Wi-Fi power save | `iw dev wlan0 get power_save` reads `off` in the before and after snapshot of all five captures, on battery and on mains, and `iwlwifi.power_save=N`; TLP sets `WIFI_PWR_ON_AC` and `WIFI_PWR_ON_BAT` both to `off` |
| PCIe ASPM on battery (`PCIE_ASPM_ON_BAT=powersupersave`) | `wlan0` is `00:14.3`, a CNVi *Root Complex Integrated Endpoint*, so it has no PCIe link and reports no `LnkCtl`, `LnkSta`, or ASPM state at all |
| iOA being present | `ngnclient` started 2026-09-15 11:04:07 and ran continuously across both the 11:05 and 11:09 failures and the 12:19 success |
| `tailscaled` being inherently incompatible | it ran 87 minutes on `RCMK` on battery, and overnight across a suspend, with no outage |

A long healthy run on `RCMK` is not evidence about `XLSMART Public`. Compare captures only within
the same SSID.

The failures also separate into two shapes, and treating them as one produced contradictory
conclusions. Keep them apart:

| Shape | Signature | Instances |
|---|---|---|
| slow wedge | `tailscaled` healthy for tens of minutes, then a roam or a `tun0` transition breaks the exit path | 2026-09-14 17:46 (28 min in, roam) and ~18:53 (39 min in, iOA GUI restart) |
| fast failure | connectivity fails within seconds of starting `tailscaled` | 2026-09-15 11:05 (26 s) and 11:09 (13 s) |
| IP-learn kick loop | AP-initiated `reason: 108` exactly 2m01s after every reassociation that sent no DHCP, with both services stopped | 2026-09-17 18:21 through 19:08 |
| lease from another VLAN | a reassociation inside the carrier grace keeps an address and gateway from the previous segment, with no DHCP in the journal | 2026-09-18 resume at 12:24:10, reproduced at 12:34:41 |

The third and fourth shapes are explained, and `scripts/network-dhcp-refresh` addresses both. The
first two remain open; `reason: 108` appears in neither, and both show a completed DHCP transaction, so
neither is a stale-lease case either.

There is still no uncontaminated capture of the first two shapes: the 11:25 and 11:35 captures predate
the probe fix, and the 12:19 capture is the first clean one but records a success. It is the healthy
baseline to diff a future failure against.

## Testing and diagnostics

Run the network checks:

```bash
bash network/test-ip-override.sh
bash network/test-smartgate-underlay.sh
bash network/test-ioa-wrapper.sh
bash network/test-tun-underlay.sh
sudo -n bash network/test-nft-direct-routing.sh
bash network/test-smartdns-no-cache.sh
bash network/test-ioa-fail-closed.sh
bash network/test-install-ioa-62.sh
sudo -n bash network/test-ioa-static-lan-overlap.sh
sudo -n bash network/test-reconfigure.sh
bash network/test-static-policy.sh
bash network/test-dhcp-refresh.sh
bash network/test-debug-capture.sh
bash network/test-network-probe-tcp.sh
bash network/test-probe-source-isolation.sh
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
