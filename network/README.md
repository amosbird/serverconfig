# Network Configuration

Migration from `netctl + dhcpcd + wpa_supplicant` to `iwd + systemd-networkd`.

## Architecture

```
iwd                      → Wi-Fi management (scan, connect, roam, EAP-TLS)
systemd-networkd         → DHCP, address, default route (all interfaces)
wpa_supplicant@enp*      → Wired 802.1X only (triggered by udev on cable plug)
SmartDNS (127.0.0.1:53)  → DNS resolution, domain-based split, captive portal
network-reconfigure      → Reconciles policy routing + DHCP DNS on link change
network-fallback         → Detaches the exit node while it cannot carry traffic
```

## Key Behaviors Preserved

| Original (netctl) | New (iwd+networkd) |
|---|---|
| `netctl switch-to <wifi>` | `iwctl station wlan0 connect <ssid>` (or auto) |
| `ncswitch` manual switching | iwd auto-roams; no manual switch needed |
| `DhcpcdOptions='-L -G'` on Tencent-WiFi | `UseGateway=false UseRoutes=false` in .network |
| dhcpcd hook → addroutes | **Removed** — no auto route injection on DHCP |
| wpa_supplicant for wifi+wired | iwd for wifi; wpa_supplicant only for wired 802.1X |
| udev → netctl start wired | udev → wpa_supplicant@enp*.service |
| resolv.conf → 127.0.0.1 | Same (immutable) |
| SmartDNS: 8.8.8.8 plaintext | SmartDNS: 8.8.8.8 DoT + DHCP DNS fallback |

## Usage

Day to day, the configs here are deployed by the normal dotfiles run:

```bash
GUI=1 bash restore.sh    # copies configs + enables the units, never touches
                         # a running service, so it can't drop the connection
```

Switching stacks (first time, or coming back from a rollback) is a separate,
disruptive operation:

```bash
# Step 1: Deploy configs (safe, no network change)
sudo bash network/migrate.sh install

# Step 2: Switch over (brief network drop)
sudo bash network/migrate.sh activate

# If anything breaks:
sudo bash network/migrate.sh rollback
```

## Exit Node Fallback

The tailscale exit node is normally on, and a selected exit node puts
`default dev tailscale0` in table 52 — so every default-routed packet depends on
that tunnel working. When it cannot carry traffic the route is a black hole and
the machine is simply offline. Causes vary — captive portal, UDP blocked, DERP
unreachable, exit node down — but the response is the same, so
`network-fallback.service` reconciles a single bit:

| state | action |
|---|---|
| exit node usable | attached |
| exit node unusable | detached, re-attached when a direct path returns |

### Why it detaches rather than routes around

An earlier version added `ip rule pref 4500 lookup main` to divert traffic past
the tunnel. Measured: the rule went in and the machine stayed offline for 45
seconds, until the exit node was removed by hand.

A routing rule only affects a route *lookup*, and a socket looks up once. With
an exit node attached the lookup returns `dev tailscale0 src 100.88.203.53`, so
every established connection has already bound the tailscale address. Adding the
rule sends those packets out the physical link still carrying a source address
that only exists inside the tunnel, and replies cannot come back. New
connections would be fine — but the application is retrying the ones it already
has, which is exactly the traffic the rule cannot help.

Detaching works because tailscaled then withdraws the table 52 default itself
and every socket renegotiates against the physical link. It is also what a human
does to recover, every time.

### The judgement never reads its own output

An earlier version consulted the rule it had installed — to stop DHCP option
114, which persists after login, from pinning the bypass on forever — and
self-oscillated at ~1Hz: bypass on → skip the portal check → tailscale looks
healthy → bypass off → portal detected again. Any judgement that depends on its
own effect does this.

So each question is answerable on its own:

1. **Is a portal intercepting?** RFC 8910 puts a capport API in DHCP option 114,
   and RFC 8908 makes it answer `{"captive": true|false}` directly. Without that
   API, interception has a positive signature: the plain-HTTP probe is answered
   by something that is not the real endpoint. A *failed* request is not that
   signature — it means the network is down, and treating it as a portal is how
   an unrelated outage used to pin the bypass on permanently.

2. **Is traffic crossing the tunnel?** A probe bound to `tailscale0`. Not
   tailscale's own health warnings: they are advisory and flap. On an access
   point that blocks DERP TLS, `no-derp-connection` toggled 103 times in one
   boot while peer-to-peer UDP carried traffic perfectly.

3. **While detached, has a direct path returned?** `tailscale ping` reports the
   path it used. Only `via <ip>:<port>` counts; `via DERP(...)` does not. On the
   networks where this daemon has any work to do, DERP is precisely what is
   blocked, so a relayed pong would send it straight back into the black hole it
   just escaped.

The probes are pinned to an address (`--resolve`), because this daemon runs when
the exit path is a black hole and DNS is usually the first casualty. A probe
that had to resolve a name would fail for a reason unrelated to the exit node,
or block behind the dying resolver.

Only a detachment this daemon performed is undone by it: the exit node it took
away is recorded in `/var/lib/network-fallback/detached-exit-node`. If the user
turns the exit node off, there is no file and nothing to restore.

Wakeups come from `tailscale debug watch-ipn` and from a timer — 20s while
detached, because a detached exit node produces no events at all and recovery
would otherwise never be noticed; 120s while healthy. `network-reconfigure` does
*not* restart this daemon on link changes: that used to destroy the detachment
record and SIGTERM it mid-reconcile on every churn.


## Policy Routing

### Underlay vs overlay

Traffic splits into two kinds, and they must never share a priority band:

- **underlay** — the packets that make a tunnel exist: tailscale's WireGuard,
  DERP and control-plane traffic, IOA's handshake with its gateway, and the DNS
  lookups those depend on. Sending any of these into a tunnel is a circular
  dependency — the tunnel cannot come up because the packets that would bring
  it up are waiting for the tunnel. Underlay always leaves via the physical
  gateway.
- **overlay** — everything the user actually wants to move, split by policy
  across tailscale / IOA / direct.

Every routing bug found here was one underlay packet captured by an overlay
rule: a full-width `MARK` erasing tailscale's own `0x80000` and dropping
WireGuard into the IOA tunnel; the CN table grabbing Chinese DERP nodes; and
the exit-node catch-all swallowing control-plane and bootstrap DNS, which
deadlocked tailscaled completely. Ordering alone does not prevent this — the
underlay band has to exist and be listed first.

### Reconciling on change, not on notice

The `.path` unit watches `/run/systemd/netif/links`, which every interface
writes to — including the ones this stack's own actions create. Measured over
one day: 79 reconfigure runs, of which **two** were needed. The rest were DHCP
renewals that changed nothing (38), carrier flaps (76 events), and — worst —
`tailscale0` and `tun0` appearing and disappearing (31 and 86), which are the
*results* of our own configuration rather than changes to it.

That was not merely wasteful. A rebuild deletes every rule it owns before
reinstalling them, so each run is a multi-second window in which LAN, private
and CN traffic falls through to tailscale's catch-all. When the run was
triggered by `tailscale up`, that window lands squarely on tailscale's own
handshake:

```
15:05:52.836  reconfigure starts (fired by tailscale0 appearing)
15:05:52.873  tailscaled rebinds, drops its DERP connections
15:05:53.063  NetInfo: udp=true          <- UDP works here
15:05:56.065  netcheck: UDP is blocked   <- and not here
15:05:57.911  reconfigure finishes
```

The underlay table is rebuilt inside that window, so tailscale's STUN probes go
out while the DERP addresses have no route. netcheck concludes UDP is blocked,
gives up on a direct connection and falls back to a relay — and on an access
point that blocks DERP TLS, the relay is a black hole. The machine goes offline
seconds after tailscale comes up, looking exactly like a routing bug in whatever
changed last. Rebuilding the ~8800-route CN table in the same window adds enough
netlink traffic to make tailscaled log `netlink receive: no buffer space
available` and lose track of the link entirely.

So the reconciler exits early unless something changed. "Changed" is two
questions, not one: has the network moved — the interface, the gateway, the DHCP
resolvers — and is the rule set still the one we would build?

The second question is a fingerprint, compared in both directions. Checking only
that our rules are *present* misses the case that cost an outage: a rule nobody
asked for, added inside a band we own, is never removed because every later run
reports "no change" and exits before it can be reconciled away. IOA, docker and
libvirt all add rules near our bands, so this is not hypothetical. The
fingerprint is computed from `DESIRED_BANDS`, the same table the apply loop
consumes, because a check that derived the answer differently from the code that
applies it would eventually disagree with itself — and that disagreement would
read as a routing bug rather than as what it is. That check takes 0.4s against
2.4s for a rebuild. `FORCE=1` overrides it for the operator path (`netfix`,
manual runs).

`tun0` is deliberately not in that set, and getting it wrong cost an outage. It
was added so `/etc/smartdns/ioa-dns.conf` could follow the tunnel — and because
the tunnel blinked 86 times in one day, every blink became a six-second rebuild.
What follows a tunnel is DNS, not routing. The SmartDNS section therefore runs
*before* the change check, unconditionally, writing its three fragments and
restarting the resolver only when a file's content actually changed; routing
needs no reaction at all, because an empty `table ioa` is a pass-through.

The resolver list is sorted before it is used, for the same class of reason: a
DHCP renewal is free to return the same servers in a different order, and
unsorted that looked like a network change — a full rebuild plus a resolver
restart, for an identical set.

Being installed is checked as well as being unchanged. The state triple says the
network has not moved, not that our configuration survived: the verification at
the end of the script removes the mangle jump when an IOA endpoint would be
routed into the IOA tunnel, and nothing about that is visible in the triple. So
the early exit also requires the underlay table and rule, the CN rule, the CN
table pointing at the current gateway, and the mangle jump to be present.

Two properties make early exit safe:

- **An empty table is a pass-through, not a black hole.** A rule pointing at an
  empty table simply fails to match and the packet falls to the next rule.
  Verified: with tailscale down, `table 52` is empty, `pref 5270 lookup 52` is
  still installed, and traffic routes normally via `main`. So rules may stay
  installed across tunnel restarts — nothing has to be torn down when a tunnel
  goes away.
- **Rules are cheap and idempotent; table *contents* are what can be wrong.**
  A table left pointing at a previous network's gateway is a black hole, which
  is why the underlay and CN tables are keyed to the gateway and rebuilt when it
  moves — and only then.

### Testing: in a namespace, never on the live machine

`network/test-reconfigure.sh` runs the reconciler against injected faults inside
a network namespace:

```
bash network/test-reconfigure.sh      # needs root
```

The script under test is built from `scripts/network-reconfigure` on every run,
with only its *external* couplings stubbed — `networkctl`, `systemctl`,
`tailscale`, `dig`, and the paths under `/etc` and `/var`. No routing logic is
replaced, so what passes there is what runs here.

It exists because the alternative was tried: injecting faults on the live
machine and trusting the script to repair them. That is circular — if the script
can repair it the test proves nothing, and if it cannot the machine goes
offline. It went offline three times before this file existed, once from a
stray `to 8.8.8.8 lookup main` at pref 2500, which sent a DNS upstream into the
exit-node tunnel and took DNS with it.

Twenty-four checks, covering idempotence, foreign and missing rules, a deleted
band, the no-empty-window guarantee, the IOA escape table going stale, a roam
onto an AP with a different subnet/gateway/resolvers, and a cold boot with no
rules, no tables and no state file.

The harness is checked by mutation: disable the delete path in `reconcile_pref`
and 2 checks must fail; disable the add path and 7 must fail; drop the staleness
test on the IOA escape table and 1 must fail; stop pinning the DHCP resolvers,
the gateway, or the cn-table gateway comparison and 1 each must fail. An earlier
version of this harness reported twelve passes with the delete path removed — it
had silently re-run a stale copy — so a test that cannot fail is treated as a
broken test. Adding the roam case found a second instance: the fixture's
routefile was missing the `route add` verb, so the cn table was never built in
any run, and no check had ever looked at it.

### Priority bands

| band | purpose |
|---|---|
| 0-99 | kernel local |
| 490 | IOA's own escape (`fwmark 0xa38`), installed by SmartGateAgent via `scripts/overrides/ip`. It carries no priority of its own, so the kernel derives one; we re-pin it here on every reconcile, but the agent re-adds it whenever the tunnel reconnects and it drifts back to 499. That is cosmetic — 499 is still ahead of the underlay band — so the pin is repair rather than prevention, and `network-status` matches this rule by mark rather than by priority |
| 500-999 | **underlay** — tunnel infrastructure, never enters a tunnel |
| 1000-1999 | directly-connected networks, **plus the physical gateway and the DHCP resolvers named explicitly** — see "The local network is not always outside the tunnel ranges" below |
| 2000-2999 | IOA — Tencent intranet |
| 3000-3999 | tailnet peers, then the private-range catch-all |
| 4000-4999 | CN direct |
| 5000+ | tailscale's own rules, then the exit node |


The underlay band is a single route table (`underlay`, id 500) rather than one
rule per address — the kernel walks the rule list linearly for every packet,
and the ~90 DERP relays alone would make that walk an order of magnitude
longer. DERP addresses are read from the running daemon and cached in
`/var/lib/network-reconfigure/derp-ips`, so they are still available during a
cold start, which is exactly when tailscale's bootstrap DNS needs them.

### Ownership

`network-reconfigure` owns the entire layout and reapplies it on every link
change. There is no mode switch and no state file: China-direct routing and IOA
are always on, and the office LAN is detected rather than declared — the wired
link only comes up after 802.1X against the corporate switch, so its presence
*is* the signal.

`network-status` prints what the kernel is actually doing. It is read-only and
needs no privileges, so its answer can never be stale or contradict reality.

`netfix` is the tool to run when the network misbehaves:

```
sudo netfix
```

It repairs, verifies and diagnoses in one pass, logging everything to
`/var/log/netfix.log`. It is safe to run at any time and safe to lose the
terminal while it runs: a detached watchdog is armed before anything is touched,
and an EXIT trap covers every other path, so if connectivity is not working when
it finishes it strips policy routing back to the configuration that always works
— plain default route, no marking, no exit node — and says so.

An earlier version had a `netmode` tool that reimplemented the layout, giving
two sources of truth that drifted: `netmode status` reported "IOA: OFF" while
the IOA rules were installed, because `network-reconfigure` had put them there.
Its remaining switches then became vacuous — `dns travel` and `dns ioa` were an
empty file and a copy of the base config once IOA became unconditional — so the
tool was removed rather than kept as a facade.

### Failure containment

The underlay table is the one thing that must never be lost: without it,
bringing up the exit node routes tailscale's own control-plane and DERP traffic
into the tunnel it is trying to build, and tailscaled deadlocks. Three
properties keep that from happening:

- **Staged rebuild.** Routes are built in table 501 and swapped in only if the
  result holds at least three quarters of what is already live. A run during a
  DNS outage or with tailscaled down keeps the previous table instead of
  replacing it with a much shorter one. The threshold is not "at least as
  large": the DERP list churns by a route or two between runs, and rejecting on
  any shrink froze the table permanently — a newly resolved IOA endpoint could
  never get in, while the ipset that exempts it moved ahead.
- **Identity before size.** A table built for a *different* network is never a
  candidate to keep, however large it is. Its routes read "via the old
  gateway", so keeping it does not preserve protection, it installs a black
  hole — and the size guard would then defend that black hole against every
  correct rebuild that followed. The test is whether the live table is *mostly*
  on the current gateway, not whether any single route matches: the resolver
  pins are written straight into the live table with the current gateway even
  when a swap was rejected, so one matching route proves nothing. Ninety stale
  routes plus one fresh pin would otherwise certify the stale table as current,
  and it would never be replaced.
- **Validated input.** `ip -batch` stops at the first *parse* error — `-force`
  only tolerates kernel errors — so a single malformed entry would silently
  truncate everything after it. Addresses are matched against an IPv4/CIDR
  pattern before being emitted.
- **Cached sources.** Both the DERP list and the IOA endpoint addresses are
  cached under `/var/lib/network-reconfigure/`. Neither source is reliable at
  the moment this runs: tailscaled may be down, and SmartDNS is restarted by
  this very script. The IOA endpoints are *unioned* with the cache rather than
  replacing it: SmartDNS answers with whichever of several valid endpoints
  measured fastest at that instant, so replacing would leave the others
  unprotected.
- **The DHCP resolvers, unconditionally.** SmartDNS forwards to whatever the
  network handed out, and `/etc/resolv.conf` points at SmartDNS — so a resolver
  reachable only through a tunnel means nothing resolves until that tunnel is
  up, including the names needed to bring it up. They are written straight into
  the table, outside the staged swap, because everything else here is an
  optimisation (lose a DERP relay and tailscale picks another) while losing the
  resolver is a total outage with no way back.

  This was the second-costliest bug in the file's history, and it masqueraded as
  a property of the wifi. On an access point whose DHCP resolver *is* the gateway
  (172.20.0.1) the pref 1000 link rule covers it by accident and everything
  works. On one that hands out a public resolver (202.152.254.230) nothing
  covered it, so the moment tailscale installed its catch-all every lookup went
  through the exit node — and that access point blocked DERP TLS and UDP, making
  the exit path a black hole. It was never the access point. It was the one
  address DNS cannot live without missing from the table whose entire job is
  "must not need a tunnel".

  The lease is parsed with a small awk state machine because `networkctl` wraps
  multi-value fields onto continuation lines; reading only the matching line
  finds the first resolver and silently drops the rest.

The rule itself is installed whenever the table has content, even if this run
could not rebuild it, so a momentarily missing default route cannot strip
protection that is already in place. If the table really is empty the rule is
withheld and the reason is logged.

### The local network is not always outside the tunnel ranges

IOA's intranet is `9.0.0.0/8`, `10.0.0.0/8` and `100.12.0.0/16`. That last one
is where every IOA service actually resolves (git, tapd, iwiki and mirrors are
all `100.12.0.x`); it is RFC 6598 carrier-grade NAT space rather than RFC 1918,
which is why it was missed for so long — it does not look like an intranet.

The ranges are broad enough to swallow the network the laptop is standing on.
An access point handing out `10.36.48.0/20` is inside `10.0.0.0/8`, so the pref
2500 rules captured the local network and its gateway; the previous access point
was `172.20.0.0/20` and never overlapped. Whether the stack worked came down to
which wifi was in range.

So the physical link's own infrastructure — the gateway and the DHCP resolvers —
is named explicitly at pref 1000, ahead of everything that could capture it.
Infrastructure the link is made of cannot be routed through something that runs
on top of it, and that must not depend on a range comparison that happens to
work.

### Marking is a whitelist, not a blacklist

`NETMODE_IOA` marks a packet only when its destination is in **both**
`ipset ioa` (SmartDNS's answer to "is this an IOA service") and `ipset
ioa_intranet` (the ranges above). The second condition is what makes the first
one safe to be wrong.

SmartDNS fills `ioa` from domain rules, and those are suffix matches:
`ipset /woa.com/ioa` catches `sgw.woa.com`, which is the IOA tunnel's own public
entry point. Marking it routed the tunnel's handshake into the tunnel it was
building — IOA died, DNS died with it, and nothing left running could repair it.
The recovery was always `iptables -t mangle -D OUTPUT -j NETMODE_IOA`.

Three attempts fixed this by *excluding* the bad addresses: an endpoint cache, a
dedicated exemption ipset, a cgroup match on SmartGateAgent. Each is a list of
exceptions racing a list SmartDNS rewrites on every lookup, and a list that has
to be complete to be safe will eventually be incomplete — every endpoint the
agent moved to was a fresh outage.

Inverting it ends the race. A public tunnel endpoint is not in the intranet
ranges, so it cannot be marked no matter what SmartDNS decides about it. The
worst a wrong ipset entry can now do is fail to help, and the failure mode is
the safe one by construction: an unmarked packet goes out the physical link and
works.

### No liveness gate on the IOA rules

The rules at pref 2500 point at `table ioa`, whose single route is
`default via 192.168.255.1 dev tun0`. Gating them on whether the tunnel is up is
the obvious idea and it is wrong twice over.

It is unnecessary: SmartGateAgent removes the route and the interface together,
within about two seconds of the tunnel dropping. An empty table is not a black
hole — the lookup misses and the packet falls through to the next rule.

It is also actively harmful. Gating on SmartGateAgent's `:443` control sessions
was tried; they are short-lived, so a sample landing between them reads "down"
while the tunnel is fine — 29 of 30 one-second samples read down while
`ioa.tencent.com` answered 301 throughout. The rules were deleted and reinstalled
every few seconds, flapping the routing table under live connections. A stable
wrong answer beats an oscillating one.

The one thing that *does* follow the tunnel is DNS. SmartDNS's intranet resolver
(`192.168.255.10`) lives inside tun0, so naming it while the tunnel is down
makes every lookup that matches an IOA domain rule wait for a timeout — and
SmartDNS holds the answer meanwhile, which turns a dead tunnel into slow DNS for
unrelated traffic. `network-reconfigure` writes that server line into
`/etc/smartdns/ioa-dns.conf` while `tun0` has an address and empties the file
when it does not. Measured with the tunnel down: 0.34s for an IOA name, against
several seconds of upstream timeout before.

### Failing safe

The script deletes every rule it owns before rebuilding them, so an abort in
between is worse than not running at all, and the machine goes offline with
nothing left running to repair it. Three things prevent that.

An `ERR` trap restores a minimal safe state. It removes the `mangle OUTPUT`
jump — the same `iptables -t mangle -D OUTPUT -j NETMODE_IOA` that was the
manual recovery every time this happened — so unmarked traffic falls through to
table main, which works. Marking is an optimisation; being reachable is not. It
also puts the `pref 500` underlay rule back if the table still has routes,
because the worse failure is the other one: with that rule missing and
tailscale's own catch-all at `pref 5270` still live, tailscale's control plane
and DERP traffic goes into the tunnel it is building, and removing the mangle
jump does nothing for it.

The trap only works because the shell is `set -Eeuo pipefail`. Without `-E` an
ERR trap is not inherited by shell functions, so a failure inside `warn` or
`has_routes` would kill the script with the recovery hook silently skipped —
exactly the situation it exists for.

The last step then checks both directions of the one invariant that has ever
taken the machine offline, by asking the kernel the same question a packet
asks. `ip route get <ioa-endpoint> mark 1`: if an IOA tunnel endpoint would be
routed into the IOA tunnel, the jump comes out. `ip route get <derp-relay>`: if
an underlay address would be routed into tailscale0, `pref 5270` comes out
(tailscale reinstalls it on its next reconfiguration, so the removal unblocks
the daemon rather than changing anything). The IOA check reads the endpoint
*cache*: it must test the endpoints that were NOT protected, and any list built
from what already succeeded can only confirm what is known good.

A `flock` serialises runs. The `.path` unit fires several times per DHCP renew,
and the destructive middle of the script is not safe against a second copy: one
run's `ip route flush table 500` lands while another has already installed the
rule pointing at it. Both waits for the link were also hoisted above the reset;
they used to sit in the middle of it, leaving a ten-second window with no
overlay rules at all while tailscale's catch-all was still live.

Every `ip rule add` is `|| true`. The pref it uses was cleared during the reset,
so EEXIST can only mean "already correct" — while an unguarded add aborts one
line after the reset, with the overlay rules deleted and not yet rebuilt. Two
interfaces sharing a scope-link subnet is enough to trigger it, which a docker
bridge or a VPN reconnect produces routinely.

This matters because `set -euo pipefail` is unusually sharp here. `ip route show
table X` exits 2 when the table has never been created — the state of every
table on a cold boot; `dig` exits 9 when no server answers — which happens
because this script restarts SmartDNS; `cat` exits 1 on a cache that does not
exist yet; `grep` exits 1 on no match; and `head` closes the pipe, SIGPIPEing
its writer. Under `pipefail` each of those killed a run mid-rebuild. They are
all handled now, but the trap is what makes the next one survivable.

### Known limitation

Traffic to an ipset-matched IOA address advertises MSS 1240 instead of 1460.
`connect()` picks a route before the packet reaches `mangle OUTPUT`, so the
socket has already bound tailscale's source address and its 1280-byte MTU; the
mark then reroutes the packets out tun0 and SNAT rewrites the source, but the
MSS was fixed at socket setup and cannot be rewritten later (TCPMSS in OUTPUT,
POSTROUTING and clamp-to-pmtu were all tried). Measured throughput difference
is within noise, so this is left as-is; fixing it properly needs destination
rules known at connect() time, which the dynamic ipset cannot provide.

## Files

```
network/
├── iwd/
│   └── main.conf                    → /etc/iwd/main.conf
├── systemd-network/
│   ├── 10-ignore-virtual.network    → /etc/systemd/network/
│   ├── 20-wired.network             → /etc/systemd/network/
│   ├── 25-wireless.network          → /etc/systemd/network/
│   └── 26-wireless-tencent.network  → /etc/systemd/network/
├── smartdns/
│   ├── smartdns.conf                → /etc/smartdns/smartdns.conf
│   ├── office.conf                  → /etc/smartdns/ (only while wired)
│   ├── dhcp-dns.conf                → /etc/smartdns/ (seed; rewritten per link)
│   └── (ioa-dns.conf)               → /etc/smartdns/ (written per link; not in repo)
├── systemd/
│   ├── network-reconfigure.path     → /etc/systemd/system/
│   ├── network-reconfigure.service  → /etc/systemd/system/
│   ├── network-fallback.service     → /etc/systemd/system/
│   └── wpa_supplicant@.service.d/
│       └── override.conf            → /etc/systemd/system/...
├── udev/
│   └── 90-wired-8021x.rules         → /etc/udev/rules.d/
├── test-reconfigure.sh              # Fault injection, in a network namespace
├── migrate.sh                       # Migration/rollback script
├── rollback/                        # Pre-iwd hooks, kept for `migrate.sh rollback`
└── README.md                        # This file

scripts/                             # symlinked into $PATH via ~/scripts
├── network-reconfigure              # the reconciler; owns the whole layout
├── network-fallback                 # detaches the exit node while it is unusable
├── network-status                   # read-only report, no privileges needed
└── netfix                           # repair + verify + diagnose, run when broken
```

## Credentials are not in this repo

Anything holding a key stays out of git and is written to its final location by
hand. `migrate.sh install` only *checks* that these exist and warns if they do
not:

| file | contents |
|---|---|
| `/var/lib/iwd/Tencent-WiFi.8021x` | EAP-TLS identity + client cert |
| `/var/lib/iwd/*.psk` | Wi-Fi passphrases (iwd stores them in plaintext) |
| `/etc/wpa_supplicant/wpa_supplicant-wired.conf` | wired 802.1X credentials |

They should be mode 600 — iwd refuses to load a `.psk` that is world-readable,
and the others are secrets regardless. To audit:

```bash
sudo find /var/lib/iwd /etc/wpa_supplicant -type f ! -perm 600 -ls
```
