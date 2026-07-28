# Network Configuration

Migration from `netctl + dhcpcd + wpa_supplicant` to `iwd + systemd-networkd`.

## Architecture

```
iwd                      → Wi-Fi management (scan, connect, roam, EAP-TLS)
systemd-networkd         → DHCP, address, default route (all interfaces)
wpa_supplicant@enp*      → Wired 802.1X only (triggered by udev on cable plug)
SmartDNS (127.0.0.1:53)  → DNS resolution, domain-based split, captive portal
network-reconfigure      → Reconciles policy routing + DHCP DNS on link change
network-fallback         → Bypasses the exit node while it is unusable
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
`default dev tailscale0` in table 52 — so all default traffic then depends on
that tunnel working. When it cannot carry traffic the route is a black hole and
everything breaks. Causes vary — captive portal, UDP blocked, DERP unreachable,
exit node offline — but the response is always the same, so
`network-fallback.service` reconciles a single bit:

| state | action |
|---|---|
| exit node selected, usable | no rule, normal policy routing |
| exit node selected, broken | `ip rule pref 4500 lookup main`, straight out the physical gateway |
| no exit node selected | no rule; nothing to bypass |

The judgement is a **pure function of external facts** and never reads the
bypass rule. This is the load-bearing constraint: an earlier version consulted
it and self-oscillated at ~1Hz (bypass on → skip the portal check → tailscale
looks healthy → bypass off → portal detected → repeat). Any judgement that
depends on its own output does this.

Facts, evaluated in order:

1. **Exit node selected?** `tailscale status --json .ExitNodeStatus` — null
   means nothing to bypass, regardless of how broken the network is.
2. **Portal intercepting?** DHCP option 114 (RFC 8910) gives the capport API
   URL; RFC 8908 then answers `{"captive": true|false}` authoritatively. That
   answer stands on its own, which is what removed the oscillation — and it
   also means authentication completion needs no guessing.
3. **Tunnel broken?** `.ExitNodeStatus.Online` covers that specific node going
   down, `.Health[]` covers UDP blocked / DERP unreachable.

Without a capport API, step 2 falls back to probing plain HTTP — but only a
*redirect* counts as interception. A failed request means the network is down,
not that a portal is present; treating failure as a portal is how an unrelated
outage used to pin the bypass on permanently.

**Opening the login page** is the part that actually makes hotel Wi-Fi usable.
Portals intercept plain HTTP only and cannot touch HTTPS, which is all a modern
browser emits — so the login page never appears on its own and the network just
looks broken. RFC 8908 supplies `user-portal-url`, which is opened directly.

**Wakeups** come from `tailscale debug watch-ipn` (tailscaled state changes)
and from `network-reconfigure` restarting the unit on link changes — the latter
matters because joining a portal network may produce no tailscale event at all.
Nothing is parsed out of `watch-ipn` (an explicitly unstable interface); it only
says "something changed, look again". A timeout is always armed on top, because
behind a portal tailscale may consider itself perfectly healthy and never send
an event: 20s while bypassed (recovery must be noticed), 120s otherwise (only
to catch a rule installed behind the daemon's back).

DNS is handled separately: DHCP-provided DNS is added to SmartDNS's default
group by `network-reconfigure`, so when public DoT upstreams (8.8.8.8:853) are
unreachable SmartDNS falls back to the local DHCP DNS. Once authenticated, DoT
resumes winning on speed-check.

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

### Priority bands

| band | purpose |
|---|---|
| 0-99 | kernel local |
| 490 | IOA's own escape (`fwmark 0xa38`), installed by SmartGateAgent via `scripts/overrides/ip` and pinned here — it carries no priority of its own, so the kernel would otherwise derive one from whatever we happened to install first |
| 500-999 | **underlay** — tunnel infrastructure, never enters a tunnel |
| 1000-1999 | directly-connected networks |
| 2000-2999 | IOA — Tencent intranet |
| 3000-3999 | tailnet peers, then the private-range catch-all |
| 4000-4999 | CN direct (4000), then the escape rule (4500) |
| 5000+ | tailscale's own rules, then the exit node |

The escape rule belongs at the bottom, not the top: it must divert exactly the
traffic that would otherwise use the exit node. Placed early it also captures
IOA and tailnet traffic, so an exit-node failure would take the unrelated IOA
tunnel down with it.

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
  result is at least as large as what is already live. A run during a DNS
  outage or with tailscaled down keeps the previous table instead of replacing
  it with a shorter one.
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
- **One list, two consumers.** That same file feeds both the underlay route
  table and the `ioa_underlay` ipset that exempts those addresses from marking.
  They were resolved separately once, drifted apart for the reason above, and an
  endpoint that was in the set but not in the table got routed into the very
  tunnel it builds — which kills IOA and takes DNS with it.

The rule itself is installed whenever the table has content, even if this run
could not rebuild it, so a momentarily missing default route cannot strip
protection that is already in place. If the table really is empty the rule is
withheld and the reason is logged.

### Failing safe

The script deletes every rule it owns before rebuilding them, so an abort in
between is worse than not running at all: packets stay marked for a tunnel that
has no rule to reach it, and the machine goes offline with nothing left running
to repair it. Two things prevent that.

An `ERR` trap removes the `mangle OUTPUT` jump — the same
`iptables -t mangle -D OUTPUT -j NETMODE_IOA` that was the manual recovery every
time this happened. Unmarked traffic falls through to table main, which works.
Marking is an optimisation; being reachable is not.

The last step then checks the one invariant that has ever taken the machine
offline: it asks the kernel, via `ip route get <endpoint> mark 1`, whether an
IOA tunnel endpoint would be routed into the IOA tunnel. If so it removes the
jump itself and logs which address was wrong.

This matters because `set -euo pipefail` is unusually sharp here. `ip route show
table X` exits 2 when the table has never been created — the state of every
table on a cold boot; `dig` exits 9 when no server answers — which happens
because this script restarts SmartDNS; and `grep` exits 1 on no match. Under
`pipefail` each of those killed the run mid-rebuild. They are all handled now,
but the trap is what makes the next one survivable.

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
│   └── dhcp-dns.conf                → /etc/smartdns/ (seed; rewritten per link)
├── systemd/
│   ├── network-reconfigure.path     → /etc/systemd/system/
│   ├── network-reconfigure.service  → /etc/systemd/system/
│   ├── network-fallback.service     → /etc/systemd/system/
│   └── wpa_supplicant@.service.d/
│       └── override.conf            → /etc/systemd/system/...
├── udev/
│   └── 90-wired-8021x.rules         → /etc/udev/rules.d/
├── migrate.sh                       # Migration/rollback script
├── rollback/                        # Pre-iwd hooks, kept for `migrate.sh rollback`
└── README.md                        # This file
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
