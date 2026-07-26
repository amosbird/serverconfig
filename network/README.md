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
| exit node selected, broken | `ip rule pref 100 lookup main`, straight out the physical gateway |
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

## IOA / Company Network

IOA domain routing is preserved in `/etc/smartdns/ioa-domains.conf`.
Policy routing is managed by `netmode`:

```bash
netmode status          # show current state
netmode ioa on          # enable IOA routing (ipset + Tencent CIDRs → ioa table)
netmode ioa off         # disable IOA routing (clean removal)
netmode cn on           # enable China IP bypass (APNIC routes → physical gateway)
netmode cn off          # disable China IP bypass
netmode apply           # re-apply active modes after wifi reconnect
netmode reset           # remove ALL custom rules
```

State is tracked in `/run/netmode/` (volatile, resets on reboot).
Rules are idempotent: running `ioa on` twice produces the same result.
All iptables rules live in a dedicated `NETMODE_IOA` chain for clean removal.

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
│   ├── mode-{travel,ioa,office}.conf → /etc/smartdns/ (selected by `netmode`)
│   └── dhcp-dns.conf                → /etc/smartdns/ (seed; rewritten per link)
├── systemd/
│   ├── network-reconfigure.path     → /etc/systemd/system/
│   ├── network-reconfigure.service  → /etc/systemd/system/
│   ├── network-fallback.service     → /etc/systemd/system/
│   └── wpa_supplicant@.service.d/
│       └── override.conf            → /etc/systemd/system/...
├── udev/
│   └── 90-wired-8021x.rules         → /etc/udev/rules.d/
├── netmode                          # Policy routing mode controller
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
