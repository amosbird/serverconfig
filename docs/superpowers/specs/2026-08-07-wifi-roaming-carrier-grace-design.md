# Wi-Fi Roaming Carrier Grace Design

## Goal

Preserve the physical DHCP configuration across short iwd BSSID roaming gaps so Tailscale does not
lose its underlay when moving between access points of the same SSID.

## Root cause

iwd performs normal roaming between access points. During the BSSID switch, `wlan0` briefly loses
carrier. The current systemd-networkd wireless files use the default `IgnoreCarrierLoss=no`, so
networkd immediately discards the DHCP lease, address, connected route, and physical default route.
It reacquires the same lease seconds later, but tailscaled observes an intermediate
`defaultRoute=""`, closes established direct and DERP paths, and can fail to establish working
replacement paths on the venue network.

systemd-networkd explicitly documents finite `IgnoreCarrierLoss=` intervals for wireless networks
with multiple access points sharing one SSID.

## Configuration

Add to both repository-managed wireless network files:

```ini
[Network]
IgnoreCarrierLoss=3s
```

Files:

- `network/systemd-network/25-wireless.network`;
- `network/systemd-network/26-wireless-tencent.network`.

The existing keys in each `[Network]` section remain unchanged. A BSSID switch that regains carrier
within three seconds retains dynamic interface configuration. If carrier remains absent longer,
networkd expires the grace period and performs the normal DHCP teardown, avoiding stale addresses
and gateways on a real network change.

The value is deliberately finite. `yes` or `infinite` is forbidden because it could preserve an old
lease indefinitely after leaving a network.

## Ownership and deployment

No new daemon or hook is added. `restore.sh` already installs all repository `.network` files, so it
remains the source of truth.

Live deployment copies the two files to `/etc/systemd/network/` and runs:

```text
networkctl reload
```

It must not restart networkd, iwd, tailscaled, SmartGateAgent, or SmartDNS. Existing connectivity
and Tailscale preferences remain untouched.

## Testing

Static checks verify:

- both wireless network files contain exactly one `IgnoreCarrierLoss=3s`;
- neither contains `IgnoreCarrierLoss=yes` or `IgnoreCarrierLoss=infinite`;
- restore continues to deploy the wireless network files;
- the installed effective configuration matches the repository after deployment.

No artificial carrier loss or destructive roam test is performed on the live network. The next
natural room-to-room roam is verified through journal evidence:

- iwd reports a BSSID roam;
- carrier may briefly drop and return;
- networkd does not report `DHCP lease lost` when carrier returns within three seconds;
- tailscaled does not observe `defaultRoute=""` for that roam;
- the exit node remains direct or recovers without manual `tailscale down`.

## Non-goals

- No disabling of iwd roaming.
- No forced disconnect/reconnect after every BSSID change.
- No changes to CN, IOA, or Tailscale policy routing.
- No mutation of Tailscale exit-node preferences.
