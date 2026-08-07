# Wi-Fi Roaming and Tencent Android MAC Design

## Goal

Preserve physical DHCP configuration across short iwd BSSID roaming gaps, and use the registered
Android MAC only when connecting to `Tencent-WiFi`.

## Root causes

### Short roaming gaps tear down the underlay

iwd performs normal roaming between access points. During a BSSID switch, `wlan0` briefly loses
carrier. The current systemd-networkd wireless configuration uses the default
`IgnoreCarrierLoss=no`, so networkd immediately discards the DHCP lease, address, connected route,
and physical default route. It reacquires the same lease shortly afterward, but tailscaled observes
an intermediate `defaultRoute=""`, closes established direct and DERP paths, and may fail to establish
working replacement paths on the venue network.

systemd-networkd explicitly documents finite `IgnoreCarrierLoss=` intervals for wireless networks
with multiple access points sharing one SSID.

### The old Android MAC hook is no longer active

The former netctl interface hook `/etc/netctl/interfaces/wlp0s20f3` used `macchanger` to assign
`1e:dc:46:00:66:1b`. Because it was an interface-level hook, it affected every network managed
through that interface. The current iwd stack does not execute the hook, while the intended policy is
to use this address only for `Tencent-WiFi`.

The repository's `26-wireless-tencent.network` is also ineffective: systemd-networkd selects the
first matching `.network` file, so the earlier generic `25-wireless.network` wins. Its no-gateway
policy additionally conflicts with the required behavior that `Tencent-WiFi` be a normal physical
network.

## Configuration

### Retain DHCP state during short wireless roaming

Add the following to `network/systemd-network/25-wireless.network`:

```ini
[Network]
IgnoreCarrierLoss=3s
```

A BSSID switch that regains carrier within three seconds retains dynamic interface configuration. If
carrier remains absent longer, networkd expires the grace period and performs normal DHCP teardown,
avoiding stale addresses and gateways after a real network change.

The value is deliberately finite. `yes` and `infinite` are forbidden because either could preserve an
old lease indefinitely after leaving a network.

### Let Tencent-WiFi use normal physical routing

Delete `network/systemd-network/26-wireless-tencent.network` and remove any legacy installed copy.
`Tencent-WiFi` then matches `25-wireless.network` and receives the same normal DHCP gateway and
routes as other Wi-Fi networks. Existing repository policy still keeps the actual LAN and CN routes
on the physical network while unmatched default traffic uses the Tailscale exit node.

### Use iwd's per-network MAC override

Add to repository-managed `network/iwd/main.conf`:

```ini
[General]
AddressRandomization=network
```

Under this mode, ordinary Wi-Fi networks use a stable MAC derived by iwd from the SSID and the
adapter's permanent address.

Add to the existing `[Settings]` section of the live secret profile
`/var/lib/iwd/Tencent-WiFi.8021x`:

```ini
AddressOverride=1e:dc:46:00:66:1b
```

iwd applies this override before associating with `Tencent-WiFi`. When connecting to another SSID,
it switches to that network's stable generated MAC instead of retaining the Android address.

The `.8021x` profile contains credentials and remains machine-managed secret material. Its
certificates, private key, passphrase, and identity must never be copied into the repository or test
output. Repository deployment reports whether the required setting is present but does not install or
replace this profile.

## Ownership and deployment

No new daemon, hook, or repair script is added. iwd owns association-time MAC selection, and
systemd-networkd owns DHCP carrier grace.

Repository deployment:

1. installs `network/iwd/main.conf` and repository `.network` files;
2. removes the obsolete `/etc/systemd/network/26-wireless-tencent.network`;
3. reports whether the live Tencent profile has the required `AddressOverride`, without printing
   profile contents;
4. runs `networkctl reload` for the networkd change.

The one-time Tencent profile edit is a separate local administration step, not part of `restore.sh`
or `migrate.sh`. It preserves the file's mode and ownership, creates a root-only backup in the same
directory, and atomically adds or replaces exactly one `AddressOverride` in the existing `[Settings]`
section. It must refuse to operate if the profile is missing, is not a regular file, or lacks that
section.

Deployment must not restart networkd, iwd, tailscaled, SmartGateAgent, or SmartDNS, and must not
force a disconnect. `AddressRandomization=network` takes effect when iwd next starts naturally. The
Tencent profile can be edited without disconnecting the current non-Tencent network; its behavior is
verified on the next natural connection to `Tencent-WiFi`. A missing or unmodified optional Tencent
profile produces a warning during general repository deployment rather than breaking unrelated
network installation.

Rollback removes repository-managed networkd files as before, including any obsolete Tencent file.
It does not modify or delete `/var/lib/iwd/Tencent-WiFi.8021x` because rollback must not mutate
pre-existing secret material.

## Error handling

The separate local profile edit fails before replacing the file if it cannot preserve mode and
ownership or cannot produce a valid profile containing exactly one override. Static validation
reports only the profile path and pass/fail status; it never emits profile contents. No failure path
restarts a network service or changes Tailscale state.

## Testing

Static tests verify:

- `network/iwd/main.conf` contains exactly one `AddressRandomization=network`;
- `25-wireless.network` contains exactly one `IgnoreCarrierLoss=3s` and no infinite equivalent;
- `26-wireless-tencent.network` is absent and install logic removes a legacy live copy;
- deployment validation recognizes a synthetic `.8021x` fixture with exactly one Android
  `AddressOverride`, warns when it is missing, and does not print synthetic credential values;
- rollback and general deployment leave the `.8021x` profile contents untouched;
- no Tencent credential profile exists under repository ownership.

No artificial carrier loss, forced reconnect, or destructive roam test is performed on the live
network.

On the next natural connection to `Tencent-WiFi`, verify:

- the association MAC is `1e:dc:46:00:66:1b`;
- DHCP installs the physical address, connected route, and default gateway;
- systemd-networkd uses `25-wireless.network`;
- connecting to another SSID stops using the Android MAC.

On the next natural room-to-room roam, verify through journal evidence:

- iwd reports a BSSID roam;
- networkd does not report `DHCP lease lost` when carrier returns within three seconds;
- tailscaled does not observe `defaultRoute=""` for that short roam;
- the exit node remains usable or recovers without `tailscale down`.

## Non-goals

- No disabling of iwd roaming.
- No forced disconnect or reconnect after a BSSID change.
- No MAC-changing event listener or custom service.
- No changes to CN, IOA, or Tailscale policy routing.
- No mutation of Tailscale exit-node preferences.
- No repository ownership of Tencent certificates, keys, identity, or passphrase.
