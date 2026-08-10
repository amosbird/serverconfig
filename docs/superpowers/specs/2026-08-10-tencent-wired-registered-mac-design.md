# Tencent Wired Registered MAC Design

## Goal

Restore the registered Tencent wired identity that the pre-networkd stack applied before DHCP and
SmartGateAgent underlay discovery.

## Root cause

The old netctl interface hook assigned `08:3a:88:5a:b5:37` to the office Ethernet adapter. The new
stack omitted this hardware calibration and uses the adapter's permanent `00:0e:c6:5d:e7:88`.
Authentication and DHCP still succeed, but public SmartGate bootstrap connections bound to wired
receive no reply, preventing proxy policy download.

## Design

Install an early systemd `.link` file matching the ASIX AX88179 adapter's stable USB path and assign:

```ini
[Link]
MACAddress=08:3a:88:5a:b5:37
```

The path match avoids applying the registered address to unrelated Ethernet devices. `restore.sh`
installs the `.link` file. A static test verifies the exact path, MAC, and ordering before
`99-default.link`.

Apply once with a controlled wired link replug/reload sequence while Wi-Fi remains connected. Do not
restart Tailscale or SmartGateAgent until wired DHCP is restored. Then restart `ngnclient` once so it
performs bootstrap using the corrected wired identity.

## Verification

Verify the wired link and DHCP lease both expose the registered MAC, table 19 remains the wired
underlay advertisement, public bootstrap TCP succeeds from the wired source, SmartGateAgent loads
policy, and a known business `9.x` target uses `proxy:tls` and becomes reachable.
