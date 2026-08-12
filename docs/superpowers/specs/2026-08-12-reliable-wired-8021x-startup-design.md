# Reliable Tencent Wired 802.1X Startup Design

## Goal

Start wired EAP-TLS authentication whenever the registered Tencent Ethernet device appears, including
cold boot, so networkd obtains the post-authentication lease before SmartGate scene and SSH policy are
needed.

## Root cause

The installed udev rule adds `SYSTEMD_WANTS=wpa_supplicant@%k.service` during a net-device `add`
event. On the failing boot, the renamed interface reached networkd and obtained the pre-authentication
`10.76.76.210/26` lease, but no wpa_supplicant unit invocation, process, control socket, or EAP event
existed. IOA's independent network-location API remained reachable and reported `inner net`, while
SmartGate's `11.176.17.58` scene endpoint and `9.134.215.72:36000` were unreachable. SmartGate stayed
`UNKNOWN` and handled the SSH proxy request as `Direct`.

## Design

Add a `[Unit]` and `[Install]` drop-in for the existing `wpa_supplicant@.service` template:

```ini
[Unit]
BindsTo=sys-subsystem-net-devices-%i.device
After=sys-subsystem-net-devices-%i.device

[Install]
WantedBy=
WantedBy=sys-subsystem-net-devices-%i.device
```

The empty `WantedBy=` resets the vendor template's `multi-user.target` install target. Enabling the
concrete instance then creates only a device-unit wants link. `BindsTo=` plus `After=` stops the
service when the USB NIC disappears; reinsertion activates the device unit and starts a fresh EAP
session. This avoids a failed boot-time service when the removable NIC is absent.

`restore.sh` installs the drop-in, reloads systemd, and enables the instance without `--now`, preserving
the current network during ordinary deployment. Live repair explicitly starts the instance, waits for
EAP success, then cycles only the networkd configuration for that interface to discard the
pre-authentication lease. SmartGateAgent remains owner of its scene, tables, and proxy policy.

## Verification

Static checks require the exact `[Install]` dependency and exact instance enable command, reject
`--now`, and preserve `-D wired`. Live verification requires EAP success, the authenticated lease,
SmartGate `INTRA`, policy download, and successful `ssh` proxy negotiation to `9.134.215.72:36000`.
No Tailscale operation or static `9/8` route is added.
