# SmartGate Office Bootstrap DNS Design

## Goal

Use Tencent's internal SmartGate bootstrap endpoints only while authenticated office Ethernet is
available, so SmartGateAgent selects the office scene instead of reporting `EXTRA` (external network).
Keep public bootstrap behavior everywhere else.

## Corrected root cause

The previous design attributed the failed internal bootstrap to a policy-download deadlock. That
conclusion was confounded by the wired adapter using an unregistered MAC address. With the wrong MAC,
wired EAP and DHCP appeared partially functional, but connections from the wired address to internal
bootstrap endpoints timed out.

After restoring the registered wired MAC `08:3a:88:5a:b5:37`, wired EAP-TLS and DHCP complete, and the
internal endpoints returned by office DNS are directly reachable on TCP port 443:

```text
smartgate.oa.tencent.com    11.176.17.58    reachable
ioa.tencent.com             10.88.202.158   reachable
ioav5-policy.ioa.tencent.com 10.99.245.241  reachable
```

The current public resolution sends SmartGateAgent to `61.241.57.22`; the server then returns
`sceneID:2, sceneName:EXTRA`. The UI's external-network status is therefore consistent with the public
bootstrap path, not with an interface, table 19, or MAC-selection failure.

## Design

Restore these mappings in `network/smartdns/office.conf`:

```text
nameserver /smartgate.oa.tencent.com/ioa
nameserver /sgw.woa.com/ioa
nameserver /ioa.tencent.com/ioa
```

The existing network reconciler installs this fragment only when an addressed wired `enp*` interface
and its DHCP router are present. When office Ethernet disappears, it empties the deployed office
fragment. Consequently:

```text
office wired present -> internal DNS bootstrap -> office scene
otherwise            -> base china-group bootstrap -> public/external scene
```

Do not hard-code endpoint IP addresses. Tencent's office DNS remains authoritative for their current
addresses.

## Ownership boundaries

This correction changes DNS selection only. It does not change routing ownership:

- the repository continues to advertise the wired underlay through table 19;
- no policy rule is added for table 19;
- SmartGateAgent continues to own mark `0xa38`, tables 20/230, `tun0`, and IOA route lifecycle;
- Tailscale continues to own mark `0x80000`, table 52, and exit-node recovery;
- repository code does not restart or alter Tailscale.

## Tests

Update the static policy check first so it fails while the mappings are absent. It must assert:

1. all three bootstrap mappings exist in the office fragment and target the `ioa` group;
2. the base configuration still maps the same domains to `china`, preserving off-office fallback;
3. the deployment contract still activates and clears the whole office fragment based on wired
   presence rather than installing permanent internal mappings.

Run the existing namespace, static-policy, IP-override, debug-capture, syntax, and whitespace checks to
ensure the DNS-only correction does not cross routing ownership boundaries.

## Deployment and live verification

Run the existing network reconciliation path to atomically install `/etc/smartdns/office.conf` and
restart SmartDNS. Verify local SmartDNS returns the same internal bootstrap answers as the office DNS.

A fresh SmartGate scene query may require restarting `ngnclient`. Do that only as an explicit,
bounded verification step after DNS is correct; do not detach interfaces or alter Tailscale. Confirm:

1. SmartGateAgent uses the registered wired MAC and wired source address;
2. bootstrap connects to an internal office-DNS answer;
3. the selected scene is no longer `EXTRA`;
4. a known proxied business endpoint still succeeds.

If internal bootstrap remains reachable but the server still returns `EXTRA`, stop and investigate the
scene protocol rather than changing table 19 or adding broader routes.
