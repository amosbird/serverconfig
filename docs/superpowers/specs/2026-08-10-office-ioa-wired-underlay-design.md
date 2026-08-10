# Office IOA DNS and Wired Underlay Advertisement Design

## Goal

When Tencent office wired Ethernet is authenticated, resolve IOA bootstrap names through office DNS
and advertise the wired DHCP gateway to SmartGateAgent without changing normal system routing.

## Root causes

The generated office SmartDNS fragment is active and its three internal resolvers are reachable, but
the base configuration explicitly maps IOA bootstrap suffixes to the `china` group. The later office
fragment adds servers to the `ioa` group without overriding those domain mappings, so bootstrap names
continue to resolve to public addresses.

The wired network intentionally suppresses its DHCP default route in `main`. This prevents office
Ethernet from becoming the machine-wide default path, but SmartGateAgent scans route tables for a
default route and therefore sees only Wi-Fi. The pre-iwd stack advertised the wired DHCP gateway in
table 19, with no policy rule pointing to that table. That hardware-specific calibration was deleted
as rollback legacy even though the current SmartGateAgent still depends on it.

## Office DNS override

While authenticated wired Ethernet has an IPv4 address, the generated office fragment additionally
contains:

```text
nameserver /smartgate.oa.tencent.com/ioa
nameserver /sgw.woa.com/ioa
nameserver /ioa.tencent.com/ioa
```

These exact mappings override the base `china` mappings only while the office fragment is installed.
The existing office `ioa` servers then return internal bootstrap addresses. When wired Ethernet goes
down, the fragment becomes the existing `# Not on the office LAN` content and the base public China
bootstrap behavior resumes.

No credentials or dynamically resolved endpoint addresses are stored in the repository.

## Table 19 ownership and behavior

The repository owns one exact default route in table 19:

```text
default via <wired DHCP gateway> dev <authenticated wired interface>
```

There is no `ip rule` that looks up table 19. Kernel policy routing for applications remains unchanged.
The route exists only for SmartGateAgent's all-table netlink discovery, allowing it to select wired
Ethernet as an underlay candidate. SmartGateAgent continues to own mark `0xa38`, tables 20 and 230,
`tun0`, and table `ioa`.

Table 19 is deliberately lower than SmartGateAgent's table 20, preserving the client behavior relied
on by the old working hook. It contains a physical gateway and cannot create tunnel recursion.

## Wired gateway discovery

`network-reconfigure` discovers an office wired candidate only when all of these hold:

- interface name matches the existing wired policy (`enp*`);
- carrier is up;
- the interface has a global IPv4 address;
- `networkctl dhcp-lease <interface>` reports an IPv4 router.

The selected candidate is deterministic: the first interface in lexical order. The repository
currently has one office wired interface; supporting simultaneous authenticated wired adapters is out
of scope.

The wired gateway is independent from `physical_gateway()`, which remains the `main` default used for
CN routes and general physical policy. Adding table 19 must not cause CN routes, DHCP DNS exceptions,
or Tailscale underlay policy to switch to wired.

## Reconciliation lifecycle

On every run under the existing lock:

1. derive the desired wired interface and DHCP gateway;
2. if both exist, `replace` the exact table-19 default;
3. delete stale repository-owned table-19 defaults that do not match the desired route;
4. if no valid candidate exists, remove repository-owned table-19 defaults;
5. never add an `ip rule` for table 19;
6. leave tables 20, 230, 52, and `ioa` byte-identical.

The table is registered as `19 wired_underlay` in `rt_tables` with the same collision checks used for
other fixed table registrations. Any conflicting name or ID fails closed before routing mutation.

## Failure handling

A missing wired router leaves table 19 empty and logs a warning; it does not guess from the subnet.
An office DNS fragment update continues to use the existing atomic write and SmartDNS restart path.
A table-19 netlink failure aborts reconciliation and does not record successful state.

The repository does not restart SmartGateAgent. Existing public SmartGateAgent connections persist
until its natural reconnect or an explicit operator restart.

## Testing

Namespace tests verify:

- wired DHCP router parsing;
- table 19 contains exactly one desired wired default when wired is up;
- table 19 is emptied when wired goes down;
- a gateway change replaces the old advertisement;
- no rule references table 19;
- CN routes remain on the normal `main` physical gateway;
- owner tables and rules remain byte-identical;
- fixed table ID/name conflicts fail before mutation;
- office DNS mappings appear only while wired is up;
- local SmartDNS bootstrap queries use the office `ioa` group configuration.

Static tests prohibit table-19 rules and mutation of SmartGateAgent/Tailscale-owned tables.

## Deployment and live verification

Deploy the reconciler and office fragment, then run one normal reconciliation. This may atomically
rewrite the office fragment and restart SmartDNS as already designed; it does not restart networkd,
iwd, Tailscale, or SmartGateAgent.

Verify read-only:

- table 19 advertises the wired gateway;
- no rule looks up table 19;
- `dig @127.0.0.1` returns internal bootstrap addresses;
- CN routes still use the Wi-Fi/main default gateway;
- owner tables remain unchanged.

SmartGateAgent is expected to move from public Wi-Fi endpoints only after it reconnects.
