# SmartGate Bootstrap DNS Deadlock Fix Design

## Goal

Keep the wired underlay advertisement while ensuring SmartGateAgent can reach public bootstrap
endpoints before it has downloaded proxy policy.

## Root cause

The office fragment overrode SmartGate bootstrap names to the internal `ioa` DNS group. Those names
then resolved to internal addresses such as `11.176.17.58`. SmartGateAgent must contact bootstrap and
policy services before proxy policy is available, so it treated those addresses as Direct and tried to
reach them over wired Ethernet. They were unreachable without the policy being downloaded, creating
a bootstrap deadlock. Subsequent business destinations also fell back to Direct because no proxy
scene was loaded.

Historical logs show the working sequence: public bootstrap connectivity first, then `Use proxy:tls`
for business `9.x` targets.

## Design

Remove these mappings from the office fragment:

```text
nameserver /smartgate.oa.tencent.com/ioa
nameserver /sgw.woa.com/ioa
nameserver /ioa.tencent.com/ioa
```

Retain the base mappings to the `china` group, the office internal DNS servers, and table 19. This
restores:

```text
public bootstrap DNS -> wired Direct bootstrap -> policy download -> TLS proxy for business targets
```

Static and namespace tests assert that office configuration never overrides bootstrap domains and
that the base configuration maps each one exactly once to `china`.

## Deployment

Reconcile once to atomically replace `/etc/smartdns/office.conf` and restart SmartDNS through the
existing path. Verify bootstrap names return public addresses. The repository does not restart
SmartGateAgent; policy recovery is observed naturally. If the existing client does not retry within a
bounded observation window, an explicit operator restart requires separate approval.
