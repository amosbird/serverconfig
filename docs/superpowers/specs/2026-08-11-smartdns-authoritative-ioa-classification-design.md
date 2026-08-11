# SmartDNS-Authoritative IOA Classification Design

## Goal

Route every address derived from an IOA business-domain rule through SmartGateAgent's `tun0`,
regardless of its IP prefix, while preventing SmartGate bootstrap and proxy transport connections
from being routed into the tunnel they establish.

## Root cause

SmartDNS correctly resolves `token.woa.com` through the `ioa` DNS group and adds its current answer,
`21.34.11.74`, to dynamic `ipset ioa`. The repository currently marks an address only when it belongs
to both `ioa` and the static `ioa_intranet` set. That second set contains only `9/8`, `10/8`, and
`100.12/16`, so the valid `21.x` business result remains unmarked and falls through to Tailscale's
exit-node catch-all.

Observed behavior confirms the boundary is the defect:

```text
token.woa.com -> 21.34.11.74
21.34.11.74 is in ipset ioa
21.34.11.74 is not in ipset ioa_intranet
normal connection -> tailscale0 -> timeout
connection forced through tun0 -> HTTP/2 302, x-proxy-by: AIO-Forward
```

## Design

Make dynamic `ipset ioa` authoritative for domain-derived IOA business classification. Remove
`ioa_intranet` and its static network whitelist. The owned mangle chain becomes:

```text
packet already has any nonzero mark -> return unchanged
destination is in ipset ioa         -> set exact full-width mark 0x1
otherwise                           -> return unmodified
```

The existing priority-2500 exact-mark rule then selects table `ioa`. Static `10/8` and `100.12/16`
rules remain because they are separately declared business ranges. No static `9/8` or `21/8` route is
added.

### Transport exclusions

Use SmartDNS's native, more-specific domain override instead of maintaining a set of drifting IP
addresses. Keep broad business rules such as:

```text
ipset /woa.com/ioa
```

and add exact suffix exclusions:

```text
ipset /sgw.woa.com/-
ipset /smartgate.oa.tencent.com/-
ipset /ioa.tencent.com/-
```

SmartDNS documents `ipset /domain/-` as the way to ignore an ipset rule for that domain. An isolated
local SmartDNS test using the installed version verified specificity and inheritance:

```text
token.woa.com                     -> added to ioa
aio-c1-tkeprod.sgw.woa.com        -> not added to ioa
sgw.woa.com                       -> not added to ioa
smartgate.oa.tencent.com          -> not added to ioa
ioa.tencent.com                   -> not added to ioa
```

The `/sgw.woa.com/-` rule deliberately covers both the base name and all SmartGate proxy subdomains.
`ioa.tencent.com` remains reachable directly through its office-internal answer while wired office
DNS is active, and through the base public bootstrap path otherwise.

## Priority and ownership boundaries

This change preserves existing ownership:

- repository-owned priority 1500 `cn` lookup remains before priority 2500, so `~/.routefile` remains
  authoritative even for an address in `ipset ioa`;
- an actual connected LAN, physical gateway, and DHCP resolvers remain priority-1000 exceptions;
- the chain returns all nonzero marks unchanged, preserving SmartGateAgent mark `0xa38`, Tailscale
  mark `0x80000`, and unknown foreign marks;
- SmartGateAgent continues to own `tun0`, table `ioa`, tables 20/230, and proxy policy;
- Tailscale continues to own table 52 and exit-node recovery;
- the repository does not discover, cache, route, restart, or reattach either tunnel's transport
  endpoints.

## Failure behavior

If SmartDNS classifies an ordinary business domain, its answer enters `ioa` and receives mark `0x1`.
If `tun0` or table `ioa` is unavailable, the existing table lookup behavior remains fail-closed for
that classified business traffic.

Transport safety depends on the explicit domain exclusions rather than guessed IP ranges. A transport
hostname added under a new suffix must be added to this exclusion list before a broad business ipset
rule covers it. Static checks therefore protect the known bootstrap and proxy suffixes.

## Tests

Follow TDD by first changing namespace and static tests so the current implementation fails. Verify:

1. an arbitrary address such as `21.34.11.74` in `ipset ioa` receives exact mark `0x1` and selects
   table `ioa`;
2. the same address outside `ipset ioa` remains unmarked and follows unmatched policy;
3. a nonzero owner mark remains unchanged even when the destination is in `ioa`;
4. routefile priority still overrides a mark-derived IOA route;
5. `ioa_intranet` is no longer created, referenced, or documented as active policy;
6. SmartDNS contains exactly one `/-` exclusion for each of `sgw.woa.com`,
   `smartgate.oa.tencent.com`, and `ioa.tencent.com`;
7. the broad `/woa.com/ioa` rule remains, proving the fix does not special-case `token.woa.com`;
8. no static `21/8` or `9/8` IOA route is introduced.

Run the full namespace suite, static policy suite, IP override test, debug-capture test, shell syntax
checks, and `git diff --check`.

## Deployment and live verification

Use the existing network reconciliation and safe SmartDNS deployment paths. Do not restart
Tailscale. After deployment:

1. query `token.woa.com` and confirm its answer enters `ipset ioa`;
2. query excluded transport names and confirm their answers do not enter `ipset ioa` after starting
   from a flushed dynamic set;
3. confirm `token.woa.com` no longer emits SYN packets on `tailscale0` and returns through SmartGate;
4. confirm SmartGate remains `sceneName:INTRA`, policy download remains successful, and a known
   business endpoint still works;
5. confirm tables 19/20/230 and Tailscale-owned table 52 remain owner-managed and unchanged in
   structure.

If a transport exclusion test fails, roll back the SmartDNS and marking changes together rather than
adding an IP-prefix exception.
