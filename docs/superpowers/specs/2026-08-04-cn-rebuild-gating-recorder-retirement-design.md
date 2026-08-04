# CN Rebuild Gating and Recorder Retirement Design

## Goal

Keep lightweight network policy repair automatic while ensuring Tailscale, tun0, and unrelated
link events never rewrite a healthy CN route table. Retire the constant packet recorder and its
ring while preserving the bounded manual incident-capture tool.

## Confirmed problem

`network-reconfigure.path` watches the whole `/run/systemd/netif/links` directory. Changes to any
networkd-visible interface can start `network-reconfigure`, including `tailscale0` creation and
removal. The reconciler's global early-exit check couples all policy health to CN maintenance: if
any rule, firewall object, SmartDNS fragment, or state fingerprint needs repair, it stages and
synchronizes the complete routefile even when the routefile, physical device, gateway, and CN table
are unchanged.

With a large routefile this produces thousands of rtnetlink notifications, long reconciler runs,
and observed tailscaled netlink receive-buffer overflows. The path trigger may remain broad for
cheap repair, but heavy CN work needs an independent decision.

## CN build state

Add `/var/lib/network-reconfigure/cn-last-applied`, separate from the existing lightweight
`last-applied` state. It records only the inputs and result required to trust the active CN table:

```text
routefile=<sha256-or-missing> dev=<physical-device> gw=<physical-gateway> count=<expected-count>
```

The file is written atomically only after the staging table and active table exactly match the
parsed routefile for the current physical path.

A CN rebuild is required when any of these conditions holds:

1. `FORCE_CN=1` was explicitly requested;
2. the routefile hash changed;
3. the physical device changed;
4. the physical gateway changed;
5. the state file is missing or malformed;
6. the active CN route count differs from the recorded expected count;
7. a non-empty routefile has no active `cn` rule or routes;
8. a missing or empty routefile still has an active CN rule or routes.

Ordinary `FORCE=1` remains a request to reconcile lightweight repository policy. It does not force
CN replacement. This keeps `netfix` safe when only a rule or firewall invariant has drifted.

## Reconciliation flow

The reconciler still derives current physical LAN, gateway, DNS, routefile hash, desired rules,
SmartDNS fragments, firewall policy, and ipsets on every invocation. It then evaluates CN state
independently:

- If CN state is current, it does not flush `cn_stage`, execute a route batch, or add, replace, or
  delete any route in `cn`.
- If the routefile is missing or empty and stale CN policy exists, it removes the CN rule before
  flushing the active and staging tables, then records count zero.
- If a non-empty routefile needs rebuilding, it uses the existing strict parser and staging
  validation. The active table is changed only after staging succeeds. The new CN state is recorded
  only after the active table exactly matches staging.
- If no physical gateway is available, existing CN routes and state remain untouched. Lightweight
  policy can still converge around the currently available physical state.

The priority-1500 rule is derived after the CN decision so the same run reflects the final active
CN table. All other route/rule owners remain untouched.

## Triggering

Keep the existing systemd path unit. A virtual-interface event may start the oneshot service, but a
healthy state must complete quickly and produce zero CN route notifications. Avoiding a new event
dispatcher keeps the design small and preserves automatic repair when any owned lightweight object
drifts.

The runnable namespace checks will model a Tailscale-style unrelated link event by running the
reconciler again with unchanged physical and routefile inputs and altered foreign owner state. They
will assert that snapshots of both `cn` and `cn_stage` are byte-identical and that the instrumented
`ip` wrapper observes no CN route mutation.

Additional checks cover routefile change, gateway/device change, missing CN state, active count
drift, `FORCE=1`, and `FORCE_CN=1`.

## Constant recorder retirement

Remove the constant packet recorder from repository deployment:

- delete `network/systemd/network-debug-pcap.service`;
- remove its install/enable/restart checks from `restore.sh`;
- make `restore.sh` disable and remove an installed legacy recorder unit, reload systemd, and remove
  only `/var/log/network-debug/ring`;
- leave `/var/log/network-debug/incidents` intact;
- remove transient `/var/log/network-debug/pre-up-*` and `/run/network-debug-pre-up-*` during the
  live cleanup, not as recurring boot-time policy.

`restore.sh` remains the source of truth: running it on a machine with the old recorder converges to
no constant capture service.

## Manual incident capture

Keep `scripts/network-debug-capture` as an explicitly invoked, bounded diagnostic tool. Simplify it
to:

1. take the before snapshot;
2. start bounded 30-second deep captures on `wlan0` and `tailscale0` when available;
3. wait with existing timeout semantics;
4. take the after snapshot;
5. retain the newest five incidents;
6. upload a Tailscale bug report only with explicit `--bugreport`.

It no longer queries, stops, restores, or copies `network-debug-pcap.service` or a packet ring.
Capture cleanup remains signal-safe and bounded. No code may run `tailscale up`, `tailscale down`,
change exit-node preferences, or restart network services.

## Error handling

- CN state is never advanced after parser, staging, active-table, or verification failure.
- An invalid state file forces a rebuild rather than being trusted.
- State writes use a temporary file beside the target followed by `mv`.
- Recorder retirement tolerates an absent unit and absent ring.
- Manual capture preserves partial evidence and returns nonzero when setup or snapshot operations
  fail according to its existing manifest contract.
- No fault injection is performed on the live network.

## Tests

### CN gating

Extend `network/test-reconfigure.sh` with instrumentation around route mutations and verify:

- unchanged physical path and routefile plus unrelated owner/link change: zero `cn` and `cn_stage`
  mutations;
- ordinary `FORCE=1`: zero CN mutations when CN state is healthy;
- `FORCE_CN=1`: rebuild occurs;
- changed routefile: rebuild occurs and state hash/count update;
- changed physical gateway/device: rebuild occurs;
- missing/malformed CN state: rebuild occurs;
- route-count drift: rebuild occurs;
- missing/empty routefile still disables stale CN policy;
- failed rebuild leaves the prior CN state record unchanged.

### Recorder retirement

Update `network/test-debug-capture.sh` and static policy checks to verify:

- the constant recorder unit is absent;
- restore retires the installed legacy unit before daemon reload and never enables it;
- restore removes only the ring, not incidents;
- the manual tool contains no recorder service lifecycle calls or ring-copy dependency;
- bounded deep capture, manifest, retention, concurrent-lock, signal cleanup, and no-tunnel-mutation
  contracts remain green.

## Live deployment

After offline tests pass:

1. install the updated path/service/script files through repository-managed locations;
2. do not restart Tailscale, SmartGateAgent, networkd, or iwd;
3. disable/remove `network-debug-pcap.service` and delete its ring;
4. remove transient pre-up capture files and markers;
5. force one explicit CN rebuild only if the new CN state needs seeding;
6. record the resulting state;
7. trigger a harmless subsequent reconciler run and prove it emits zero CN route events;
8. confirm the current direct Tailscale exit-node path and preferences are unchanged.

## Non-goals

- No change to the CN routefile semantics or its priority over repository IOA business rules.
- No conversion of CN routes to ipset/nftables.
- No Tailscale restart or exit-node preference mutation.
- No filtering or ownership of SmartGateAgent tables 20/230 or Tailscale table 52.
- No removal of the manual incident tool or retained incidents.
