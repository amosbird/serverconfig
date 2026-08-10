# CN Gateway Change Batch Promotion Fix Design

## Goal

Make a CN route rebuild converge when an existing prefix changes gateway, restoring China DNS after
Wi-Fi or AP changes.

## Root cause

The batched promotion first emits `route replace` for every desired route, then computes stale routes
by comparing complete kernel-rendered route strings. When a prefix exists in both sets with a new
gateway, `route replace` already removes the old route. The later stale delete still targets that old
full route shape, receives `RTNETLINK answers: No such process`, aborts the batch, and leaves
`cn-last-applied` unchanged.

The active table can contain the desired routes after this failure, but the batch reports failure and
subsequent transitions may retain a stale table. In the observed incident, China DNS upstreams still
resolved through the previous Wi-Fi gateway and timed out.

## Design

Staleness is determined by route destination key, not the complete route string:

1. snapshot normalized desired and current routes;
2. emit all desired routes as `route replace ... table cn` first;
3. extract the first field (destination prefix or `default`) from each desired route into a sorted key
   set;
4. append `route del` only for current routes whose destination key is absent from the desired key
   set;
5. execute one promotion batch;
6. retain final full-route set equality and state-write validation.

A current route with the same destination but a different gateway is updated exclusively by
`replace`; it is not deleted afterward. A current prefix absent from the desired set is still deleted.

The routefile grammar produces one desired route per destination. Add an explicit duplicate-prefix
validation before staging so destination keys are unambiguous and a malformed routefile cannot hide
one desired route behind another.

## Failure semantics and ownership

Desired replacements remain before stale deletions. A netlink failure still aborts without advancing
`cn-last-applied`, and the next reconciliation retries. Only `cn_stage` and `cn` are changed; owner
marks, tables, services, and Tailscale preferences remain untouched.

## Testing

A namespace regression test starts with desired prefixes installed through gateway A, changes the
physical gateway to B, and verifies one forced rebuild:

- exits successfully;
- leaves every desired prefix via B;
- emits no stale delete for a prefix already replaced;
- removes a separate prefix absent from the desired routefile;
- advances CN state only after final equality;
- preserves tunnel-owner state.

Static checks continue to prohibit per-route promotion processes. Add duplicate-destination input to
the routefile rejection matrix.

## Deployment

After tests pass, deploy via the repository symlink and run one `FORCE_CN=1` reconciliation on the
current WLAN. Verify table `cn` uses the current main gateway, China DNS upstreams resolve through
`cn`, and `baidu.com` resolves through local SmartDNS. No network or tunnel service is restarted.
