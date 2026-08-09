# CN Route Batch Promotion Design

## Goal

Reduce rebuilding roughly 8,800 CN routes from several seconds to a small number of batched netlink
operations without changing routing policy or ownership.

## Root cause

The staging table is already populated with one `ip -batch` invocation. After validation, however,
`network-reconfigure` promotes every staged route into table `cn` by launching a separate
`ip route replace` process. It also checks every old route with a full-string `grep` against the
entire desired set before deleting it.

An isolated namespace measurement with 8,789 routes showed:

```text
staging ip -batch:          0.068 seconds
current per-route promotion: 8.670 seconds
batched promotion:           0.071 seconds
```

The main cost is 8,789 process launches. The stale-route check also has an unnecessary O(n²) text
comparison ceiling.

## Design

Keep the existing parse, staging, and route-count validation. Once `cn_stage` is valid:

1. normalize and sort the desired routes from `cn_stage`;
2. normalize and sort the routes currently in `cn`;
3. generate one promotion batch containing every desired route as
   `route replace ... table cn`;
4. append `route del ... table cn` only for current routes absent from the desired set, using `comm`
   over the sorted snapshots;
5. execute the complete promotion with one `ip -batch` invocation;
6. compare the final normalized `cn` set with the desired set;
7. write `cn-last-applied` only after that comparison succeeds.

Desired replacements precede stale deletions in the batch, preserving the existing add-before-delete
failure behavior. `ip -batch` is intentionally not run with `-force`: the first netlink failure stops
the batch, the final state is not recorded, and the next reconciliation retries. As today, a mid-batch
failure can leave a partially updated active table but is visible and does not claim convergence.

The batch content is held in a temporary file rather than a shell argument or one command per route.
The temporary file is removed on every exit path.

## Scope and ownership

This changes only repository-owned tables `cn_stage` and `cn`. It does not change:

- routefile authority or grammar;
- priorities 500, 1000, 1500, 2500, or 3000;
- SmartGateAgent tables `20`, `230`, or `ioa`;
- Tailscale table `52`, marks, exit-node preference, or recovery;
- CN rebuild gating or `FORCE_CN` semantics.

## Testing

Extend the namespace test to create a large synthetic routefile and verify:

- staging uses one `ip -batch` call;
- promotion uses one additional `ip -batch` call;
- no per-route `ip route replace ... table cn` process is invoked;
- all desired routes exist in `cn`;
- stale routes are removed;
- desired routes are emitted before stale deletions;
- a rejected promotion batch leaves `cn-last-applied` unchanged;
- tunnel-owner snapshots remain byte-identical.

Keep the existing malformed staging and final set-equality tests. Add a static rejection for a
per-route promotion loop so the performance regression cannot return silently.

## Deployment

Deploy the updated `scripts/network-reconfigure` through the repository symlink. No service restart is
required. A subsequent routefile change, gateway change, count drift, or explicit `FORCE_CN=1` uses
the batched path. Do not force another rebuild merely to benchmark the live network; namespace tests
provide the destructive performance check.
