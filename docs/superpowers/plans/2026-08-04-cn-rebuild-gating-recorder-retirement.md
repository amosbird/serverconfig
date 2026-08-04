# CN Rebuild Gating and Recorder Retirement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prevent healthy CN tables from being rewritten on unrelated link events and retire constant packet capture while retaining bounded manual incident capture.

**Architecture:** Give CN routes an independent, atomically written state fingerprint and gate all CN table mutations behind explicit rebuild conditions. Delete the constant recorder unit and remove the manual tool's ring/service lifecycle, leaving snapshots plus bounded on-demand pcaps.

**Tech Stack:** Bash, iproute2, systemd, Linux network namespaces, tcpdump

---

### Task 1: Specify CN Route Mutation Gating

**Files:**
- Modify: `network/test-reconfigure.sh`

- [ ] Add a test-only `ip` wrapper that logs `route flush|add|replace|del` operations targeting tables `cn` and `cn_stage`, while forwarding every command to the real `ip`.
- [ ] Add assertions that a healthy second run and `FORCE=1` produce an empty CN mutation log.
- [ ] Add assertions that `FORCE_CN=1`, routefile changes, gateway changes, missing/malformed CN state, and active count drift produce CN mutations.
- [ ] Assert a failed staging run leaves `cn-last-applied` byte-identical.
- [ ] Run `sudo -n bash network/test-reconfigure.sh`; expect the new no-mutation assertions to fail because production currently rebuilds CN on every non-early-exit reconciliation.
- [ ] Commit the failing specification.

### Task 2: Implement Independent CN State

**Files:**
- Modify: `scripts/network-reconfigure`
- Test: `network/test-reconfigure.sh`

- [ ] Add `CN_STATE_FILE="$CACHE_DIR/cn-last-applied"` after cache initialization.
- [ ] Parse the routefile once to derive strict expected count and hash without mutating routes.
- [ ] Define the expected CN state as routefile hash, physical device, gateway, and expected count.
- [ ] Trust current CN state only when the state file matches exactly, active count matches, route/rule presence matches empty/non-empty intent, and `FORCE_CN` is not set.
- [ ] Skip every CN/staging mutation when trusted, regardless of `FORCE=1` or other policy drift.
- [ ] For missing/empty routefiles, disable stale CN policy and atomically record count zero.
- [ ] For a required non-empty rebuild, retain strict staging and active verification, then atomically write the CN state.
- [ ] Derive the priority-1500 desired band after CN processing.
- [ ] Run the focused namespace suite; expect all gating and existing 101 checks to pass.
- [ ] Commit the implementation.

### Task 3: Specify Recorder Retirement

**Files:**
- Modify: `network/test-debug-capture.sh`
- Modify: `network/test-static-policy.sh`

- [ ] Replace service/ring deployment assertions with checks that the recorder unit is absent and `restore.sh` disables/removes a legacy installed unit before daemon reload.
- [ ] Require `restore.sh` to delete `/var/log/network-debug/ring` but never delete `incidents`.
- [ ] Remove freeze/copy/restore test cases and require `capture_main` to start deep capture directly.
- [ ] Keep tests for bounded captures, before/after ordering, manifests, output limits, lock exclusion, retention, CLI sudo re-exec, and no network/tunnel mutation.
- [ ] Run `bash network/test-debug-capture.sh` and `bash network/test-static-policy.sh`; expect failures against current recorder-dependent production code.
- [ ] Commit the failing specification.

### Task 4: Retire Recorder and Simplify Manual Capture

**Files:**
- Delete: `network/systemd/network-debug-pcap.service`
- Modify: `scripts/network-debug-capture`
- Modify: `restore.sh`
- Modify: `network/README.md`
- Test: `network/test-debug-capture.sh`
- Test: `network/test-static-policy.sh`

- [ ] Delete recorder state helpers and `freeze_ring` from the manual tool.
- [ ] Change `capture_main` signature to `(base, duration, bugreport, lock, note...)` and start deep capture before the before snapshot.
- [ ] Keep timeout completion normalization, signal-safe process cleanup, evidence permissions, retention, and explicit-only bugreport behavior.
- [ ] Remove recorder installation/enable/restart from `restore.sh`; add idempotent disable/remove and ring deletion before daemon reload.
- [ ] Remove README claims about constant capture and document manual-only capture.
- [ ] Run debug, static, syntax, and shellcheck tests; expect all to pass.
- [ ] Commit recorder retirement.

### Task 5: Full Offline Verification

**Files:**
- No new files

- [ ] Run `bash -n` on every changed shell file.
- [ ] Run `sudo -n bash network/test-reconfigure.sh`; expect all checks pass.
- [ ] Run `bash network/test-debug-capture.sh`, `bash network/test-static-policy.sh`, `bash network/test-ip-override.sh`, and `bash network/test-smartdns-no-cache.sh`; expect exit 0.
- [ ] Run `shellcheck -S error` on changed shell files, excluding the pre-existing whole-file SC2218 finding in `network/test-static-policy.sh` by checking only files without that baseline violation.
- [ ] Run `git diff --check`.

### Task 6: Controlled Live Deployment and Cleanup

**Files:**
- No repository changes

- [ ] Capture Tailscale prefs/status and current CN/owner state before deployment.
- [ ] Install updated repository-managed scripts and units without restarting Tailscale, SmartGateAgent, networkd, or iwd.
- [ ] Disable/remove `network-debug-pcap.service`, daemon-reload, delete only ring and pre-up transient artifacts, and preserve incidents.
- [ ] Seed `cn-last-applied` with one controlled `FORCE_CN=1` run only if missing.
- [ ] Run ordinary `FORCE=1` while monitoring routes and prove zero table `cn`/`cn_stage` mutation events.
- [ ] Confirm Tailscale remains direct, prefs unchanged, owner tables intact, and Internet/IOA probes pass.
- [ ] Report retained incidents and cleaned debug paths.
