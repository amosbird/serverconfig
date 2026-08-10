# CN Gateway Change Batch Fix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prevent promotion from deleting routes already replaced during a CN gateway change.

**Architecture:** Derive stale routes by destination-prefix key while retaining full-route final validation and one batch invocation.

**Tech Stack:** Bash, iproute2 batch files, Linux network namespace tests.

---

### Task 1: Reproduce the gateway-change failure

**Files:**
- Modify: `network/test-reconfigure.sh`

- [ ] Install desired CN routes through gateway A plus one stale-only prefix.
- [ ] Change the physical main gateway to B.
- [ ] Force CN reconciliation and assert it exits zero.
- [ ] Assert desired prefixes use B and stale-only prefix is absent.
- [ ] Assert no delete command targets a desired prefix.
- [ ] Verify owner snapshots remain unchanged.
- [ ] Run the namespace test and confirm RED on the existing implementation.

### Task 2: Fix stale-key calculation

**Files:**
- Modify: `scripts/network-reconfigure`
- Modify: `network/test-reconfigure.sh`

- [ ] Add a desired destination-key temporary file.
- [ ] Reject duplicate routefile prefixes.
- [ ] Generate stale deletes only when the current route's first field is absent from desired keys.
- [ ] Keep desired replacement commands before delete commands.
- [ ] Keep final full-route equality and state-write gates.
- [ ] Run namespace and static tests and confirm GREEN.
- [ ] Commit implementation and tests.

### Task 3: Restore live China DNS

**Files:**
- No source changes.

- [ ] Run all network test suites and syntax/diff checks.
- [ ] Snapshot owner state.
- [ ] Run `FORCE_CN=1 scripts/network-reconfigure wlan0` once.
- [ ] Verify table `cn` uses the current main WLAN gateway.
- [ ] Verify China DNS upstream route lookups use `cn` and `baidu.com` resolves.
- [ ] Verify owner state is unchanged and no service was restarted.
