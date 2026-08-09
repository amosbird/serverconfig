# CN Route Batch Promotion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace per-route CN table promotion with one validated `ip -batch` transaction input.

**Architecture:** Keep the existing independently validated `cn_stage` table. Snapshot desired and current routes into sorted temporary files, generate add-before-delete commands into one promotion file, execute it once with `ip -batch`, then retain the existing final equality and state-write gates.

**Tech Stack:** Bash, iproute2 `ip -batch`, Linux network namespaces, existing shell test harness.

---

### Task 1: Add the regression check

**Files:**
- Modify: `network/test-reconfigure.sh`
- Modify: `network/test-static-policy.sh`

- [ ] Add a namespace assertion that a forced CN rebuild logs exactly two `ip -batch` calls: staging and promotion.
- [ ] Assert the command log contains no standalone `route replace ... table cn` invocation.
- [ ] Seed one stale `cn` route and assert the promotion batch removes it while retaining all desired routes.
- [ ] Add a static rejection for the old per-route promotion loop.
- [ ] Run `sudo -n bash network/test-reconfigure.sh` and confirm RED for promotion call count/per-route invocation.

### Task 2: Batch promotion

**Files:**
- Modify: `scripts/network-reconfigure`

- [ ] Add cleanup-tracked temporary files for desired routes, current routes, stale routes, and promotion commands.
- [ ] After staging count validation, write normalized sorted desired/current snapshots.
- [ ] Generate desired `route replace ... table cn` commands first.
- [ ] Use `comm -23` to append only stale `route del ... table cn` commands.
- [ ] Execute exactly one promotion `ip -batch` call.
- [ ] Keep final table equality and `cn-last-applied` write after successful promotion.
- [ ] Run the focused namespace and static tests and confirm GREEN.
- [ ] Commit the implementation and tests.

### Task 3: Verify performance and regressions

**Files:**
- No additional source files.

- [ ] Run all four repository network checks.
- [ ] Run shell syntax and `git diff --check`.
- [ ] In an isolated namespace, compare 8,789-route staging and promotion batch timings and confirm both are sub-second on this host.
- [ ] Verify no live route rebuild or service restart occurred.
- [ ] Review the final diff for ownership, failure-state, and cleanup regressions.
