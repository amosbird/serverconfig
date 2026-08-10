# SmartGate Bootstrap DNS Deadlock Fix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove office bootstrap DNS overrides that prevent SmartGateAgent policy download.

**Architecture:** Keep public China bootstrap resolution in the base config and reserve office DNS for business domains.

**Tech Stack:** SmartDNS configuration, Bash static and namespace tests.

---

### Task 1: Add regression checks

**Files:**
- Modify: `network/test-static-policy.sh`
- Modify: `network/test-reconfigure.sh`

- [ ] Assert base bootstrap mappings exist exactly once with group `china`.
- [ ] Assert office source and generated fixture contain no bootstrap mappings.
- [ ] Run tests and confirm RED.

### Task 2: Remove overrides

**Files:**
- Modify: `network/smartdns/office.conf`
- Modify: `network/test-reconfigure.sh`

- [ ] Delete the three office `/ioa` bootstrap mappings.
- [ ] Update the office fixture to match production semantics.
- [ ] Run all network checks, syntax, and diff validation.
- [ ] Commit the fix.

### Task 3: Deploy and verify

- [ ] Run one normal reconciliation to update the live office fragment.
- [ ] Verify SmartDNS bootstrap names return public addresses.
- [ ] Observe SmartGateAgent logs for successful policy refresh and `Use proxy:tls` business decisions.
- [ ] Verify table 19 and owner state remain unchanged.
