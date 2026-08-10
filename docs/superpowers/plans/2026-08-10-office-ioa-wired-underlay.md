# Office IOA Wired Underlay Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Resolve IOA bootstrap names through office DNS and advertise the authenticated wired DHCP gateway to SmartGateAgent in table 19.

**Architecture:** Extend the existing office fragment with exact bootstrap mappings. Extend `network-reconfigure` with deterministic wired lease discovery and ownership-limited reconciliation of one table-19 default, without adding a policy rule or touching tunnel-owned tables.

**Tech Stack:** Bash, systemd-networkd lease inspection, iproute2 policy tables, SmartDNS, isolated network namespace tests.

---

### Task 1: Write failing behavior tests

**Files:**
- Modify: `network/test-reconfigure.sh`
- Modify: `network/test-static-policy.sh`

- [ ] Extend the networkctl fixture to return separate wired and wireless lease files.
- [ ] Model a wired interface with carrier, address, and DHCP router.
- [ ] Assert table 19 contains the wired default and no rule references it.
- [ ] Assert wired loss removes table 19 while owner tables remain unchanged.
- [ ] Assert gateway changes replace the advertisement.
- [ ] Assert the office fragment contains exact bootstrap-to-ioa mappings only while wired is up.
- [ ] Add static prohibitions on table-19 policy rules and owner-table mutation.
- [ ] Run namespace/static tests and confirm RED.

### Task 2: Implement current-stack calibration

**Files:**
- Modify: `network/smartdns/office.conf`
- Modify: `scripts/network-reconfigure`

- [ ] Add the three exact bootstrap mappings to the office fragment.
- [ ] Register fixed table `19 wired_underlay` with collision validation.
- [ ] Parse DHCP router from `networkctl dhcp-lease` for deterministic authenticated `enp*` candidate.
- [ ] Reconcile only the desired table-19 default, or empty table 19 when no candidate exists.
- [ ] Keep physical main gateway/CN derivation unchanged.
- [ ] Run namespace/static tests and confirm GREEN.
- [ ] Commit implementation and tests.

### Task 3: Verify and deploy

**Files:**
- No additional source files.

- [ ] Run all four network checks plus shell syntax and diff checks.
- [ ] Snapshot owner tables/rules before live reconciliation.
- [ ] Run one normal `network-reconfigure` reconciliation.
- [ ] Verify table 19, absence of rules to 19, internal bootstrap DNS answers, unchanged CN gateway, and unchanged owner state.
- [ ] Do not restart SmartGateAgent, Tailscale, networkd, or iwd.
