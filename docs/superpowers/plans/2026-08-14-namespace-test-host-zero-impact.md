# Namespace Test Host-Zero-Impact Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Verify after every namespace test run that no synthetic networking state leaked to the host.

**Architecture:** Run the anonymous network namespace as a child instead of replacing the outer harness. Compare a compact host-state snapshot before and after the child, while preserving the child's status and the existing temporary-directory trap.

**Tech Stack:** Bash, util-linux `unshare`, iproute2, ipset, iptables

---

### Task 1: Specify the outer harness contract

**Files:**
- Modify: `network/test-static-policy.sh`

- [ ] Require a host snapshot helper in `network/test-reconfigure.sh`.
- [ ] Reject `exec unshare`; require child `unshare` status capture and post-run comparison.
- [ ] Run static policy and confirm RED.

### Task 2: Implement host-zero-impact verification

**Files:**
- Modify: `network/test-reconfigure.sh:937-979`

- [ ] Add a read-only snapshot covering synthetic prefixes, owned test priorities, test links,
      namespace list, `ioa_intranet`, and relevant test chains.
- [ ] Capture the snapshot before running the anonymous namespace.
- [ ] Run `unshare -rn --mount` as a child with `set +e`, save its status, and restore shell mode.
- [ ] Compare the post-run host snapshot with the baseline; fail loudly on any difference without
      mutating host state.
- [ ] Exit with the child status when host state is unchanged.

### Task 3: Verify and commit

- [ ] Run static, namespace, IP override, debug-capture, syntax, and `git diff --check` checks.
- [ ] Run explicit host sentinel checks after the namespace suite.
- [ ] Commit only the spec, plan, harness, and static-policy test.
- [ ] Repeat static and namespace checks post-commit.
