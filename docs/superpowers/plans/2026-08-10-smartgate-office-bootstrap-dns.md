# SmartGate Office Bootstrap DNS Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore office-only internal DNS selection for SmartGate bootstrap domains so authenticated Tencent Ethernet selects the office scene.

**Architecture:** Keep the existing base `china` mappings as the off-office fallback and add exact `ioa` overrides to the dynamically deployed office fragment. Change no routes or ownership boundaries; validate the behavior with the existing static and namespace checks before bounded live deployment.

**Tech Stack:** Bash, SmartDNS configuration, systemd services, Linux network namespaces

---

### Task 1: Restore office bootstrap mappings

**Files:**
- Modify: `network/test-static-policy.sh:50-59`
- Modify: `network/smartdns/office.conf:6-11`

- [ ] **Step 1: Write the failing static-policy check**

Replace the office-override rejection with an exact assertion:

```bash
    if [ "$(grep -Fxc "nameserver /$domain/ioa" network/smartdns/office.conf)" -ne 1 ]; then
        echo "FAIL office SmartDNS does not map $domain exactly once to ioa" >&2
        fail=1
    fi
```

- [ ] **Step 2: Run the check to verify it fails**

Run: `bash network/test-static-policy.sh`

Expected: nonzero exit with `FAIL office SmartDNS does not map smartgate.oa.tencent.com exactly once to ioa` and equivalent failures for the other two domains.

- [ ] **Step 3: Add the minimal office mappings**

Insert after the three office DNS servers:

```text
# Use internal bootstrap endpoints while office Ethernet is authenticated.
nameserver /smartgate.oa.tencent.com/ioa
nameserver /sgw.woa.com/ioa
nameserver /ioa.tencent.com/ioa
```

- [ ] **Step 4: Run focused checks**

Run:

```bash
bash network/test-static-policy.sh
bash -n network/test-static-policy.sh scripts/network-reconfigure network/test-reconfigure.sh
```

Expected: static policy reports no failures and syntax checks exit zero.

- [ ] **Step 5: Run complete repository network checks**

Run:

```bash
sudo -n bash network/test-reconfigure.sh
bash network/test-ip-override.sh
bash network/test-debug-capture.sh
git diff --check
```

Expected: `test-reconfigure: 121 passed, 0 failed`; the other checks exit zero, including the expected bounded debug-capture warnings.

- [ ] **Step 6: Deploy and verify live scene**

Run the existing reconciler for the active wired interface, verify local bootstrap answers are internal, restart `ngnclient` once, and inspect fresh SmartGateAgent logs:

```bash
sudo -n scripts/network-reconfigure enp9s0u2u1u2
dig +short +tries=1 +timeout=2 smartgate.oa.tencent.com @127.0.0.1
dig +short +tries=1 +timeout=2 ioa.tencent.com @127.0.0.1
sudo -n systemctl restart ngnclient
```

Expected: local answers match office DNS internal addresses, bootstrap uses an internal address, and the fresh scene is not `EXTRA`. If it remains `EXTRA`, stop without changing routes.

- [ ] **Step 7: Commit only task files**

```bash
git add network/test-static-policy.sh network/smartdns/office.conf
git commit -m "network: restore office SmartGate bootstrap DNS"
```
