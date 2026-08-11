# SmartDNS-Authoritative IOA Classification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Route all SmartDNS-classified IOA business addresses, including `token.woa.com`'s `21.x` answer, through table `ioa` while excluding SmartGate transport domains.

**Architecture:** Remove the static `ioa_intranet` intersection and make dynamic `ipset ioa` the sole domain-derived classifier. Use SmartDNS's specific `ipset /domain/-` overrides to prevent bootstrap and proxy suffixes from entering that set, preserving owner marks and routefile priority.

**Tech Stack:** Bash, SmartDNS, ipset/iptables, Linux policy routing and network namespaces

---

### Task 1: Specify authoritative dynamic classification

**Files:**
- Modify: `network/test-reconfigure.sh:572-648`
- Modify: `network/test-static-policy.sh`

- [ ] **Step 1: Change namespace assertions to require any `ioa` address to reach the mark rule**

Use `21.34.11.74` as an arbitrary address outside the old whitelist, assert a real packet reaches the mark rule when present in `ioa`, then delete it and assert the same packet does not reach the rule. Remove the `ioa_intranet` drift test.

- [ ] **Step 2: Add static transport-exclusion and removal checks**

Require exactly one of each:

```text
ipset /sgw.woa.com/-
ipset /smartgate.oa.tencent.com/-
ipset /ioa.tencent.com/-
```

Reject active references to `ioa_intranet`, static `21/8 -> ioa`, and static `9/8 -> ioa`.

- [ ] **Step 3: Run tests and confirm RED**

Run:

```bash
sudo -n bash network/test-reconfigure.sh
bash network/test-static-policy.sh
```

Expected: failures because the production chain still intersects `ioa_intranet`, creates that set, and lacks the transport exclusions.

### Task 2: Implement authoritative SmartDNS classification

**Files:**
- Modify: `scripts/network-reconfigure`
- Modify: `network/smartdns/smartdns.conf`

- [ ] **Step 1: Simplify the owned mark chain**

Remove `IOA_INTRANET_SET`, `IOA_INTRANET_CIDRS`, set creation, fingerprinting, flushing, and intersection. Keep:

```bash
iptables -t mangle -A "$NEXT_CHAIN" -m mark ! --mark 0x0/0xffffffff -j RETURN
iptables -t mangle -A "$NEXT_CHAIN" \
    -m set --match-set ioa dst \
    -j MARK --set-xmark 0x1/0xffffffff
```

Retain the exact mark policy rule and tun0 MASQUERADE.

- [ ] **Step 2: Add SmartDNS transport exclusions**

Add the three `ipset /domain/-` directives adjacent to the bootstrap/transport explanation and update the stale explanation that claims only the firewall whitelist can prevent additions.

- [ ] **Step 3: Run focused tests and confirm GREEN**

Run:

```bash
sudo -n bash network/test-reconfigure.sh
bash network/test-static-policy.sh
bash -n scripts/network-reconfigure network/test-reconfigure.sh network/test-static-policy.sh
```

Expected: namespace and static suites pass with no syntax errors.

### Task 3: Update active architecture documentation

**Files:**
- Modify: `network/README.md:86-106`

- [ ] **Step 1: Replace the obsolete dual-set model**

Document `ipset ioa` as authoritative, the three specific transport exclusions, the unchanged static `10/8` and `100.12/16` rules, and the absence of static `9/8`/`21/8` routes.

- [ ] **Step 2: Verify no active references remain**

Run:

```bash
grep -RIn 'ioa_intranet' scripts/network-reconfigure network/README.md network/test-reconfigure.sh network/test-static-policy.sh
```

Expected: no output and grep exit 1.

### Task 4: Verify, deploy, and commit

**Files:**
- Modify: `scripts/network-reconfigure`
- Modify: `network/smartdns/smartdns.conf`
- Modify: `network/test-reconfigure.sh`
- Modify: `network/test-static-policy.sh`
- Modify: `network/README.md`

- [ ] **Step 1: Run complete offline checks**

```bash
sudo -n bash network/test-reconfigure.sh
bash network/test-static-policy.sh
bash network/test-ip-override.sh
bash network/test-debug-capture.sh
bash -n scripts/network-reconfigure network/test-reconfigure.sh network/test-static-policy.sh
git diff --check
```

Expected: all checks pass; debug-capture may print its expected bounded file-size warnings.

- [ ] **Step 2: Deploy through existing paths**

Run `sudo -n scripts/network-reconfigure enp9s0u2u1u2`, safely deploy the repository SmartDNS config through the existing deployment helper, and do not restart Tailscale.

- [ ] **Step 3: Verify dynamic membership and live traffic**

Flush only repository/SmartDNS-owned dynamic `ipset ioa`, query business and excluded transport names, then verify:

- `21.34.11.74` enters `ioa` after querying `token.woa.com`;
- SmartGate transport answers do not enter `ioa`;
- `token.woa.com` returns HTTP through AIO and emits no SYN on `tailscale0`;
- SmartGate remains `INTRA` and tables 19/20/230 remain owner-managed.

- [ ] **Step 4: Commit only task files**

```bash
git add scripts/network-reconfigure network/smartdns/smartdns.conf \
    network/test-reconfigure.sh network/test-static-policy.sh network/README.md
git commit -m "network: trust SmartDNS IOA classification"
```

- [ ] **Step 5: Run fresh post-commit verification and inspect status**

Repeat the focused static/namespace/live checks and confirm unrelated user files remain uncommitted.
