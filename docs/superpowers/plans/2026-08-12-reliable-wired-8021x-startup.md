# Reliable Tencent Wired 802.1X Startup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reliably start the registered Tencent wired EAP-TLS instance on cold boot and hot-plug.

**Architecture:** Add a systemd template install dependency from the exact interface device unit to the existing wired wpa_supplicant template, and enable only that concrete instance. Preserve the existing udev fallback and avoid deployment-time live network changes.

**Tech Stack:** systemd, udev, wpa_supplicant, Bash

---

### Task 1: Add the failing deployment contract

**Files:**
- Modify: `network/test-static-policy.sh`

- [ ] Assert the template drop-in contains exactly `[Install]` and `WantedBy=sys-subsystem-net-devices-%i.device`.
- [ ] Assert `restore.sh` never enables or starts a concrete wpa_supplicant instance.
- [ ] Assert the udev rule matches exactly registered MAC `08:3a:88:5a:b5:37` and requests only
      `wpa_supplicant@enp9s0u2u1u2.service`.
- [ ] Reject `enable --now` and any Tailscale mutation.
- [ ] Run `bash network/test-static-policy.sh`; expect failure for missing startup contract.

### Task 2: Implement device-bound startup

**Files:**
- Modify: `network/systemd/wpa_supplicant@.service.d/override.conf`
- Modify: `restore.sh`
- Modify: `network/README.md`

- [ ] Add:

```ini
[Unit]
BindsTo=sys-subsystem-net-devices-%i.device
After=sys-subsystem-net-devices-%i.device

[Install]
WantedBy=
WantedBy=sys-subsystem-net-devices-%i.device
```

- [ ] Remove any concrete wpa_supplicant enable operation from `restore.sh`.
- [ ] Restrict the udev hot-plug rule to the registered adapter MAC and concrete interface instance.
- [ ] Document that generic restore installs policy but does not assume the USB adapter exists.
- [ ] Run static and shell syntax checks; expect green.

### Task 3: Verify and deploy

**Files:**
- Modify: `network/systemd/wpa_supplicant@.service.d/override.conf`
- Modify: `restore.sh`
- Modify: `network/README.md`
- Modify: `network/test-static-policy.sh`

- [ ] Run all network repository checks and `git diff --check`.
- [ ] Install the generic template drop-in and hardware-restricted udev rule, reload rules, and remove
      the mistakenly created live enable symlink without stopping the active service.
- [ ] Start the instance explicitly for live repair and wait for `CTRL-EVENT-EAP-SUCCESS`.
- [ ] Cycle only `enp9s0u2u1u2` through networkctl to replace the pre-authentication lease.
- [ ] Reconcile repository policy, restart ngnclient once if needed, then verify SmartGate `INTRA`, policy download, and `ssh -o BatchMode=yes -o ConnectTimeout=10 9.134.215.72 true` reaches SSH authentication rather than proxy timeout.
- [ ] Commit only task files and run fresh post-commit checks.
