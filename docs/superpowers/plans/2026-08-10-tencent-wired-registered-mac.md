# Tencent Wired Registered MAC Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Apply the registered Tencent wired MAC before DHCP and SmartGate bootstrap.

**Architecture:** Use a path-scoped systemd `.link` configuration installed by the existing restore path.

**Tech Stack:** systemd.link, Bash static tests, networkctl/udev.

---

### Task 1: Test and configure

- [ ] Add static assertions for exact adapter path, MAC, and restore deployment.
- [ ] Confirm RED.
- [ ] Add `network/systemd-network/10-tencent-wired.link`.
- [ ] Update `restore.sh` to install repository `.link` files.
- [ ] Run static/syntax checks and commit.

### Task 2: Apply and verify

- [ ] Install the `.link` file.
- [ ] Keep Wi-Fi connected while cycling only the wired interface/USB device.
- [ ] Verify registered MAC and wired DHCP lease.
- [ ] Reconcile table 19.
- [ ] Restart `ngnclient` once and verify public bootstrap, policy load, `proxy:tls`, and internal access.
