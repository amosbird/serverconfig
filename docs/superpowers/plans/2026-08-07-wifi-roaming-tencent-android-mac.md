# Wi-Fi Roaming and Tencent Android MAC Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Preserve DHCP state during short Wi-Fi roams and make only `Tencent-WiFi` use the registered Android MAC while retaining normal physical routing.

**Architecture:** iwd performs association-time MAC selection with global per-network randomization and a secret-profile `AddressOverride`; systemd-networkd retains DHCP configuration for a finite three-second carrier gap. The repository owns only global iwd/networkd policy and deployment validation, while `/var/lib/iwd/Tencent-WiFi.8021x` remains local secret material.

**Tech Stack:** Bash, iwd 3.12 configuration, systemd-networkd `.network` files, existing shell static tests.

---

## File structure

- Modify `network/iwd/main.conf`: enable iwd's per-network MAC policy.
- Modify `network/systemd-network/25-wireless.network`: add finite carrier-loss grace.
- Delete `network/systemd-network/26-wireless-tencent.network`: remove ineffective no-gateway policy.
- Modify `network/migrate.sh`: validate the local Tencent override, remove an obsolete installed networkd file, reload networkd configuration, and preserve the secret profile on rollback.
- Modify `restore.sh`: perform the same non-destructive cleanup, validation, and networkd reload in the normal deployment path.
- Modify `network/test-static-policy.sh`: leave runnable static and fixture-based checks for configuration, deployment, secrecy, and rollback contracts.
- Modify `network/README.md`: document Wi-Fi roaming behavior, Tencent routing, and local secret-profile ownership.
- Locally modify `/var/lib/iwd/Tencent-WiFi.8021x`: add the Android MAC override without exposing or committing credentials.

### Task 1: Lock down the desired repository configuration

**Files:**
- Modify: `network/test-static-policy.sh`
- Modify: `network/iwd/main.conf`
- Modify: `network/systemd-network/25-wireless.network`
- Delete: `network/systemd-network/26-wireless-tencent.network`

- [ ] **Step 1: Write failing static assertions**

Insert after the existing foreign-routing checks in `network/test-static-policy.sh`:

```bash
iwd_config=network/iwd/main.conf
wireless_config=network/systemd-network/25-wireless.network
obsolete_tencent_config=network/systemd-network/26-wireless-tencent.network

if [ "$(grep -Fxc 'AddressRandomization=network' "$iwd_config")" -ne 1 ]; then
    echo 'FAIL iwd does not use stable per-network MAC addresses' >&2
    fail=1
else
    echo 'OK   iwd uses stable per-network MAC addresses'
fi
if [ "$(grep -Fxc 'IgnoreCarrierLoss=3s' "$wireless_config")" -ne 1 ] ||
   grep -Eq '^IgnoreCarrierLoss=(yes|infinite)$' "$wireless_config"; then
    echo 'FAIL wireless carrier grace is not exactly finite 3s' >&2
    fail=1
else
    echo 'OK   wireless carrier grace is finite 3s'
fi
if [ -e "$obsolete_tencent_config" ]; then
    echo 'FAIL obsolete Tencent no-gateway networkd config still exists' >&2
    fail=1
else
    echo 'OK   Tencent-WiFi uses the generic physical Wi-Fi configuration'
fi
```

- [ ] **Step 2: Run the test and verify RED**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: nonzero exit with these new failures:

```text
FAIL iwd does not use stable per-network MAC addresses
FAIL wireless carrier grace is not exactly finite 3s
FAIL obsolete Tencent no-gateway networkd config still exists
```

- [ ] **Step 3: Add the minimal iwd setting**

Change the beginning of `network/iwd/main.conf` to:

```ini
[General]
# Use a stable generated MAC per SSID; Tencent-WiFi overrides this in its local secret profile.
AddressRandomization=network
# Let systemd-networkd handle IP configuration
EnableNetworkConfiguration=false
```

- [ ] **Step 4: Add finite carrier grace**

Change the `[Network]` section of `network/systemd-network/25-wireless.network` to:

```ini
[Network]
DHCP=yes
# Preserve DHCP state across short BSSID roaming gaps, but expire it on a real disconnect.
IgnoreCarrierLoss=3s
# DNS handled by SmartDNS on 127.0.0.1
DNS=127.0.0.1
DNSDefaultRoute=false
```

- [ ] **Step 5: Delete the obsolete Tencent networkd file**

Run:

```bash
rm network/systemd-network/26-wireless-tencent.network
```

Expected: only `25-wireless.network` remains responsible for WLAN DHCP and gateway behavior.

- [ ] **Step 6: Run the focused check and verify GREEN**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: exit 0 and the three new checks print `OK`.

- [ ] **Step 7: Commit the repository policy change**

```bash
git add network/test-static-policy.sh network/iwd/main.conf \
    network/systemd-network/25-wireless.network \
    network/systemd-network/26-wireless-tencent.network
git commit -m "network: preserve Wi-Fi state across short roams"
```

### Task 2: Make deployment remove stale policy and validate the secret profile

**Files:**
- Modify: `network/test-static-policy.sh`
- Modify: `network/migrate.sh`
- Modify: `restore.sh`

- [ ] **Step 1: Add a failing fixture test for profile validation**

Add before `exit "$fail"` in `network/test-static-policy.sh`:

```bash
tencent_iwd_profile_contract() (
    local sandbox profile output secret='fixture-private-passphrase'
    sandbox=$(mktemp -d)
    profile="$sandbox/Tencent-WiFi.8021x"
    trap 'rm -rf "$sandbox"' EXIT

    # shellcheck source=network/migrate.sh
    source network/migrate.sh

    cat >"$profile" <<EOF
[Security]
EAP-Method=TLS
EAP-TLS-ClientKeyPassphrase=$secret

[Settings]
AutoConnect=true
AddressOverride=1e:dc:46:00:66:1b
EOF
    output=$(check_tencent_iwd_profile "$profile")
    if [[ "$output" != *'[ok]'* ]] || [[ "$output" == *"$secret"* ]]; then
        echo 'FAIL valid Tencent iwd profile was rejected or leaked credentials' >&2
        return 1
    fi

    sed -i '/^AddressOverride=/d' "$profile"
    output=$(check_tencent_iwd_profile "$profile")
    if [[ "$output" != *'[WARN]'* ]] || [[ "$output" == *"$secret"* ]]; then
        echo 'FAIL missing Tencent Android MAC override was not safely reported' >&2
        return 1
    fi

    output=$(check_tencent_iwd_profile "$sandbox/missing.8021x")
    if [[ "$output" != *'[WARN]'* ]]; then
        echo 'FAIL missing Tencent profile was not reported as optional' >&2
        return 1
    fi
    echo 'OK   Tencent iwd profile validation is optional and does not expose secrets'
)
if ! tencent_iwd_profile_contract; then
    fail=1
fi
```

Also add deployment-source assertions near the configuration checks:

```bash
for deployment in restore.sh network/migrate.sh; do
    if ! grep -Fq '26-wireless-tencent.network' "$deployment" ||
       ! grep -Fq 'networkctl reload' "$deployment"; then
        echo "FAIL $deployment does not remove stale Tencent policy and reload networkd" >&2
        fail=1
    fi
done
if sed -n '/^rollback() {/,/^}/p' network/migrate.sh |
   grep -Eq 'rm .*Tencent-WiFi\.8021x|sed .*Tencent-WiFi\.8021x'; then
    echo 'FAIL rollback mutates the Tencent secret profile' >&2
    fail=1
else
    echo 'OK   rollback leaves the Tencent secret profile untouched'
fi
```

- [ ] **Step 2: Run the test and verify RED**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: nonzero exit because `check_tencent_iwd_profile` is undefined and deployment paths do not yet both remove/reload.

- [ ] **Step 3: Add the testable validation helper**

Add before `install_configs()` in `network/migrate.sh`:

```bash
check_tencent_iwd_profile() {
    local profile=${1:-/var/lib/iwd/Tencent-WiFi.8021x}
    if [ ! -f "$profile" ]; then
        echo "  [WARN] $profile missing — Tencent-WiFi won't work"
    elif [ "$(grep -Fxc 'AddressOverride=1e:dc:46:00:66:1b' "$profile")" -eq 1 ]; then
        echo "  [ok] $profile has the Android MAC override"
    else
        echo "  [WARN] $profile is missing the Android MAC override"
    fi
}
```

Replace the Tencent-profile branch at the start of `install_iwd_profiles()` with:

```bash
    check_tencent_iwd_profile
```

This helper prints no profile content and treats missing local secret material as a warning.

- [ ] **Step 4: Remove stale networkd policy and reload safely in migration install**

Immediately after copying repository `.network` files in `install_configs()`, add:

```bash
    rm -f /etc/systemd/network/26-wireless-tencent.network
    networkctl reload
```

Keep this in `install_configs()` only. Do not add a networkd/iwd restart or link reconfiguration.

- [ ] **Step 5: Add equivalent behavior to restore**

Immediately after the `.network` copy in `restore.sh`, add:

```bash
    sudo rm -f /etc/systemd/network/26-wireless-tencent.network
    if sudo test -f /var/lib/iwd/Tencent-WiFi.8021x &&
       sudo grep -Fqx 'AddressOverride=1e:dc:46:00:66:1b' \
           /var/lib/iwd/Tencent-WiFi.8021x; then
        echo '  [ok] Tencent-WiFi profile has the Android MAC override'
    else
        echo '  [WARN] Tencent-WiFi profile is missing or lacks the Android MAC override'
    fi
    sudo networkctl reload
```

The `grep` result is the only secret-profile observation; never print the matching file or its other lines.

- [ ] **Step 6: Keep rollback cleanup explicit**

Leave `/var/lib/iwd/Tencent-WiFi.8021x` untouched. Keep the existing rollback list including
`26-wireless-tencent` so rollback also removes stale installations created by older repository
versions.

- [ ] **Step 7: Run static tests and verify GREEN**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: exit 0, including:

```text
OK   Tencent iwd profile validation is optional and does not expose secrets
OK   rollback leaves the Tencent secret profile untouched
```

- [ ] **Step 8: Run syntax checks**

Run:

```bash
bash -n restore.sh network/migrate.sh network/test-static-policy.sh
```

Expected: exit 0 with no output.

- [ ] **Step 9: Commit deployment behavior**

```bash
git add restore.sh network/migrate.sh network/test-static-policy.sh
git commit -m "network: deploy Tencent per-network MAC policy"
```

### Task 3: Document operator behavior and verification

**Files:**
- Modify: `network/README.md`

- [ ] **Step 1: Add documentation contract checks first**

Add near the other Wi-Fi static checks in `network/test-static-policy.sh`:

```bash
for statement in \
    'IgnoreCarrierLoss=3s' \
    'AddressRandomization=network' \
    'AddressOverride=1e:dc:46:00:66:1b'
do
    if ! grep -Fq "$statement" network/README.md; then
        echo "FAIL network README omits Wi-Fi policy: $statement" >&2
        fail=1
    fi
done
```

- [ ] **Step 2: Verify the documentation test is RED**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: nonzero exit with one or more `network README omits Wi-Fi policy` messages.

- [ ] **Step 3: Document the Wi-Fi policy**

Add this section before `## Reconciliation and AP changes` in `network/README.md`:

```markdown
## Wi-Fi roaming and MAC identity

The generic `25-wireless.network` applies `IgnoreCarrierLoss=3s` to every WLAN. A short iwd BSSID
roam therefore retains the DHCP address, connected route, and physical default route. A longer
carrier loss still expires the lease normally; infinite carrier retention is intentionally forbidden.

`network/iwd/main.conf` sets `AddressRandomization=network`, so ordinary SSIDs receive a stable
per-network MAC. The local secret profile `/var/lib/iwd/Tencent-WiFi.8021x` additionally contains
`AddressOverride=1e:dc:46:00:66:1b`, the MAC registered for the Android identity. That profile and its
EAP-TLS secrets are not repository-owned. `Tencent-WiFi` uses the normal wireless DHCP gateway and
physical routing; there is no special no-gateway `.network` file.

The iwd global setting takes effect after the next natural iwd start. Do not restart iwd merely to
apply it during an active remote session. Validate the MAC and routing on the next natural
`Tencent-WiFi` connection, and validate carrier grace on the next natural room-to-room roam.
```

- [ ] **Step 4: Run the static test and verify GREEN**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: exit 0.

- [ ] **Step 5: Commit documentation and its check**

```bash
git add network/README.md network/test-static-policy.sh
git commit -m "docs: explain Wi-Fi roaming and MAC ownership"
```

### Task 4: Apply the local secret-profile override without disconnecting Wi-Fi

**Files:**
- Modify locally only: `/var/lib/iwd/Tencent-WiFi.8021x`
- Do not add any `/var/lib/iwd` file to git.

- [ ] **Step 1: Confirm the current network is not Tencent-WiFi**

Run:

```bash
iwctl station wlan0 show | sed -n '/Connected network/p'
```

Expected before proceeding: the connected network is not `Tencent-WiFi`. If it is, stop and wait for a safe maintenance window rather than changing association identity live.

- [ ] **Step 2: Create a root-only backup**

Run:

```bash
sudo -n cp -a /var/lib/iwd/Tencent-WiFi.8021x \
    /var/lib/iwd/Tencent-WiFi.8021x.before-android-mac
sudo -n chmod 600 /var/lib/iwd/Tencent-WiFi.8021x.before-android-mac
```

Expected: both files remain owned by root and mode `600`.

- [ ] **Step 3: Atomically add the override without displaying secrets**

Run:

```bash
sudo -n python3 - <<'PY'
from pathlib import Path
import os
import stat
import tempfile

path = Path('/var/lib/iwd/Tencent-WiFi.8021x')
data = path.read_text()
lines = [line for line in data.splitlines() if not line.startswith('AddressOverride=')]
try:
    settings = lines.index('[Settings]')
except ValueError as error:
    raise SystemExit('Tencent-WiFi profile has no [Settings] section') from error
insert_at = next(
    (index for index in range(settings + 1, len(lines)) if lines[index].startswith('[')),
    len(lines),
)
lines.insert(insert_at, 'AddressOverride=1e:dc:46:00:66:1b')
result = '\n'.join(lines) + '\n'
st = path.stat()
fd, candidate = tempfile.mkstemp(prefix='.Tencent-WiFi.8021x.', dir=path.parent)
try:
    os.write(fd, result.encode())
    os.fsync(fd)
    os.close(fd)
    fd = -1
    os.chown(candidate, st.st_uid, st.st_gid)
    os.chmod(candidate, stat.S_IMODE(st.st_mode))
    os.replace(candidate, path)
finally:
    if fd >= 0:
        os.close(fd)
    if os.path.exists(candidate):
        os.unlink(candidate)
PY
```

Expected: exit 0. This is a one-time administration command, not a repository script.

- [ ] **Step 4: Verify only non-secret metadata and the exact override**

Run:

```bash
sudo -n test "$(stat -c %a /var/lib/iwd/Tencent-WiFi.8021x)" = 600
sudo -n test "$(grep -Fxc 'AddressOverride=1e:dc:46:00:66:1b' \
    /var/lib/iwd/Tencent-WiFi.8021x)" = 1
```

Expected: both commands exit 0 and print nothing.

- [ ] **Step 5: Verify no tracked or untracked credential copy appeared**

Run:

```bash
if git status --short --untracked-files=all | grep -E 'Tencent-WiFi.*\.8021x'; then
    echo 'FAIL Tencent credential material appeared in the repository' >&2
    exit 1
fi
```

Expected: exit 0 with no output.

### Task 5: Run the complete non-destructive verification and deploy repository files

**Files:**
- No new source files.
- Installed copies under `/etc/iwd` and `/etc/systemd/network` are deployment artifacts.

- [ ] **Step 1: Run all repository network checks before deployment**

```bash
bash network/test-ip-override.sh
sudo -n bash network/test-reconfigure.sh
bash network/test-static-policy.sh
bash network/test-debug-capture.sh
```

Expected: every command exits 0. `test-reconfigure.sh` performs destructive operations only inside its isolated network namespace; none of these commands may change the live network.

- [ ] **Step 2: Check shell syntax and whitespace**

```bash
bash -n restore.sh network/migrate.sh network/test-static-policy.sh
git diff --check
```

Expected: both commands exit 0 with no output.

- [ ] **Step 3: Install only the changed repository configuration**

Avoid running the broad GUI restore path. Copy the two changed files, remove the obsolete installed file, then reload networkd:

```bash
sudo -n cp network/iwd/main.conf /etc/iwd/main.conf
sudo -n cp network/systemd-network/25-wireless.network \
    /etc/systemd/network/25-wireless.network
sudo -n rm -f /etc/systemd/network/26-wireless-tencent.network
sudo -n networkctl reload
```

Expected: exit 0. Do not restart networkd, iwd, tailscaled, SmartGateAgent, or SmartDNS.

- [ ] **Step 4: Verify installed bytes and effective networkd file**

```bash
sudo -n cmp -s network/iwd/main.conf /etc/iwd/main.conf
sudo -n cmp -s network/systemd-network/25-wireless.network \
    /etc/systemd/network/25-wireless.network
sudo -n test ! -e /etc/systemd/network/26-wireless-tencent.network
networkctl status wlan0 --no-pager | grep -F '25-wireless.network'
```

Expected: all commands exit 0. The active connection remains up; the iwd MAC policy waits for the next natural iwd start.

- [ ] **Step 5: Verify prohibited services were not restarted by this deployment**

Record current active state without mutating it:

```bash
systemctl is-active systemd-networkd iwd tailscaled smartdns
systemctl is-active ngnclient 2>/dev/null || true
```

Expected: read-only status output only. Do not attempt to repair a service here.

- [ ] **Step 6: Run fresh post-deployment static verification**

```bash
bash network/test-static-policy.sh
```

Expected: exit 0.

- [ ] **Step 7: Confirm repository state contains only intended commits plus pre-existing user edits**

```bash
git status --short
git --no-pager log -4 --oneline
```

Expected: no uncommitted network-task files; pre-existing unrelated user modifications remain untouched.

### Task 6: Natural-event validation checklist

**Files:**
- No changes during this task.

- [ ] **Step 1: Validate the next natural Tencent-WiFi connection**

After naturally connecting to `Tencent-WiFi`, run read-only checks:

```bash
iwctl station wlan0 show
ip -brief link show wlan0
ip -4 route show table main default dev wlan0
networkctl status wlan0 --no-pager
```

Expected:

- connected SSID is `Tencent-WiFi`;
- `wlan0` MAC is `1e:dc:46:00:66:1b`;
- a DHCP physical default route exists on `wlan0`;
- networkd reports `/etc/systemd/network/25-wireless.network`.

- [ ] **Step 2: Validate leaving Tencent-WiFi**

After naturally connecting to another SSID, run:

```bash
ip -brief link show wlan0
```

Expected: the MAC is no longer `1e:dc:46:00:66:1b`.

- [ ] **Step 3: Validate the next natural meeting-room roam**

After a natural BSSID roam shorter than three seconds, inspect without changing state:

```bash
journalctl -b -u iwd -u systemd-networkd -u tailscaled --since '-10 minutes' --no-pager
```

Expected around the roam:

- iwd reports the BSSID transition;
- networkd does not report `DHCP lease lost` for the short carrier gap;
- tailscaled does not report `defaultRoute=""` for that roam;
- exit-node traffic remains usable without `tailscale down`.

If carrier loss exceeds three seconds, DHCP teardown is expected and does not invalidate the design.
