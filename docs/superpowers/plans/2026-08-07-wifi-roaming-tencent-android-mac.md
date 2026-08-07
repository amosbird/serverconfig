# Wi-Fi Roaming, Tencent Android MAC, and Legacy Cleanup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove the retired netctl migration/rollback stack, preserve DHCP state during short Wi-Fi roams, and use the registered Android MAC only on `Tencent-WiFi` with normal physical routing.

**Architecture:** `restore.sh` becomes the sole repository deployment path. iwd owns association-time per-network MAC selection, systemd-networkd owns a finite carrier-loss grace and normal DHCP routing, and the credential-bearing Tencent iwd profile remains local secret material outside git.

**Tech Stack:** Bash, iwd 3.12 configuration, systemd-networkd `.network` files, existing static and namespace shell tests.

---

## File structure

- Delete `network/migrate.sh`: the completed migration and rollback CLI is no longer supported.
- Delete `network/rollback/README.md`, `network/rollback/90-amos-dhcp`, and `network/rollback/90-wired-netctl.rules`: remove pre-iwd rollback artifacts.
- Delete `scripts/ncswitch`: remove the netctl switch helper.
- Delete `old_README.org`: remove obsolete setup guidance.
- Modify `.config/fish/conf.d/completions.fish`: remove netctl/ncswitch completion code.
- Modify `network/test-static-policy.sh`: remove migration-only tests and add a current-source legacy absence check plus Wi-Fi policy checks.
- Modify `network/iwd/main.conf`: enable stable per-network MAC selection.
- Modify `network/systemd-network/25-wireless.network`: add finite roaming carrier grace.
- Delete `network/systemd-network/26-wireless-tencent.network`: remove ineffective no-gateway behavior.
- Modify `restore.sh`: clean obsolete installed Tencent networkd policy, report the local MAC override without exposing secrets, and reload networkd configuration.
- Modify `network/README.md`: replace migration/rollback guidance with current deployment, roaming, and secret ownership documentation.
- Modify `docs/superpowers/specs/2026-08-07-wifi-roaming-carrier-grace-design.md`: already revised to record the approved cleanup scope.
- Modify this plan to reflect the approved scope; older completed files in `docs/superpowers/plans/` remain unchanged historical records.
- Modify locally only `/var/lib/iwd/Tencent-WiFi.8021x`: add the Android MAC override without exposing or committing credentials.

## Task 1: Retire executable migration and netctl legacy

**Files:**
- Modify: `network/test-static-policy.sh`
- Delete: `network/migrate.sh`
- Delete: `network/rollback/README.md`
- Delete: `network/rollback/90-amos-dhcp`
- Delete: `network/rollback/90-wired-netctl.rules`
- Delete: `scripts/ncswitch`
- Delete: `old_README.org`
- Delete: `network/systemd-network/26-wireless-tencent.network`
- Modify: `network/README.md`
- Modify: `.config/fish/conf.d/completions.fish`

- [ ] **Step 1: Add a failing current-source legacy check**

Add before `exit "$fail"` in `network/test-static-policy.sh`:

```bash
legacy_network_sources=(
    restore.sh
    network/README.md
    network/iwd
    network/smartdns
    network/systemd
    network/systemd-network
    network/systemd-networkd.conf.d
    network/udev
    scripts
    .config/fish/conf.d/completions.fish
)
legacy_network_pattern='netctl|dhcpcd|ncswitch|network/migrate\.sh|network/rollback'
if grep -RInE "$legacy_network_pattern" "${legacy_network_sources[@]}" \
        >/tmp/network-legacy.$$ 2>/dev/null; then
    echo 'FAIL current network sources still reference the retired migration stack' >&2
    cat /tmp/network-legacy.$$ >&2
    fail=1
else
    echo 'OK   current network sources do not reference the retired migration stack'
fi
rm -f /tmp/network-legacy.$$
for retired in network/migrate.sh network/rollback scripts/ncswitch old_README.org; do
    if [ -e "$retired" ]; then
        echo "FAIL retired network artifact still exists: $retired" >&2
        fail=1
    fi
done
```

The explicit source list excludes `docs/superpowers/plans/`, preserving completed plans unchanged as historical records.

- [ ] **Step 2: Run the test and verify RED**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: nonzero exit with `FAIL current network sources still reference the retired migration stack` and retired-artifact failures.

- [ ] **Step 3: Remove migration-only tests before deleting their implementation**

In `network/test-static-policy.sh`, delete these checks and their fixture functions because they exclusively test `network/migrate.sh`:

- the activation check beginning with `# Activation must not create or seed the legacy fragment`;
- the SmartDNS fragment-backup source assertions beginning with `# shellcheck disable=SC2016 # fragment variables are literal source text`;
- the complete `smartdns_backup_restore_contract()` function and its invocation;
- the second half of the foreign-routing deployment check that requires `network/migrate.sh` and rollback removal;
- migration references from `reject 'no fallback deployment references'`;
- all checks beginning with `reject 'install is non-destructive'` through the obsolete-underlay mapping assertion;
- the complete `rollback_stage_registration_contract()` function and its invocation;
- any remaining `source network/migrate.sh`, `network/migrate.sh`, `rollback()`, or migration-only assertion.

Retain `smartdns_deployment_contract()`: it sources `network/smartdns-deploy.sh` and tests the current SmartDNS deployment path independently of migration.

- [ ] **Step 4: Remove the retired files**

Run:

```bash
git rm network/migrate.sh \
    network/rollback/README.md \
    network/rollback/90-amos-dhcp \
    network/rollback/90-wired-netctl.rules \
    scripts/ncswitch \
    old_README.org
```

Expected: all six files are staged for deletion and `network/rollback/` becomes empty.

- [ ] **Step 5: Remove fish completion and current README legacy**

Delete this function and completion from `.config/fish/conf.d/completions.fish`:

```fish
function __fish_netctl_get_profiles
    command netctl list | sed -e 's/^[ \t*]*//'
end
```

```fish
complete -f -c ncswitch -a '(__fish_netctl_get_profiles)'
```

Do not change unrelated completions.

Delete `network/systemd-network/26-wireless-tencent.network`, whose no-gateway behavior is both
ineffective and contrary to the approved normal physical routing:

```bash
git rm network/systemd-network/26-wireless-tencent.network
```

Replace the `## Migration and rollback` section in `network/README.md` with this minimal current
statement; Task 4 will add the detailed Wi-Fi operator guidance:

```markdown
## Deployment

`restore.sh` is the only repository deployment entry point. There is no supported rollback to the
retired pre-iwd network stack. Installed pre-iwd files under `/etc` are left for a separate inventoried
cleanup so wired 802.1X credentials are not removed accidentally.
```

- [ ] **Step 6: Run syntax and static checks**

Run:

```bash
bash -n network/test-static-policy.sh
fish -n .config/fish/conf.d/completions.fish
bash network/test-static-policy.sh
```

Expected: all three commands exit 0. No deleted migration implementation is sourced or required,
and current source/documentation contains no retired-stack reference.

- [ ] **Step 7: Commit the legacy deletion**

```bash
git add network/test-static-policy.sh network/README.md \
    network/systemd-network/26-wireless-tencent.network \
    .config/fish/conf.d/completions.fish
git commit -m "network: remove retired migration stack"
```

## Task 2: Lock down Wi-Fi roaming and routing policy

**Files:**
- Modify: `network/test-static-policy.sh`
- Modify: `network/iwd/main.conf`
- Modify: `network/systemd-network/25-wireless.network`

- [ ] **Step 1: Add failing configuration assertions**

Add near the foreign-routing configuration checks in `network/test-static-policy.sh`:

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

Expected: nonzero exit for the first two new configuration requirements. The obsolete Tencent file
absence check already passes because Task 1 removed it.

- [ ] **Step 3: Enable iwd per-network MAC selection**

Change the start of `network/iwd/main.conf` to:

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

- [ ] **Step 5: Confirm special Tencent networkd policy is absent**

Run:

```bash
test ! -e network/systemd-network/26-wireless-tencent.network
```

Expected: exit 0; `Tencent-WiFi` uses the generic WLAN file and normal DHCP gateway/routes.

- [ ] **Step 6: Run the static check and verify GREEN for Wi-Fi policy**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: the three Wi-Fi policy assertions print `OK`. Any remaining failure must be investigated before proceeding.

- [ ] **Step 7: Commit Wi-Fi repository policy**

```bash
git add network/test-static-policy.sh network/iwd/main.conf \
    network/systemd-network/25-wireless.network
git commit -m "network: preserve Wi-Fi state across short roams"
```

## Task 3: Make restore the sole safe deployment path

**Files:**
- Modify: `network/test-static-policy.sh`
- Modify: `restore.sh`

- [ ] **Step 1: Add failing restore deployment assertions**

Add near the Wi-Fi assertions in `network/test-static-policy.sh`:

```bash
if ! grep -Fq 'sudo rm -f /etc/systemd/network/26-wireless-tencent.network' restore.sh ||
   ! grep -Fq 'sudo networkctl reload' restore.sh; then
    echo 'FAIL restore does not remove stale Tencent policy and reload networkd' >&2
    fail=1
else
    echo 'OK   restore removes stale Tencent policy and reloads networkd'
fi
if ! grep -Fq "grep -Fqx 'AddressOverride=1e:dc:46:00:66:1b'" restore.sh; then
    echo 'FAIL restore does not validate the Tencent Android MAC override' >&2
    fail=1
fi
reject 'restore never prints Tencent credential profile contents' \
    'cat .*Tencent-WiFi\.8021x|sed .*Tencent-WiFi\.8021x|grep -v .*Tencent-WiFi\.8021x' \
    restore.sh
reject 'restore does not restart network owners for Wi-Fi policy deployment' \
    'systemctl (restart|try-restart) (systemd-networkd|iwd|tailscaled|smartdns|ngnclient)' \
    restore.sh
```

- [ ] **Step 2: Run the test and verify RED**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: nonzero exit with `FAIL restore does not remove stale Tencent policy and reload networkd` and missing override validation.

- [ ] **Step 3: Add minimal restore behavior**

Immediately after copying repository `.network` files in `restore.sh`, add:

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

This check reports only pass/fail and never emits profile contents. `networkctl reload` reloads `.network` definitions without restarting networkd or forcing a reconnect.

- [ ] **Step 4: Run focused verification**

Run:

```bash
bash -n restore.sh network/test-static-policy.sh
bash network/test-static-policy.sh
```

Expected: both commands exit 0, including `OK   restore removes stale Tencent policy and reloads networkd`.

- [ ] **Step 5: Commit the sole deployment path**

```bash
git add restore.sh network/test-static-policy.sh
git commit -m "network: deploy Wi-Fi policy through restore"
```

## Task 4: Replace current migration documentation

**Files:**
- Modify: `network/test-static-policy.sh`
- Modify: `network/README.md`

- [ ] **Step 1: Add failing documentation assertions**

Add near the current-source legacy check:

```bash
for statement in \
    'IgnoreCarrierLoss=3s' \
    'AddressRandomization=network' \
    'AddressOverride=1e:dc:46:00:66:1b' \
    'restore.sh'
do
    if ! grep -Fq "$statement" network/README.md; then
        echo "FAIL network README omits current deployment policy: $statement" >&2
        fail=1
    fi
done
```

- [ ] **Step 2: Run the test and verify RED**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: nonzero exit for missing detailed Wi-Fi policy statements; the current-source legacy check
remains green because Task 1 already replaced the retired deployment text.

- [ ] **Step 3: Add Wi-Fi behavior documentation**

Insert before `## Reconciliation and AP changes` in `network/README.md`:

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

- [ ] **Step 4: Replace the migration and rollback section**

Replace `## Migration and rollback` and its contents with:

```markdown
## Deployment

`restore.sh` is the only repository deployment entry point. With `GUI=1`, it installs the iwd,
networkd, udev, and systemd configuration, removes obsolete installed repository files, validates
SmartDNS before replacement, and reloads configuration without taking ownership of tunnel recovery.
There is no supported rollback to the retired pre-iwd network stack.

For a focused network-only update, copy the changed repository files to their matching `/etc` paths,
remove obsolete installed files explicitly, and use `networkctl reload`. Do not restart iwd,
systemd-networkd, Tailscale, SmartGateAgent, or SmartDNS merely to apply the Wi-Fi roaming policy.
Installed pre-iwd files under `/etc` are intentionally left for a separate inventoried cleanup so the
wired 802.1X credential path is not removed accidentally.
```

Ensure no current README text names the deleted migration, netctl, dhcpcd, or ncswitch paths. Historical files under `docs/superpowers/plans/` remain unchanged.

- [ ] **Step 5: Run documentation and current-source checks**

Run:

```bash
bash network/test-static-policy.sh
```

Expected: exit 0, including the current-source legacy absence check.

- [ ] **Step 6: Commit current documentation**

```bash
git add network/README.md network/test-static-policy.sh
git commit -m "docs: make restore the sole network deployment path"
```

## Task 5: Apply the local Tencent MAC override without disconnecting Wi-Fi

**Files:**
- Modify locally only: `/var/lib/iwd/Tencent-WiFi.8021x`
- Do not create a repository copy.

- [ ] **Step 1: Confirm the current network is not Tencent-WiFi**

Run:

```bash
iwctl station wlan0 show | sed -n '/Connected network/p'
```

Expected before proceeding: the connected network is not `Tencent-WiFi`. If it is, stop and wait for a safe maintenance window.

- [ ] **Step 2: Create a root-only backup**

Run:

```bash
sudo -n cp -a /var/lib/iwd/Tencent-WiFi.8021x \
    /var/lib/iwd/Tencent-WiFi.8021x.before-android-mac
sudo -n chmod 600 /var/lib/iwd/Tencent-WiFi.8021x.before-android-mac
```

Expected: source and backup are root-owned and mode `600`.

- [ ] **Step 3: Atomically add exactly one override without displaying secrets**

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

Expected: exit 0. This remains a one-time local administration command, not a new script.

- [ ] **Step 4: Verify only mode and the exact setting**

Run:

```bash
sudo -n test "$(stat -c %a /var/lib/iwd/Tencent-WiFi.8021x)" = 600
sudo -n test "$(grep -Fxc 'AddressOverride=1e:dc:46:00:66:1b' \
    /var/lib/iwd/Tencent-WiFi.8021x)" = 1
```

Expected: both commands exit 0 and print nothing.

- [ ] **Step 5: Verify no credential copy appeared in git state**

Run:

```bash
if git status --short --untracked-files=all | grep -E 'Tencent-WiFi.*\.8021x'; then
    echo 'FAIL Tencent credential material appeared in the repository' >&2
    exit 1
fi
```

Expected: exit 0 with no output.

## Task 6: Run complete verification and deploy only changed network files

**Files:**
- No new source files.
- Installed files under `/etc/iwd` and `/etc/systemd/network` are deployment artifacts.

- [ ] **Step 1: Run all repository network checks**

```bash
bash network/test-ip-override.sh
sudo -n bash network/test-reconfigure.sh
bash network/test-static-policy.sh
bash network/test-debug-capture.sh
```

Expected: every command exits 0. Namespace tests may mutate only their isolated namespace, never the live network.

- [ ] **Step 2: Run syntax and whitespace checks**

```bash
bash -n restore.sh network/test-static-policy.sh
fish -n .config/fish/conf.d/completions.fish
git diff --check
```

Expected: all commands exit 0 with no output.

- [ ] **Step 3: Install only changed repository configuration**

```bash
sudo -n cp network/iwd/main.conf /etc/iwd/main.conf
sudo -n cp network/systemd-network/25-wireless.network \
    /etc/systemd/network/25-wireless.network
sudo -n rm -f /etc/systemd/network/26-wireless-tencent.network
sudo -n networkctl reload
```

Expected: exit 0. Do not restart networkd, iwd, tailscaled, SmartGateAgent, or SmartDNS.

- [ ] **Step 4: Verify installed bytes and active networkd file**

```bash
sudo -n cmp -s network/iwd/main.conf /etc/iwd/main.conf
sudo -n cmp -s network/systemd-network/25-wireless.network \
    /etc/systemd/network/25-wireless.network
sudo -n test ! -e /etc/systemd/network/26-wireless-tencent.network
networkctl status wlan0 --no-pager | grep -F '25-wireless.network'
```

Expected: all commands exit 0. The active connection remains up; iwd's global policy waits for its next natural start.

- [ ] **Step 5: Run fresh post-deployment verification**

```bash
bash network/test-static-policy.sh
git status --short
git --no-pager log -6 --oneline
```

Expected: tests exit 0; no network-task files are uncommitted; pre-existing unrelated user edits remain untouched.

## Task 7: Validate on natural network events

**Files:**
- No changes.

- [ ] **Step 1: Validate the next natural Tencent-WiFi connection**

```bash
iwctl station wlan0 show
ip -brief link show wlan0
ip -4 route show table main default dev wlan0
networkctl status wlan0 --no-pager
```

Expected: SSID `Tencent-WiFi`, MAC `1e:dc:46:00:66:1b`, a physical DHCP default route, and `25-wireless.network`.

- [ ] **Step 2: Validate leaving Tencent-WiFi**

After naturally connecting to another SSID:

```bash
ip -brief link show wlan0
```

Expected: the MAC is no longer `1e:dc:46:00:66:1b`.

- [ ] **Step 3: Validate the next natural meeting-room roam**

```bash
journalctl -b -u iwd -u systemd-networkd -u tailscaled --since '-10 minutes' --no-pager
```

Expected for a carrier gap shorter than three seconds: iwd reports the BSSID transition, networkd does not report `DHCP lease lost`, tailscaled does not report `defaultRoute=""`, and exit-node traffic remains usable without `tailscale down`. A carrier loss longer than three seconds may correctly tear down DHCP state.
