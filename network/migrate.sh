#!/usr/bin/env bash
# Migrate from netctl+dhcpcd+wpa_supplicant to iwd+systemd-networkd
#
# This script:
# 1. Installs new configs (non-destructive)
# 2. Converts netctl Wi-Fi profiles to iwd format
# 3. Does NOT stop current networking — you run the "activate" step manually
#
# Usage:
#   sudo bash network/migrate.sh install    # deploy configs (safe, no network change)
#   sudo bash network/migrate.sh activate   # switch over (will briefly drop network)
#   sudo bash network/migrate.sh rollback   # revert to netctl

set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
BACKUP_DIR="/var/lib/network-migration-backup"

install_configs() {
    echo "=== Installing new network configs (no network change yet) ==="

    # Backup current state
    mkdir -p "$BACKUP_DIR"
    cp -a /etc/smartdns/smartdns.conf "$BACKUP_DIR/smartdns.conf.bak" 2>/dev/null || true
    cp -a /etc/resolv.conf "$BACKUP_DIR/resolv.conf.bak" 2>/dev/null || true
    ip rule save > "$BACKUP_DIR/ip-rules.bak" 2>/dev/null || true
    iptables-save > "$BACKUP_DIR/iptables.bak" 2>/dev/null || true

    # Install iwd config
    mkdir -p /etc/iwd
    cp "$DIR/iwd/main.conf" /etc/iwd/main.conf
    echo "  [ok] /etc/iwd/main.conf"

    # Install iwd network profiles
    mkdir -p /var/lib/iwd
    install_iwd_profiles
    echo "  [ok] iwd profiles in /var/lib/iwd/"

    # Install systemd-networkd configs
    mkdir -p /etc/systemd/network
    cp "$DIR/systemd-network/"*.network /etc/systemd/network/
    echo "  [ok] /etc/systemd/network/*.network"

    # Install wpa_supplicant@ override
    mkdir -p /etc/systemd/system/wpa_supplicant@.service.d
    cp "$DIR/systemd/wpa_supplicant@.service.d/override.conf" \
       /etc/systemd/system/wpa_supplicant@.service.d/override.conf
    echo "  [ok] wpa_supplicant@ override"

    # Install udev rule
    cp "$DIR/udev/90-wired-8021x.rules" /etc/udev/rules.d/90-wired-8021x.rules
    echo "  [ok] /etc/udev/rules.d/90-wired-8021x.rules"

    # Backup SmartDNS config (don't overwrite yet — applied on activate)
    cp "$DIR/smartdns/smartdns.conf" "$BACKUP_DIR/smartdns-new.conf"
    echo "  [ok] SmartDNS configs staged in $BACKUP_DIR (applied on activate)"



    # Install scripts to ~/scripts (already in PATH via symlink)
    # network-reconfigure and network-fallback live in scripts/

    # Install systemd units
    cp "$DIR/systemd/network-reconfigure.path" /etc/systemd/system/
    cp "$DIR/systemd/network-reconfigure.service" /etc/systemd/system/
    cp "$DIR/systemd/network-fallback.service" /etc/systemd/system/
    echo "  [ok] systemd units"


    # Reload udev and systemd
    udevadm control --reload-rules
    systemctl daemon-reload
    echo ""
    echo "=== Install complete. Run 'sudo bash network/migrate.sh activate' to switch. ==="
}

install_iwd_profiles() {
    # EAP-TLS profiles are pre-deployed to:
    #   /var/lib/iwd/Tencent-WiFi.8021x (iwd wifi)
    #   /etc/wpa_supplicant/wpa_supplicant-wired.conf (wired 802.1X)
    # They are NOT managed by this script (contain credentials, deployed manually).
    if [ -f /var/lib/iwd/Tencent-WiFi.8021x ]; then
        echo "  [ok] /var/lib/iwd/Tencent-WiFi.8021x (pre-existing)"
    else
        echo "  [WARN] /var/lib/iwd/Tencent-WiFi.8021x missing — Tencent-WiFi won't work"
    fi
    if [ -f /etc/wpa_supplicant/wpa_supplicant-wired.conf ]; then
        echo "  [ok] /etc/wpa_supplicant/wpa_supplicant-wired.conf (pre-existing)"
    else
        echo "  [WARN] /etc/wpa_supplicant/wpa_supplicant-wired.conf missing — wired 802.1X won't work"
    fi

    # Credential files are secrets: iwd refuses a group/world-readable .psk,
    # and the 802.1X configs carry a private key passphrase. Report anything
    # left readable rather than silently tightening files we did not create.
    local loose
    loose=$(find /var/lib/iwd /etc/wpa_supplicant -maxdepth 1 -type f \
                 ! -name '*.open' ! -perm 600 2>/dev/null)
    if [ -n "$loose" ]; then
        echo "  [WARN] credential files are not mode 600:"
        echo "$loose" | sed 's/^/           /'
        echo "         fix with: sudo chmod 600 <file>"
    fi

    # Convert netctl WPA-PSK profiles to iwd .psk format
    for f in /etc/netctl/wlp0s20f3-*; do
        [ -f "$f" ] || continue
        local profile_name
        profile_name="$(basename "$f" | sed 's/^wlp0s20f3-//')"

        local security essid key
        security=$(grep -oP '^Security=\K.*' "$f" 2>/dev/null || echo "")
        essid=$(grep -oP '^ESSID=\K.*' "$f" 2>/dev/null | sed "s/^'//;s/'$//;s/\\\\//g" || echo "")
        key=$(grep -oP '^Key=\K.*' "$f" 2>/dev/null | sed 's/\\//g' || echo "")

        [ -z "$essid" ] && continue

        # Skip EAP profiles (handled separately)
        [[ "$security" == *wpa-config* ]] && continue
        [[ "$security" == *wpa-configsection* ]] && continue

        local iwd_file
        if [ "$security" = "none" ] || [ -z "$security" ]; then
            iwd_file="/var/lib/iwd/${essid}.open"
            cat > "$iwd_file" << EOF
[Settings]
AutoConnect=true
EOF
        elif [ "$security" = "wpa" ] || [ -z "$key" ]; then
            [ -z "$key" ] && continue
            iwd_file="/var/lib/iwd/${essid}.psk"
            # A .psk holds the passphrase in plaintext, and iwd refuses to load
            # a profile that is group/world readable. Create it locked down.
            ( umask 077; cat > "$iwd_file" << EOF
[Security]
Passphrase=$key

[Settings]
AutoConnect=true
EOF
            )
            chmod 600 "$iwd_file"
        fi
    done
}

activate() {
    echo "=== Activating iwd + systemd-networkd (will briefly drop network) ==="

    # Install iwd if needed
    if ! pacman -Q iwd &>/dev/null; then
        pacman -S --noconfirm iwd
    fi


    # Stop old stack
    netctl stop-all 2>/dev/null || true
    systemctl stop dhcpcd 2>/dev/null || true
    systemctl stop dhcpcd@wlan0 2>/dev/null || true
    killall wpa_supplicant 2>/dev/null || true

    # Disable old services
    systemctl disable --now netctl-auto@wlan0.service 2>/dev/null || true
    systemctl disable --now dhcpcd.service 2>/dev/null || true
    # Remove old udev rule
    rm -f /etc/udev/rules.d/90-wired-netctl.rules

    # Enable new stack
    systemctl enable --now iwd.service
    systemctl enable --now systemd-networkd.service
    systemctl enable --now network-reconfigure.path
    systemctl enable --now network-fallback.service

    # Apply SmartDNS config now
    cp "$BACKUP_DIR/smartdns-new.conf" /etc/smartdns/smartdns.conf
    # Both are rewritten by network-reconfigure; seed them so smartdns starts.
    echo '# Not on the office LAN' > /etc/smartdns/office.conf
    touch /etc/smartdns/dhcp-dns.conf

    # Lock resolv.conf
    chattr -i /etc/resolv.conf 2>/dev/null || true
    echo "nameserver 127.0.0.1" > /etc/resolv.conf
    chattr +i /etc/resolv.conf

    # Restart SmartDNS with new config
    systemctl restart smartdns.service

    echo ""
    echo "=== Migration active. Verifying... ==="
    sleep 3
    verify
}

rollback() {
    echo "=== Rolling back to netctl + dhcpcd ==="

    # Stop new stack
    systemctl disable --now iwd.service 2>/dev/null || true
    systemctl disable --now systemd-networkd.service 2>/dev/null || true
    systemctl disable --now network-reconfigure.path 2>/dev/null || true
    systemctl disable --now network-fallback.service 2>/dev/null || true
    rm -f /etc/systemd/system/network-{reconfigure.path,reconfigure.service,fallback.service}

    # Restore the pre-iwd hooks (kept in network/rollback/ for exactly this)
    cp "$DIR/rollback/90-wired-netctl.rules" /etc/udev/rules.d/ 2>/dev/null || true
    cp "$DIR/rollback/90-amos-dhcp" /usr/lib/dhcpcd/dhcpcd-hooks/90-amos 2>/dev/null || true
    rm -f /etc/udev/rules.d/90-wired-8021x.rules

    # Restore SmartDNS
    if [ -f "$BACKUP_DIR/smartdns.conf.bak" ]; then
        cp "$BACKUP_DIR/smartdns.conf.bak" /etc/smartdns/smartdns.conf
        systemctl restart smartdns.service 2>/dev/null || true
    fi

    # Restore resolv.conf
    chattr -i /etc/resolv.conf 2>/dev/null || true
    if [ -f "$BACKUP_DIR/resolv.conf.bak" ]; then
        cp "$BACKUP_DIR/resolv.conf.bak" /etc/resolv.conf
    else
        echo "nameserver 127.0.0.1" > /etc/resolv.conf
    fi

    # Remove new networkd configs (but keep iwd profiles for future use)
    rm -f /etc/systemd/network/{10-ignore-virtual,20-wired,25-wireless,26-wireless-tencent}.network
    rm -f /etc/systemd/system/wpa_supplicant@.service.d/override.conf

    # Reload
    udevadm control --reload-rules
    systemctl daemon-reload

    # Bring interface down so netctl can take over cleanly
    # Note: with iwd stopped, interface reverts to kernel name wlp0s20f3
    ip link set wlan0 down 2>/dev/null || true
    ip link set wlp0s20f3 down 2>/dev/null || true

    echo ""
    echo "=== Rollback complete. Reconnect with: ==="
    echo "    sudo netctl switch-to <profile>"
    echo "    (or: ncswitch <profile>)"
}

verify() {
    local ok=true

    printf "  Interface:    "
    if ip -4 addr show wlan0 | grep -q 'inet '; then
        echo "✓ wlan0 has IPv4"
    else
        echo "✗ no IPv4 on wlan0"
        ok=false
    fi

    printf "  Default route: "
    if ip -4 route show default | grep -q 'default'; then
        echo "✓ $(ip -4 route show default | head -1)"
    else
        echo "✗ no default route"
        ok=false
    fi

    printf "  DNS:          "
    if dig +short +timeout=3 google.com @127.0.0.1 | grep -q .; then
        echo "✓ SmartDNS resolving"
    else
        echo "✗ DNS not working"
        ok=false
    fi

    printf "  iwd:          "
    if iwctl station wlan0 show 2>/dev/null | grep -q "Connected"; then
        echo "✓ connected"
    else
        echo "✗ not connected"
        ok=false
    fi

    if $ok; then
        echo ""
        echo "  === All checks passed ==="
    else
        echo ""
        echo "  === SOME CHECKS FAILED — consider 'sudo bash network/migrate.sh rollback' ==="
    fi
}

case "${1:-}" in
    install)  install_configs ;;
    activate) activate ;;
    rollback) rollback ;;
    verify)   verify ;;
    *)
        echo "Usage: sudo bash $0 {install|activate|rollback|verify}"
        echo ""
        echo "  install   — deploy configs without changing current network"
        echo "  activate  — switch from netctl to iwd+networkd (brief outage)"
        echo "  rollback  — revert to netctl+dhcpcd"
        echo "  verify    — check current network health"
        exit 1
        ;;
esac
