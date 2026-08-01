#!/usr/bin/env bash
# Install a SmartDNS base config only after a disposable foreground instance accepts it.

set -euo pipefail

deploy_smartdns_config() (
    local source_config=$1 smartdns_dir=${2:-/etc/smartdns}
    local smartdns_bin=${3:-smartdns} timeout_bin=${4:-timeout}
    local systemctl_bin=${5:-systemctl} candidate validation validation_port rc

    mkdir -p "$smartdns_dir"
    candidate=$(mktemp "$smartdns_dir/smartdns.conf.XXXXXX")
    validation=$(mktemp "$smartdns_dir/smartdns-validation.conf.XXXXXX")
    trap 'rm -f "$candidate" "$validation"' EXIT

    cp "$source_config" "$candidate"
    validation_port=$((20000 + BASHPID % 30000))
    # shortcut: only the two loopback listeners need isolation; keep the candidate otherwise exact.
    sed -e "s/^bind 127\\.0\\.0\\.1:53$/bind 127.0.0.1:$validation_port/" \
        -e "s/^bind-tcp 127\\.0\\.0\\.1:53$/bind-tcp 127.0.0.1:$validation_port/" \
        "$candidate" >"$validation"

    rc=0
    "$timeout_bin" 2 "$smartdns_bin" -f -x -p - -c "$validation" || rc=$?
    if [ "$rc" -ne 124 ]; then
        echo "SmartDNS validation failed with status $rc; live config was not changed" >&2
        return 1
    fi

    chmod 644 "$candidate"
    mv "$candidate" "$smartdns_dir/smartdns.conf"
    if ! "$systemctl_bin" restart smartdns.service; then
        echo 'SmartDNS config installed, but service restart failed' >&2
        return 1
    fi
    rm -f "$validation"
)

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    if [ "$#" -ne 1 ]; then
        echo "Usage: $0 SMARTDNS_CONFIG" >&2
        exit 2
    fi
    deploy_smartdns_config "$1"
fi
