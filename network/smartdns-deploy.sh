#!/usr/bin/env bash
# Atomically install a SmartDNS base config and roll it back if the service rejects it.

set -euo pipefail

deploy_smartdns_config_locked() (
    local source_config=$1 smartdns_dir=$2 systemctl_bin=$3
    local target="$smartdns_dir/smartdns.conf" candidate backup='' had_target=false

    candidate=$(mktemp "$smartdns_dir/.smartdns.conf.candidate.XXXXXX")
    trap 'rm -f "$candidate" ${backup:+"$backup"}' EXIT
    cp "$source_config" "$candidate"

    if [ -e "$target" ]; then
        had_target=true
        chmod --reference="$target" "$candidate"
        chown --reference="$target" "$candidate"
        backup=$(mktemp "$smartdns_dir/.smartdns.conf.backup.XXXXXX")
        cp -a "$target" "$backup"
    else
        chown 0:0 "$candidate"
        chmod 644 "$candidate"
    fi

    mv "$candidate" "$target"
    if "$systemctl_bin" restart smartdns.service &&
       "$systemctl_bin" is-active --quiet smartdns.service; then
        rm -f ${backup:+"$backup"}
        return 0
    fi

    echo 'SmartDNS deployment failed; restoring previous config' >&2
    if $had_target; then
        mv "$backup" "$target"
    else
        rm -f "$target"
    fi

    if ! "$systemctl_bin" restart smartdns.service; then
        echo 'SmartDNS deployment failed and rollback restart failed' >&2
        return 2
    fi
    echo 'SmartDNS deployment failed; previous config restored and restarted' >&2
    return 1
)

deploy_smartdns_config() (
    local source_config=$1 smartdns_dir=${2:-/etc/smartdns}
    local systemctl_bin=${3:-systemctl}
    local lock_path=${4:-/run/lock/network-reconfigure.lock}

    mkdir -p "$smartdns_dir"
    exec 9>"$lock_path"
    flock 9
    deploy_smartdns_config_locked "$source_config" "$smartdns_dir" "$systemctl_bin"
)

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    if [ "$#" -ne 1 ]; then
        echo "Usage: $0 SMARTDNS_CONFIG" >&2
        exit 2
    fi
    deploy_smartdns_config "$1" /etc/smartdns systemctl /run/lock/network-reconfigure.lock
fi
