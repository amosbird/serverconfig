#!/usr/bin/env bash
# Atomically install a SmartDNS base config and roll it back if the service rejects it.

set -euo pipefail

deploy_smartdns_config_locked() (
    local source_config=$1 smartdns_dir=$2 systemctl_bin=$3
    local preserve_source_metadata=${4:-false}
    local target="$smartdns_dir/smartdns.conf" candidate backup='' had_target=false

    candidate=$(mktemp "$smartdns_dir/.smartdns.conf.candidate.XXXXXX") || {
        echo 'SmartDNS deployment failed to create candidate' >&2
        return 1
    }
    trap 'rm -f "$candidate" ${backup:+"$backup"}' EXIT
    cp "$source_config" "$candidate" || {
        echo 'SmartDNS deployment failed to copy source config' >&2
        return 1
    }

    if $preserve_source_metadata; then
        chmod --reference="$source_config" "$candidate" || {
            echo 'SmartDNS deployment failed to copy source mode' >&2
            return 1
        }
        chown --reference="$source_config" "$candidate" || {
            echo 'SmartDNS deployment failed to copy source ownership' >&2
            return 1
        }
    elif [ -e "$target" ]; then
        chmod --reference="$target" "$candidate" || {
            echo 'SmartDNS deployment failed to copy target mode' >&2
            return 1
        }
        chown --reference="$target" "$candidate" || {
            echo 'SmartDNS deployment failed to copy target ownership' >&2
            return 1
        }
    else
        chown 0:0 "$candidate" || {
            echo 'SmartDNS deployment failed to set candidate ownership' >&2
            return 1
        }
        chmod 644 "$candidate" || {
            echo 'SmartDNS deployment failed to set candidate mode' >&2
            return 1
        }
    fi

    if [ -e "$target" ]; then
        backup=$(mktemp "$smartdns_dir/.smartdns.conf.backup.XXXXXX") || {
            echo 'SmartDNS deployment failed to create backup' >&2
            return 1
        }
        cp -a "$target" "$backup" || {
            echo 'SmartDNS deployment failed to copy backup' >&2
            return 1
        }
        had_target=true
    fi

    mv "$candidate" "$target" || {
        echo 'SmartDNS deployment failed to install candidate' >&2
        return 1
    }
    if "$systemctl_bin" restart smartdns.service &&
       "$systemctl_bin" is-active --quiet smartdns.service; then
        rm -f ${backup:+"$backup"}
        return 0
    fi

    echo 'SmartDNS deployment failed; restoring previous config' >&2
    if $had_target; then
        mv "$backup" "$target" || {
            echo 'SmartDNS deployment failed and previous config could not be restored' >&2
            return 2
        }
    else
        rm -f "$target"
    fi

    if ! "$systemctl_bin" restart smartdns.service; then
        echo 'SmartDNS deployment failed and rollback restart failed' >&2
        return 2
    fi
    if ! "$systemctl_bin" is-active --quiet smartdns.service; then
        echo 'SmartDNS deployment failed and rollback service is inactive' >&2
        return 2
    fi
    echo 'SmartDNS deployment failed; previous config restored, restarted, and active' >&2
    return 1
)

deploy_smartdns_config() (
    local source_config=$1 smartdns_dir=${2:-/etc/smartdns}
    local systemctl_bin=${3:-systemctl}
    local lock_path=${4:-/run/lock/network-reconfigure.lock}

    mkdir -p "$smartdns_dir" || {
        echo 'SmartDNS deployment failed to create config directory' >&2
        return 1
    }
    exec 9>"$lock_path" || {
        echo 'SmartDNS deployment failed to open lock file' >&2
        return 1
    }
    flock 9 || {
        echo 'SmartDNS deployment failed to acquire lock' >&2
        return 1
    }
    deploy_smartdns_config_locked "$source_config" "$smartdns_dir" "$systemctl_bin" || return
)

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    if [ "$#" -ne 1 ]; then
        echo "Usage: $0 SMARTDNS_CONFIG" >&2
        exit 2
    fi
    deploy_smartdns_config "$1" /etc/smartdns systemctl /run/lock/network-reconfigure.lock
fi
