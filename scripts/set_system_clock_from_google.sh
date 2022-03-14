#!/usr/bin/env bash

##
# @author Jay Taylor (https://jaytaylor.com)
#
# @date 2018-08-28
#
# @description Sets system time based on what is reported by google.com.  Useful
# for cases when you have a Linux machine on a network which is only able to
# reach the internet through an HTTP proxy.
#
# Inspired by ryenus' answer @ https://superuser.com/a/807326/72342
#

set -o errexit
set -o pipefail
set -o nounset

if [ "${UID:-}" != '0' ]; then
    echo "ERROR: $0 must be run as root" 1>&2
    exit 1
fi

if [ "${1:-}" = '-v' ]; then
    set -o xtrace
    shift
fi

utc_stamp="$( \
    curl --head -sS -H 'Cache-Control: no-cache' 'https://google.com/' \
        | grep '^Date:' \
        | cut -d' ' -f3-6 \
)Z"

if [ "${utc_stamp}" = 'Z' ]; then
    echo 'ERROR: setting system clock aborted due to empty $utc_stamp var value' 1>&2
    exit 1
fi

echo "INFO: setting system clock to: ${utc_stamp}" 1>&2
date -s "${utc_stamp}"
