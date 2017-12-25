#!/usr/bin/env bash

if ip r | fgrep "128.0.0.0/1" > /dev/null
then
    echo "       VPN        "
fi
