#!/usr/bin/env bash

if ip r | fgrep "default via 10.10.10.1 dev tun0" > /dev/null
then
    echo "       VPN        "
fi
