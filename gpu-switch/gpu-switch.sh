#!/bin/bash
# gpu-switch: reads kernel cmdline and activates the corresponding xorg config
# Usage: called by gpu-switch.service at early boot (before display-manager)

CONF_DIR="/home/amos/git/serverconfig/gpu-switch"
TARGET="/etc/X11/xorg.conf.d/10-nvidia-prime.conf"

if grep -qw 'gpu=nvidia' /proc/cmdline; then
    cp "$CONF_DIR/xorg-nvidia-primary.conf" "$TARGET"
    echo "gpu-switch: nvidia primary activated"
else
    cp "$CONF_DIR/xorg-intel-primary.conf" "$TARGET"
    echo "gpu-switch: intel primary activated"
fi
