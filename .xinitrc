#!/bin/bash

# --------------------------------------------------------------------
# Source files
# --------------------------------------------------------------------

# Load xprofile configuration files
# shellcheck source=/dev/null
[ -r "/etc/xprofile" ] && . "/etc/xprofile"
# shellcheck source=/dev/null
[ -r "$HOME/.xprofile" ] && . "$HOME/.xprofile"
[ -r "$HOME/.Xresources" ] && xrdb -merge "$HOME/.Xresources"

# Give the panel a real mode before Qtile scans outputs. A NULL NVIDIA
# framebuffer leaves Qtile with no screen, so the first MapRequest throws
# and tty1 stays black.
if [ -x "$HOME/scripts/Xsetup" ]; then
    "$HOME/scripts/Xsetup"
fi

exec /usr/bin/qtile start -b x11
