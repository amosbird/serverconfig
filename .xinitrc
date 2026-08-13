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

exec /usr/bin/qtile start -b x11
