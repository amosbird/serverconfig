#!/bin/bash

# --------------------------------------------------------------------
# Source files
# --------------------------------------------------------------------

# Load xprofile configuration files
# shellcheck source=/dev/null
[ -r "/etc/xprofile" ] && . "/etc/xprofile"
# shellcheck source=/dev/null
[ -r "$HOME/.xprofile" ] && . "$HOME/.xprofile"

exec $HOME/scripts/wm
