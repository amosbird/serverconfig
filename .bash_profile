# .bash_profile

# Get the aliases and functions
if [[ -f ~/.bashrc ]]; then
    . ~/.bashrc
fi

# using startx directly breaks terminal fonts, and dbus is wrong.
# if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
#     exec startx
# fi
