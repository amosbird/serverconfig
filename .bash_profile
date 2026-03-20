# .bash_profile

# Termius auto-launch: Termius sets TERM=vt100 as a trigger signal.
# Fix TERM back to xterm-256color, enter Gentoo Prefix, then start opencode tmux.
if [[ $TERM == "vt100" && -n $SSH_TTY && -z $EPREFIX ]]; then
    export TERM=xterm-256color
    export OPENCODE_AUTO=1
    source ~/tmp/gentoo/login2
    exit
fi

# Get the aliases and functions
if [[ -f ~/.bashrc ]]; then
    . ~/.bashrc
fi

# using startx directly breaks terminal fonts, and dbus is wrong.
# if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
#     exec startx
# fi

# Auto-launch opencode tmux when triggered by Termius (via OPENCODE_AUTO env var)
if [[ -n $OPENCODE_AUTO && -z $TMUX ]]; then
    unset OPENCODE_AUTO
    exec "$HOME/scripts/startopencode-tmux"
fi
