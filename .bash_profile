# .bash_profile

# Start the local graphical session once per boot. If it exits, stay on tty1 for recovery.
if [[ -z ${SSH_TTY:-} && -z ${DISPLAY:-} && ${XDG_VTNR:-0} == 1 &&
      -n ${XDG_RUNTIME_DIR:-} && ! -e $XDG_RUNTIME_DIR/qtile-autostart-attempted ]]; then
    # shortcut: one attempt per boot; remove the marker or run startx manually to retry
    touch "$XDG_RUNTIME_DIR/qtile-autostart-attempted"
    exec /usr/bin/startx
fi

# Login shells need the non-graphical tool path; X sessions set their own order in .xprofile.
export PATH="$HOME/scripts:$HOME/.emacs.d/bin:$HOME/.local/bin:$HOME/.npm-packages/bin:$HOME/.cargo/bin:$HOME/.mambatools/bin:$PATH"

# Termius auto-launch: Termius sets TERM=vt100 as a trigger signal.
# Fix TERM back to xterm-256color, enter Gentoo Prefix, then start opencode tmux.
if [[ $TERM == "vt100" && -n $SSH_TTY && -z $EPREFIX ]]; then
    export TERM=xterm-256color
    export OPENCODE_AUTO=1
    source ~/tmp/gentoo/login2
    exit
fi

# Auto-launch opencode tmux when triggered by Termius (via OPENCODE_AUTO env var)
if [[ -n $OPENCODE_AUTO && -z $TMUX ]]; then
    unset OPENCODE_AUTO
    exec "$HOME/scripts/startopencode-tmux"
fi
