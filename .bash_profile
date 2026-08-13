# .bash_profile

# Start the local graphical session once per boot. If it exits, stay on tty1 for recovery.
if [[ -z ${SSH_TTY:-} && -z ${DISPLAY:-} && ${XDG_VTNR:-0} == 1 &&
      -n ${XDG_RUNTIME_DIR:-} && ! -e $XDG_RUNTIME_DIR/qtile-autostart-attempted ]]; then
    # shortcut: one attempt per boot; remove the marker or run startx manually to retry
    touch "$XDG_RUNTIME_DIR/qtile-autostart-attempted"
    exec /usr/bin/startx
fi

if [[ -f ~/.bashrc ]]; then
    source ~/.bashrc
fi
