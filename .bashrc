# .bashrc

if tty_path=$(tty 2>/dev/null); then
    stty -ixon 2>/dev/null
    export GPG_TTY="$tty_path"
fi
unset tty_path

# User specific environment and startup programs
