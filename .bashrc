# .bashrc

stty -ixon 2> /dev/null

if [ -n "$DISPLAY" ] || [ -n "$WAYLAND_DISPLAY" ]; then
    export PATH="$HOME/scripts:$HOME/.emacs.d/bin:$HOME/.npm-packages/bin:$HOME/.cargo/bin:$PATH:$HOME/.local/bin:$HOME/.mambatools/bin"
else
    export PATH="$HOME/scripts:$HOME/.emacs.d/bin:$HOME/.local/bin:$HOME/.npm-packages/bin:$HOME/.cargo/bin:$HOME/.mambatools/bin:$PATH"
fi

# User specific environment and startup programs
