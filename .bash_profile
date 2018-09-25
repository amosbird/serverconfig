# .bash_profile

# Get the aliases and functions
if [[ -f ~/.bashrc ]]; then
    . ~/.bashrc
fi

# if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
#   exec startx
# fi
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
#export SDKMAN_DIR="/home/amos/.sdkman"
#[[ -s "/home/amos/.sdkman/bin/sdkman-init.sh" ]] && source "/home/amos/.sdkman/bin/sdkman-init.sh"
#if [ -e /home/amos/.nix-profile/etc/profile.d/nix.sh ]; then . /home/amos/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
