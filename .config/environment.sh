# Common environment shared by the X session and tstart.sh.
export LANG=en_US.UTF-8
export CORES="$(getconf _NPROCESSORS_ONLN)"
export NPROC="$(nproc)"
export MAKEFLAGS="-j$CORES"
export FZF_DEFAULT_OPTS="--ansi --multi --bind=ctrl-v:half-page-down,alt-v:half-page-up,ctrl-l:accept"
export EDITOR=eee
export VISUAL=eee
export DIRENV_LOG_FORMAT=
export MANPATH= # Rely on man_db.conf instead
export LSP_USE_PLISTS=true

if [[ -s /etc/hostname ]]; then
    export HOSTNAME="$(</etc/hostname)"
fi

export SSH_AUTH_SOCK=/tmp/ssh_auth_sock
