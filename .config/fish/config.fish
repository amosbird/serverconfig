stty -ixon 2>/dev/null
set fish_color_command --bold
set fish_greeting
set FISH_CLIPBOARD_CMD cat

set -x CORES (getconf _NPROCESSORS_ONLN)
set -x MAKEFLAGS -j$CORES
set -x FZF_DEFAULT_OPTS "--ansi --multi --bind=ctrl-v:half-page-down,alt-v:half-page-up,ctrl-l:accept"
set -x SHELL /bin/bash
set -x EDITOR eee
set -x VISUAL eee
set -x DIRENV_LOG_FORMAT ""
set -x UID (id -u)
set -x GPG_TTY (tty)
# set -x HADOOP_CONF_DIR /tmp/gentoo/etc/hadoop

set -e LS_COLORS
alias l exa
alias declare set
alias vim nvim

status --is-interactive
and function __jump_add --on-variable PWD
    status --is-command-substitution; and return
    jump chdir
end

status --is-interactive
and function __direnv_export_eval --on-event fish_prompt
    if count $TMUX >/dev/null
        tmux refresh-client -S
    end
    eval (direnv export fish)
end

if not set -q fish_initialized
    set -U fish_color_autosuggestion 555 brblack
    set -U fish_color_cancel -r
    set -U fish_color_comment red
    set -U fish_color_cwd green
    set -U fish_color_cwd_root red
    set -U fish_color_end brmagenta
    set -U fish_color_error brred
    set -U fish_color_escape bryellow --bold
    set -U fish_color_history_current --bold
    set -U fish_color_host normal
    set -U fish_color_match --background=brblue
    set -U fish_color_normal normal
    set -U fish_color_operator bryellow
    set -U fish_color_param cyan
    set -U fish_color_quote yellow
    set -U fish_color_redirection brblue
    set -U fish_color_search_match bryellow '--background=brblack'
    set -U fish_color_selection white --bold '--background=brblack'
    set -U fish_color_status red
    set -U fish_color_user brgreen
    set -U fish_color_valid_path --underline
    set -U fish_key_bindings fish_default_key_bindings
    set -U fish_pager_color_completion
    set -U fish_pager_color_description B3A06D yellow
    set -U fish_pager_color_prefix white --bold --underline
    set -U fish_pager_color_progress brwhite '--background=cyan'

    set -U fish_initialized 1
end

set -g fish_user_paths $HOME/scripts $HOME/.emacs.d/bin $HOME/.local/bin $HOME/.npm-packages/bin $HOME/.cargo/bin
contains -- /usr/share/fish/vendor_completions.d $fish_complete_path
or set -g fish_complete_path $fish_complete_path[1..-2] /usr/share/fish/vendor_completions.d $fish_complete_path[-1]

# set -x TERM xterm-256color
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# eval /tmp/gentoo/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

set -gx ATUIN_SESSION (atuin uuid)

function _atuin_preexec --on-event fish_preexec
    if not test -n "$fish_private_mode"
        set -g ATUIN_HISTORY_ID (atuin history start -- "$argv[1]")
    end
end

function _atuin_postexec --on-event fish_postexec
    set -l s $status

    if test -n "$ATUIN_HISTORY_ID"
        ATUIN_LOG=error atuin history end --exit $s -- $ATUIN_HISTORY_ID &>/dev/null &
        disown
    end

    set --erase ATUIN_HISTORY_ID
end
