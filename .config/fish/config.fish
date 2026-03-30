stty -ixon 2>/dev/null
set fish_greeting
set FISH_CLIPBOARD_CMD cat

set fish_color_autosuggestion 555 brblack
set fish_color_cancel -r
set fish_color_command --bold
set fish_color_comment red
set fish_color_cwd green
set fish_color_cwd_root red
set fish_color_end brmagenta
set fish_color_error brred
set fish_color_escape bryellow --bold
set fish_color_history_current --bold
set fish_color_host normal
set fish_color_host_remote yellow
set fish_color_match --background=brblue
set fish_color_normal normal
set fish_color_operator bryellow
set fish_color_param cyan
set fish_color_quote yellow
set fish_color_redirection brblue
set fish_color_search_match bryellow --background=brblack
set fish_color_selection white --bold --background=brblack
set fish_color_status red
set fish_color_user brgreen
set fish_color_valid_path --underline
set fish_pager_color_completion
set fish_pager_color_description B3A06D yellow
set fish_pager_color_prefix white --bold --underline
set fish_pager_color_progress brwhite --background=cyan
set fish_pager_color_selected_background -r

set -x CORES (getconf _NPROCESSORS_ONLN)
set -x MAKEFLAGS -j$CORES
set -x FZF_DEFAULT_OPTS "--ansi --multi --bind=ctrl-v:half-page-down,alt-v:half-page-up,ctrl-l:accept"
set -x SHELL /bin/bash
set -x EDITOR eee
set -x VISUAL eee
set -x DIRENV_LOG_FORMAT ""
set -x GPG_TTY (tty)

set -e LS_COLORS
alias l eza
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
