stty -ixon 2> /dev/null
set fish_color_command --bold
set fish_greeting
set FISH_CLIPBOARD_CMD cat

set FZF_HOME $HOME/.fzf
set -x CORES (getconf _NPROCESSORS_ONLN)
set -x MAKEFLAGS -j$CORES

set -x RUST_SRC_PATH /home/amos/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
set -x FZF_DEFAULT_OPTS "--ansi --multi --bind=ctrl-v:half-page-down,alt-v:half-page-up,ctrl-l:accept"
set -x GOPATH /home/amos/go
set -x SHELL /bin/bash
set -x CARGO_HOME $HOME/.cargo
set -x CABAL_HOME $HOME/.cabal
set -x PYENV_ROOT $HOME/.pyenv
set -x N_HOME $HOME/.n
set -x ARCANIST_HOME $HOME/git/arcanist
set -x EDITOR vim
set -x VISUAL vim
set -x GTK_IM_MODULE fcitx
set -x XMODIFIERS @im=fcitx
set -x QT_IM_MODULE fcitx
set -x ASAN_OPTIONS "handle_segv=0:detect_leaks=0"
set -x USE_GOLD_LINKER true
set -x DIRENV_LOG_FORMAT ""
set -x UID (id -u)
set -x GPG_TTY (tty)

set -e LS_COLORS
alias l "exa"

status --is-interactive;
and function __jump_add --on-variable PWD
  status --is-command-substitution; and return
  jump chdir
end

status --is-interactive;
and function __direnv_export_eval --on-event fish_prompt;
  eval (direnv export fish);
end

set -x ASDF_DIR $HOME/.asdf
set -l asdf_data_dir (if test -n "$ASDF_DATA_DIR";
    echo $ASDF_DATA_DIR;
else;
    echo $HOME/.asdf;
end)

status --is-interactive;
and source $ASDF_DIR/completions/asdf.fish

if not set -q fish_initialized
    set -U fish_color_autosuggestion '555'  'brblack'
    set -U fish_color_cancel -r
    set -U fish_color_comment red
    set -U fish_color_cwd green
    set -U fish_color_cwd_root red
    set -U fish_color_end brmagenta
    set -U fish_color_error brred
    set -U fish_color_escape 'bryellow'  '--bold'
    set -U fish_color_history_current --bold
    set -U fish_color_host normal
    set -U fish_color_match --background=brblue
    set -U fish_color_normal normal
    set -U fish_color_operator bryellow
    set -U fish_color_param cyan
    set -U fish_color_quote yellow
    set -U fish_color_redirection brblue
    set -U fish_color_search_match 'bryellow'  '--background=brblack'
    set -U fish_color_selection 'white'  '--bold'  '--background=brblack'
    set -U fish_color_status red
    set -U fish_color_user brgreen
    set -U fish_color_valid_path --underline
    set -U fish_key_bindings fish_default_key_bindings
    set -U fish_pager_color_completion
    set -U fish_pager_color_description 'B3A06D'  'yellow'
    set -U fish_pager_color_prefix 'white'  '--bold'  '--underline'
    set -U fish_pager_color_progress 'brwhite'  '--background=cyan'

    # prepend path
    set -U fish_user_paths $HOME/scripts $ASDF_DIR/bin $ASDF_DIR/shims $asdf_data_dir/shims $FZF_HOME/bin $GOPATH/bin $CARGO_HOME/bin $CABAL_HOME/bin $ARCANIST_HOST/bin $HOME/.local/bin $HOME/bin /usr/local/bin /usr/local/go/bin /usr/bin /bin /sbin /usr/sbin
    set -U fish_initialized
end

contains -- /usr/share/fish/vendor_completions.d $fish_complete_path;
or set -g fish_complete_path $fish_complete_path[1..-2] /usr/share/fish/vendor_completions.d $fish_complete_path[-1]
