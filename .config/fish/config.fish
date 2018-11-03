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

# prepend path
set -U fish_user_paths $HOME/scripts $FZF_HOME/bin $N_HOME/bin $PYENV_ROOT/bin $GOPATH/bin $CARGO_HOME/bin $CABAL_HOME/bin $ARCANIST_HOST/bin $HOME/.local/bin $HOME/bin /usr/local/bin /usr/local/go/bin /usr/bin /bin /sbin /usr/sbin

set -gx PATH '/home/amos/.pyenv/shims' $PATH
set -gx PYENV_SHELL fish
function pyenv
  set command $argv[1]
  set -e argv[1]

  switch "$command"
  case activate deactivate rehash shell
    source (pyenv "sh-$command" $argv|psub)
  case '*'
    command pyenv "$command" $argv
  end
end

# set -gx PATH '/home/amos/.pyenv/plugins/pyenv-virtualenv/shims' $PATH;
# set -gx PYENV_VIRTUALENV_INIT 1;
# function _pyenv_virtualenv_hook --on-event fish_prompt;
#   set -l ret $status
#   if [ -n "$VIRTUAL_ENV" ]
#     pyenv activate --quiet; or pyenv deactivate --quiet; or true
#   else
#     pyenv activate --quiet; or true
#   end
#   return $ret
# end

# status --is-interactive; and source (pyenv init -|psub)
# status --is-interactive; and source (pyenv virtualenv-init -|psub)

[ -s "/home/amos/.jabba/jabba.fish" ]; and source "/home/amos/.jabba/jabba.fish"
