set fish_color_command --bold
set fish_greeting
set FISH_CLIPBOARD_CMD cat

set FZF_HOME $HOME/.fzf
# if set -q TMUX
#   set FZF_TMUX 1
# end

set -x RUST_SRC_PATH /home/amos/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
# set -x TERMINFO $HOME/share/terminfo
set -x C_INCLUDE_PATH /home/amos/include $C_INCLUDE_PATH
set -x CPLUS_INCLUDE_PATH /home/amos/include $CPLUS_INCLUDE_PATH
set -x LIBRARY_PATH /home/amos/lib $LIBRARY_PATH
set -x FZF_DEFAULT_OPTS "--ansi --multi --bind=ctrl-v:half-page-down,alt-v:half-page-up,ctrl-l:accept"
set -x GOPATH $HOME/go
set -x CARGO_HOME $HOME/.cargo
set -x CABAL_HOME $HOME/.cabal
set -x EDITOR vim
set -x VISUAL vim
set -x GTAGSFORCECPP true
set -x GTK_IM_MODULE fcitx
set -x XMODIFIERS @im=fcitx
set -x QT_IM_MODULE fcitx
set -x ASAN_OPTIONS "handle_segv=0:detect_leaks=0"
set -x USE_GOLD_LINKER true
set -x DIRENV_LOG_FORMAT ""
set -x FLAMEGRAPH_DIR /home/amos/softwares/FlameGraph
set -x UID (id -u)

status --is-interactive;
and function __jump_add --on-variable PWD
  status --is-command-substitution; and return
  jump chdir
end

status --is-interactive;
and function __direnv_export_eval --on-event fish_prompt;
  eval (direnv export fish);
end


# doesn't work
# ulimit tcpdump fdisk lsattr mtr

# set -U grc_plugin_execs cvs df diff dig gcc g++ ifconfig make mount netstat ping ps tail traceroute \
# wdiff blkid du dnf docker docker-machine env id ip iostat last lsblk lspci lsmod lsof getfacl getsebool uptime nmap \
# findmnt free semanage sar ss sysctl systemctl stat showmount tune2fs vmstat w who


# for executable in $grc_plugin_execs
#   rm ~/.config/fish/functions/$executable.fish
#   if type -q $executable
#     set cmd (type -p $executable)
#     echo "function $executable" > ~/.config/fish/functions/$executable.fish
#     echo "  if isatty 1"  >> ~/.config/fish/functions/$executable.fish
#     echo "    grc $cmd \$argv" >> ~/.config/fish/functions/$executable.fish
#     echo "  else" >> ~/.config/fish/functions/$executable.fish
#     echo "    $cmd \$argv" >> ~/.config/fish/functions/$executable.fish
#     echo "  end" >> ~/.config/fish/functions/$executable.fish
#     echo "end" >> ~/.config/fish/functions/$executable.fish
#   end
# end

# prepend path
set -U fish_user_paths $HOME/scripts $FZF_HOME/bin $GOPATH/bin $CARGO_HOME/bin $CABAL_HOME/bin $HOME/.local/bin $HOME/bin /bin /sbin

if test -e ~/.local/share/icons-in-terminal/icons.fish
    source ~/.local/share/icons-in-terminal/icons.fish
end
