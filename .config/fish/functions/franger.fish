function franger -d ""
  set RANGER_PID (tmux list-panes -s -F '#{pane_pid}' -t ranger 2> /dev/null)
  if not test -z $RANGER_PID
    # Leave the current cwd for ranger to read and cleanup.
    pwd > /tmp/franger-cwd-$UID
    # Tell ranger to read zsh's cwd from /tmp and cd to it.
    kill -SIGUSR1 $RANGER_PID
  else
    tmux new -d -s ranger 'exec ranger --cmd="set preview_images=false"'
  end
  tmux swap-pane -s ranger:1.1
  # A second check needed because the process could have been
  # started or stopped in the meantime.
  set RANGER_PID (tmux list-panes -s -F '#{pane_pid}' -t ranger 2> /dev/null)
  if not test -z $RANGER_PID
    cd /proc/$RANGER_PID/cwd
  end
end
