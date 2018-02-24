function tmuxgdb -d "set up tmux panes for gdb."
  tmux neww -a -n Debug "sleep infinity"
  tmux set -w pane-border-status off
  tmux splitw -h
  tmux select-pane -t 1
  set tty1 (tmux display -p "#{pane_tty}")

  tmux splitw -v
  set tty4 (tmux display -p "#{pane_tty}")
  tmux select-pane -t 1

  tmux splitw -v "sleep infinity"
  set tty2 (tmux display -p "#{pane_tty}")

  tmux splitw -h "sleep infinity"
  set tty3 (tmux display -p "#{pane_tty}")

  # tmux list-windows
  tmux select-layout "328c,211x50,0,0{105x50,0,0[105x18,0,0,337,105x18,0,19{52x18,0,19,340,52x18,53,19,341},105x12,0,38,339],105x50,106,0,338}"

  tmux select-pane -t 4
  tmux send-keys "reptyr -l sh -c 'tmux select-pane -t 5; tmux send-keys \"cgdb -- -tty \$REPTYR_PTY -ex \\\"dashboard threads -output $tty1\\\" -ex \\\"dashboard stack -output $tty1\\\" -ex \\\"dashboard expression -output $tty3\\\" -ex \\\"dashboard registers -output $tty2\\\" $argv\" C-m'" C-m
  sleep 0.2
  tmux send -R -t 4
end
