function tmuxgdb -d ""
  tmux neww -a -n Debug
  tmux set -w pane-border-status off
  tmux splitw -h
  tmux select-pane -t 1
  set tty1 (tmux display -p "#{pane_tty}")

  tmux splitw -v
  tmux send-keys "sleep infinity" C-m
  set tty3 (tmux display -p "#{pane_tty}")
  tmux select-pane -t 1

  tmux splitw -v
  set tty2 (tmux display -p "#{pane_tty}")

  tmux splitw -h
  set tty4 (tmux display -p "#{pane_tty}")
  # tmux send-keys "reptyr -l" C-m
  tmux select-pane -t 5
  tmux select-layout "a37e,211x50,0,0{105x50,0,0[105x18,0,0,339,105x25,0,19{52x25,0,19,342,52x25,53,19,343},105x5,0,45,341],105x50,106,0,340}"
  tmux send-keys "cgdb -- $argv -tty $tty3 -ex \"dashboard threads -output $tty1\" -ex \"dashboard stack -output $tty1\" -ex \"dashboard expression -output $tty4\" -ex \"dashboard registers -output $tty2\"" C-m
end
