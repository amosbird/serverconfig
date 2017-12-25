function cluster -d ""
  tmux neww -n Cluster
  tmux splitw -h -p 50
  tmux send-keys "ssh nobida144" C-m
  tmux splitw -v -p 50
  tmux send-keys "ssh nobida145" C-m
  tmux select-pane -t 1
  tmux send-keys "ssh nobida146" C-m
  tmux splitw -v -p 50
  tmux send-keys "ssh nobida148" C-m
  tmux set synchronize-panes
end
