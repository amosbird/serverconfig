#' -*- mode:conf -*-

set -s mouse off
set -s prefix None
set -s prefix2 None
set -s status off
set -s renumber-windows on

set -s key-table emacs

bind-key -T emacs M-0 switch-client -t amos\; run-shell -t amos '/home/amos/scripts/setcursor.sh $(tmux display -p "#{pane_tty}")'
bind-key -T emacs M-= switch-client -t htop\; run-shell -t htop '/home/amos/scripts/setcursor.sh $(tmux display -p "#{pane_tty}")'

bind-key -T emacs User0 send C-Enter
