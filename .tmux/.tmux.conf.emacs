#' -*- mode:conf -*-

set -s mouse off
set -s prefix None
set -s prefix2 None
set -s status off
set -s renumber-windows on

set -s key-table emacs

bind-key -T emacs M-0 switch-client -t amos\; run-shell -t amos "fish -c 'setcursor.sh (tmux display -p \"#{pane_tty}\")'"
bind-key -T emacs M-= switch-client -t htop\; run-shell -t amos "fish -c 'setcursor.sh (tmux display -p \"#{pane_tty}\")'"

bind-key -T emacs C-^ previous-window\; send f12
bind-key -T emacs C-_ next-window\; send f12
bind-key -T emacs M-1 select-window -t 1\; send f12
bind-key -T emacs M-2 select-window -t 2\; send f12
bind-key -T emacs M-3 select-window -t 3\; send f12
bind-key -T emacs M-4 select-window -t 4\; send f12
bind-key -T emacs M-5 select-window -t 5\; send f12
bind-key -T emacs M-6 select-window -t 6\; send f12
bind-key -T emacs M-7 select-window -t 7\; send f12
bind-key -T emacs M-8 select-window -t 8\; send f12
bind-key -T emacs M-9 select-window -t 9\; send f12

