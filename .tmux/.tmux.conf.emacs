#' -*- mode:conf -*-

set -s mouse off
set -s prefix None
set -s prefix2 None
set -s status off
set -s renumber-windows on

set -s key-table emacs

bind-key -T emacs M-0 switch-client -t amos\; run-shell -t amos '/home/amos/scripts/setcursor.sh $(tmux display -p "#{pane_tty}")'
bind-key -T emacs M-= switch-client -t htop\; run-shell -t htop '/home/amos/scripts/setcursor.sh $(tmux display -p "#{pane_tty}")'

set -s user-keys[0] "\e[70~"
set -s user-keys[1] "\e[71~"
set -s user-keys[2] "\e[72~"
set -s user-keys[3] "\e[73~"
set -s user-keys[4] "\e[74~"
set -s user-keys[5] "\e[75~"
set -s user-keys[6] "\e[76~"
set -s user-keys[7] "\e[77~"
set -s user-keys[8] "\e[78~"

bind-key -T emacs User0 send C-Enter
# bind-key -T emacs User1 send User9
# bind-key -T emacs User2 previous-window\; send f12
bind-key -T emacs User3 previous-window\; send f12
bind-key -T emacs User4 next-window\; send f12

bind-key -T emacs M-1 select-window -t 1\; send f12
bind-key -T emacs M-2 select-window -t 2\; send f12
bind-key -T emacs M-3 select-window -t 3\; send f12
bind-key -T emacs M-4 select-window -t 4\; send f12
bind-key -T emacs M-5 select-window -t 5\; send f12
bind-key -T emacs M-6 select-window -t 6\; send f12
bind-key -T emacs M-7 select-window -t 7\; send f12
bind-key -T emacs M-8 select-window -t 8\; send f12
bind-key -T emacs M-9 select-window -t 9\; send f12
