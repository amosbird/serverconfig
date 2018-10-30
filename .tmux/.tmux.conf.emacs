#' -*- mode:conf -*-

set -s mouse off
set -s prefix None
set -s prefix2 None
set -s status off
set -s renumber-windows on
set -s detach-on-destroy off

set -s key-table emacs

bind-key -T emacs M-0 switch-client -t amos
bind-key -T emacs M-= switch-client -t htop

set -s user-keys[0] "\e[70~"
bind-key -T emacs User0 send C-Enter
