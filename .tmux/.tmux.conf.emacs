#' -*- mode:conf -*-

set -s mouse off
set -s prefix None
set -s prefix2 None
set -s status off
set -s renumber-windows on

set -s key-table emacs
bind-key -T emacs M-0 switch-client -t amos
bind-key -T emacs M-= switch-client -t htop
