#!/usr/bin/env bash

set -eu

if [ "$#" -eq 3 ]; then
    INPUT_LINE_NUMBER=${1:-0}
    CURSOR_LINE=${2:-1}
    CURSOR_COLUMN=${3:-1}
    AUTOCMD_TERMCLOSE_CMD="call cursor(max([0,${INPUT_LINE_NUMBER}-1])+${CURSOR_LINE}, ${CURSOR_COLUMN})"
else
    AUTOCMD_TERMCLOSE_CMD="normal G"
fi

exec nvim \
    -u NONE \
    -c "map <silent> q :qa!<CR>" \
    -c "map <silent> <ESC> :qa!<CR>" \
    -c "noremap <silent> u <C-u>" \
    -c "noremap <silent> d <C-d>" \
    -c "noremap <silent> <nowait> g gg" \
    -c "set shell=bash scrollback=100000 termguicolors laststatus=0 clipboard+=unnamedplus" \
    -c "autocmd TermEnter * stopinsert" \
    -c "autocmd TermClose * ${AUTOCMD_TERMCLOSE_CMD}" \
    -c 'terminal sed </dev/fd/63 -e "s/'$'\x1b'']8;;file:[^\]*[\]//g" && sleep 0.01 && printf "'$'\x1b'']2;"' 63<&0 0</dev/null
