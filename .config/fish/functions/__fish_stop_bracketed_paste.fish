# Defined in /tmp/fish.539Zj7/__fish_stop_bracketed_paste.fish @ line 2
function __fish_stop_bracketed_paste
	set fish_bind_mode $__fish_last_bind_mode
    set -e __fish_paste_quoted
    commandline | read -z cmdline
    string trim -- $cmdline | perl -pe 'chomp if eof' | read -z cmdline
    set -l x (string sub -l $__fish_amos_cursor $__fish_amos_cmd)
    set -l y (string sub -s (math $__fish_amos_cursor + 1) $__fish_amos_cmd)
    commandline -r "$x$cmdline$y"
    commandline -C (math $__fish_amos_cursor + (string length $cmdline))
    set -e __fish_amos_cmd
    set -e __fish_amos_cursor
    commandline -f force-repaint
end
