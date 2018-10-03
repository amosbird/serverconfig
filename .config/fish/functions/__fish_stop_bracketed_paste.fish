# Defined in /tmp/fish.45yWbK/__fish_stop_bracketed_paste.fish @ line 2
function __fish_stop_bracketed_paste
	set fish_bind_mode $__fish_last_bind_mode
    set -e __fish_paste_quoted
    set -l cmdline (string trim -- (commandline))
    test -z "$cmdline[-1]" && set -e cmdline[-1]
    set -l x (string sub -l $__fish_amos_cursor $__fish_amos_cmd)
    set -l y (string sub -s (math $__fish_amos_cursor + 1) $__fish_amos_cmd) 
    echo $cmdline > /tmp/cmd
    commandline -r "$x$cmdline$y"
    commandline -C (math $__fish_amos_cursor + (string length $cmdline))
    set -e __fish_amos_cmd
    set -e __fish_amos_cursor
    commandline -f force-repaint
end
