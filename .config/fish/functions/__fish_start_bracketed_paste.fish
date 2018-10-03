# Defined in /tmp/fish.xqdJSX/__fish_start_bracketed_paste.fish @ line 2
function __fish_start_bracketed_paste
	set -g __fish_last_bind_mode $fish_bind_mode
    # If the token is currently single-quoted,
    # we escape single-quotes (and backslashes).
    __fish_commandline_is_singlequoted
    and set -g __fish_paste_quoted 1
    set -g __fish_amos_cmd (commandline)
    set -g __fish_amos_cursor (commandline -C)
    commandline -r ""
end
