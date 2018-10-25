# Defined in /tmp/fish.cOAXXK/edit_command_buffer.fish @ line 2
function edit_command_buffer --description 'Edit the command buffer in an external editor'
	set -l f (mktemp)
    if set -q f[1]
        mv $f $f.fish
        set f $f.fish
    else
        # We should never execute this block but better to be paranoid.
        if set -q TMPDIR
            set f $TMPDIR/fish.$fish_pid.fish
        else
            set f /tmp/fish.$fish_pid.fish
        end
        touch $f
        or return 1
    end
    commandline -b >$f
    __fish_disable_bracketed_paste
    editor $f (commandline -C)
    set ret $status
    __fish_enable_bracketed_paste
    if test $ret -eq 0
        if test -s $f
            commandline -r -- (cat $f)
        else
            commandline -r ""
        end
        commandline -C 999999
    end
    commandline -f repaint
    command rm $f
end
