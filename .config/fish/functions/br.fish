function br --wraps=broot
    broot --set-install-state installed &> /dev/null
    set -l cmd_file (mktemp)
    if broot --outcmd $cmd_file $argv
        read --local --null cmd < $cmd_file
        rm -f $cmd_file
        eval $cmd
        if count $TMUX > /dev/null
            tmux refresh-client -S
        end
    else
        set -l code $status
        rm -f $cmd_file
        return $code
    end
end
