# modified from https://github.com/junegunn/fzf/wiki/Examples-(fish)#completion
function __fzf_complete -d 'fzf completion and print selection back to commandline'
    # As of 2.6, fish's "complete" function does not understand
    # subcommands. Instead, we use the same hack as __fish_complete_subcommand and
    # extract the subcommand manually.
    set -l cmd (commandline -co) (commandline -ct)

    switch $cmd[1]
        case env sudo
            for i in (seq 2 (count $cmd))
                switch $cmd[$i]
                    case '-*'
                    case '*=*'
                    case '*'
                        set cmd $cmd[$i..-1]
                        break
                end
            end
    end

    set -l cmd_lastw $cmd[-1]
    set cmd (string join -- ' ' $cmd)

    set -l initial_query ''
    set -l quote ''
    set -l other ''
    if test -n "$cmd_lastw"
        set first (string sub -s 1 -l 1 -- $cmd_lastw)
        if test $first = '"' -o $first = "'"
            set quote $first
            set initial_query (string unescape -- (string sub -s 2 -- $cmd_lastw))
        else if test $first = '{'
            set other $first
            set initial_query (string unescape -- $cmd_lastw)
        else if test $first = '~'
            set other $first
            set initial_query (string unescape -- $cmd_lastw)
        else
            set initial_query (string unescape -- $cmd_lastw)
        end
    end

    set -l complist (complete -C$cmd)

    # do nothing if there is nothing to select from
    test -z "$complist"; and return
    set -l result

    set -l compwc (count $complist)
    if test $compwc -eq 1
        # if there is only one option dont open fzf
        set result (string split \t -- $complist)[1]
    else
        string join -- \n $complist | rg -v '^\s+|^$' | sort -u |
        fzf --cycle --reverse --inline-info --multi --height 40% --reverse --select-1 --exit-0 -i --query=$initial_query | read -d\n -z -a result
        for i in (seq (count $result))
            set result[$i] (string split \t -- $result[$i])[1]
        end
        set result $result[1..-2]
        if test -z "$result"
            commandline -f repaint
            return
        end
    end

    if test -n $quote
        set -l cmdline
        for i in (seq (count $result))
            set -l r $result[$i]
            set -l stage1 (string replace -a -- \\ \\\\ $r)
            if [ $i -eq 1 ]
                set cmdline $cmdline $quote(string replace -a -- $quote \\$quote $stage1)
            else
                set cmdline $cmdline (string replace -a -- $quote \\$quote $stage1)
            end
        end
        commandline -t -- (string join ' ' -- $cmdline)
    else if test $other = '~'
        for i in (seq (count $result))
            commandline -t -- (string sub -s 2 (string escape -n -- $result[$i]))
            [ $i -lt (count $result) ]; and commandline -i ' '
        end
    else if test $other = '{'
        set -l cmdline
        for i in (seq (count $result))
            # NOTE it breaks when completion contains comma
            set -l r (string escape -n -- (string sub -s 2 $result[$i]))
            if [ $i -eq 1 ]
                set cmdline $cmdline $other$r
            else
                set cmdline $cmdline (string split , -- $r)[-1]
            end
        end
        commandline -t -- (string join ',' -- $cmdline)
    else
        set -l r (string trim -- (string join ' ' -- (string escape -n -- $result)))
        commandline -t -- $r
    end

    commandline -f repaint
end

