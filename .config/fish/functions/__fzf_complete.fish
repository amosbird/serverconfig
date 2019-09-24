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
    if test -n "$cmd_lastw"
        set first (string sub -s 1 -l 1 -- $cmd_lastw)
        if test $first = '"' -o $first = "'"
            set quote $first
            set initial_query --query=(string sub -s 2 -- $cmd_lastw)
        else
            set initial_query --query=$cmd_lastw
        end
    end

    set -l complist (complete -C$cmd)
    set -l result

    # do nothing if there is nothing to select from
    test -z "$complist"; and return

    set -l compwc (echo $complist | wc -w)
    if test $compwc -eq 1
        # if there is only one option dont open fzf
        set result "$complist"
    else
        set -l query
        string join -- \n $complist | sort | uniq \
        | fzf --print-query --cycle --reverse --inline-info --multi --height 40% --reverse --select-1 --exit-0 -i $initial_query \
        | cut -f1 | while read -l r
            # first line is the user entered query
            if test -z "$query"
                set query $r
                # rest of lines are selected candidates
            else
                set result $result $r
            end
        end

        # exit if user canceled
        if test -z "$query" ;and test -z "$result"
            commandline -f repaint
            return
        end

        # if user accepted but no candidate matches, use the input as result
        if test -z "$result"
            set result $query
        end
    end

    if test -n $quote
        for i in (seq (count $result))
            set -l r $result[$i]
            set -l stage1 (string replace -a -- \\ \\\\ $r)
            if [ $i -eq 1 ]
                commandline -t -- $quote(string replace -a -- $quote \\$quote $stage1)
            else
                commandline -t -- (string replace -a -- $quote \\$quote $stage1)
            end
            [ $i -lt (count $result) ]; and commandline -i ' '
        end
    else
        commandline -t -- (string escape -n -- $result)
    end

    commandline -f repaint
end

