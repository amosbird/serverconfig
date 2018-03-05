function perfflametmux
    # set -l options -x 'f,F' -x 'F,s' 'h/help' 'f/file' 'F/fifo' 's/suffix=' 'T-testing'
    # argparse -n psub --max-args=0 $options -- $argv
    argparse -n perfflame 'h/help' 'p=' -- $argv
    or return
    if set -q _flag_h
        echo 'Usage: perfflame [-h|--help] ...'
        return
    end

    if math "1 <= "$_flag_p > /dev/null ^ /dev/null
        mkdir -p ~/perfdata
        pushd ~/perfdata
        set -l df (mktemp (date '+%Y-%m-%d-%H_%M_%S_')XXX.data)
        set -l ds (basename -s .data $df).svg
        set -l cols (tput cols)
        set -l rows (tput lines)
        set -l msg "Start recording process "$_flag_p". Hit Ctrl-C to finish."
        set -l centercol (math \({$cols}-(string length $msg)\)/2)
        set -l centercol2 (math \({$cols}-8\)/2)
        set -l centerrow (math $rows/2)
        tput cup $centerrow $centercol
        set -l msg \e\[1m\e\[92m$msg\e\[0m
        set -l clock \e\[1m\e\[33m(string repeat -n $centercol2 " ")
        echo $msg
        echo
        sw $clock $df $ds perf record -F 99 -g -p $_flag_p -o $df ^ /dev/null
        # warning: this never runs. The script should be driven by tmux
        popd
    end
end
