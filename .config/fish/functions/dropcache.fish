function dropcache
    # set -l options -x 'f,F' -x 'F,s' 'h/help' 'f/file' 'F/fifo' 's/suffix=' 'T-testing'
    # argparse -n psub --max-args=0 $options -- $argv
    argparse -n dropcache 'h/help' -- $argv
    or return
    if set -q _flag_h
        echo 'Usage: dropcache [-h|--help] ...'
        return
    end
    echo 1 | sudo tee /proc/sys/vm/drop_caches > /dev/null
end
