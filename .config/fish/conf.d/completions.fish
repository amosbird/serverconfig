function __fish_clickhouse_get_query_tests
    bash -c "cd $CLICKHOUSE_QUERY_TESTS_DIR && command fd '.(sql|sh)\$'"
end

function __fish_clickhouse_get_performance_tests
    bash -c "cd $CLICKHOUSE_PERF_TESTS_DIR && command fd '.xml\$'"
end

function __fish_netctl_get_profiles
    command netctl list | sed -e 's/^[ \t*]*//'
end

function __fish_complete_user_pids -d "Print a list of user process identifiers along with brief descriptions"
    # This may be a bit slower, but it's nice - having the tty displayed is really handy
    # 'tail -n +2' deletes the first line, which contains the headers
    #  $fish_pid is removed from output by string match -r -v

    # Display the tty if available
    # But not if it's just question marks, meaning no tty
    ps -u $UID -o pid,args | string match -r -v '^\s*'$fish_pid'\s' | tail -n +2 | string replace -r ' *([0-9]+) +([^ ].*[^ ]|[^ ]) +([^ ]+) *$' '$1\t$2 [$3]' | string replace -r ' *\[\?*\] *$' ''
end

complete -c repoadd -w emerge
complete -f -c ncswitch -a '(__fish_netctl_get_profiles)'

complete -f -c topnet -a '(__fish_complete_pids)'
complete -f -c topfiles -a '(__fish_complete_pids)'

complete -f -c tmuxperftop2 -a '(__fish_complete_user_pids)'
complete -f -c procenv -a '(__fish_complete_user_pids)'
complete -f -c limits -a '(__fish_complete_user_pids)'
complete -f -c heapstat -a '(__fish_complete_user_pids)'
complete -f -c btpid -a '(__fish_complete_user_pids)'

complete -f -c tq -a '(__fish_clickhouse_get_query_tests)'
complete -f -c tp -a '(__fish_clickhouse_get_performance_tests)'
complete -f -c tp1 -a '(__fish_clickhouse_get_performance_tests)'

complete -x -c tmuxgdb -s p -a '(__fish_complete_user_pids)'
complete -x -c strace -s p -a '(__fish_complete_user_pids)'
complete -x -c perfflametmux -s p -a '(__fish_complete_user_pids)'
complete -x -c perfflame -s p -a '(__fish_complete_user_pids)'
complete -x -c perf -s p -a '(__fish_complete_user_pids)'
complete -x -c gdb -s p -a '(__fish_complete_user_pids)'
complete -x -c cgdb -s p -a '(__fish_complete_user_pids)'
