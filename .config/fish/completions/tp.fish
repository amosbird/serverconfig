function __fish_clickhouse_get_performance_tests
    bash -c "cd $CLICKHOUSE_PERF_TESTS_DIR && command fd '.xml\$'"
end

complete -f -c tp -a '(__fish_clickhouse_get_performance_tests)'
