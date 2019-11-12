function __fish_clickhouse_get_query_tests
    bash -c "cd $CLICKHOUSE_QUERY_TESTS_DIR && command fd '.(sql|sh)\$'"
end

complete -f -c tq -a '(__fish_clickhouse_get_query_tests)'
