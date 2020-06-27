#!/usr/bin/env bash


for file in MobilePhoneModel PageCharset Params URLDomain UTMSource Referer URL Title; do
# for file in URL Title; do
    # for size in 30000 100000 300000 1000000 5000000; do
    for size in 5000000; do
        echo
        BEST_METHOD=0
        BEST_RESULT=0
        for method in {5..7}; do
            echo -ne $file $size $method '';
            TOTAL_ELEMS=0
            for i in {0..1000}; do
                TOTAL_ELEMS=$(( $TOTAL_ELEMS + $size ))
                if [[ $TOTAL_ELEMS -gt 25000000 ]]; then break; fi
                /mnt/disk5/ClickHouse/build-dev/src/Interpreters/tests/hash_map_string_3 $size $method < ${file}.bin 2>&1 |
                    grep HashMap | grep -oE '[0-9\.]+ elem';
            done | mawk -W interactive '{ if ($1 > x) { x = $1 }; printf(".") } END { print x }' | tee /tmp/hash_map_string_3_res;
            CUR_RESULT=$(cat /tmp/hash_map_string_3_res | tr -d '.')
            if [[ $CUR_RESULT -gt $BEST_RESULT ]]; then
                BEST_METHOD=$method
                BEST_RESULT=$CUR_RESULT
            fi;
        done;
        echo Best: $BEST_METHOD - $BEST_RESULT
    done;
done
