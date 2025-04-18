#!/usr/bin/env bash

if ! command -v atuin &> /dev/null
then
    exit 0
fi

if ! command -v atuin-clickhouse &> /dev/null
then
    exit 0
fi

LOCK_FILE="/tmp/atuin_sync.lock"
LOG_FILE="/tmp/atuin_sync.log"

read -r time status <"$LOCK_FILE"

current_time=$(date +%s)
if (( current_time - time < 600 )) && [ "$status" = 0 ]; then
    exit 0;
fi

printf 'history syncing'

exec 200>>"$LOCK_FILE"
if ! flock -n 200; then
    exit 0
fi

exec 1>&- 2>&-

nohup bash -c "atuin sync >/dev/null 2>\"$LOG_FILE\"; atuin-clickhouse sync >/dev/null 2>\"$LOG_FILE\"; s=\$?; echo \$(date +%s) \$s > \"$LOCK_FILE\"" &
