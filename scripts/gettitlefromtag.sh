#!/usr/bin/env bash

url=$1
while read x; do
    curl $x 2>/dev/null | rg '<a href="/problems/' | tac | sed -e 's/.*="/** [[https:\/\/leetcode.com/' | sed -e 's=">=][=' | sed -e 's=</a>=]]\n='
done <"${url:-/dev/stdin}"
