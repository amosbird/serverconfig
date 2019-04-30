#!/usr/bin/env bash

url=${1:-$(cat)}
html=$(mktemp)
wget $url -O $html 2>/dev/null
title=$(cat $html | pup -p 'h3 text{}' | python /home/amos/tt.py)
tags=$(cat $html | pup -p 'a[class="btn btn-xs btn-default btn-round text-sm"] text{}' | python /home/amos/ttt.py)
echo "* [[$url][$title]]                $tags"
echo
echo "** Description"
echo
echo "#+BEGIN_EXAMPLE"
cat $html | pup -p 'meta[name="description"] attr{content}'
echo "#+END_EXAMPLE"
echo
echo "** Code"
echo
echo "#+BEGIN_SRC cpp :exports none"
cat $html | pup -p 'script' | rg codeDefinition | sed 's/codeDefinition: //' | sed 's/,],$/]/' | sed "s/'/\"/g" | jq '.[0].defaultCode' | sed 's/\\r\\n/\n/g' | sed 's/"//' | sed 's/"$//'
echo "#+END_SRC"
echo
echo "** Answer"
echo
echo "#+BEGIN_SRC cpp"
cat $html | pup -p 'script' | rg codeDefinition | sed 's/codeDefinition: //' | sed 's/,],$/]/' | sed "s/'/\"/g" | jq '.[0].defaultCode' | sed 's/\\r\\n/\n/g' | sed 's/"//' | sed 's/"$//'
echo "#+END_SRC"
echo
rm $html
