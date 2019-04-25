#!/usr/bin/env bash

pdf_file=$1
page=$(($2 + 1)) # The page number star at zero in llpp
x=$3
y=$4

synctex edit -o "$page:$x:$y:$pdf_file" -x "emacsclient +%{line} '%{input}'"
