#!/usr/bin/env bash

/home/amos/bin/pbpst -P https://la.wentropy.com -Sf "$1" | tail -1 | awk '{print $1".png"}' | copyq add - && copyq select 0
notify-send -a "teiler" "Image Uploaded" "$(copyq clipboard)";
