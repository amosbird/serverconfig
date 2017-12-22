#!/usr/bin/env bash

web-ext --source-dir ~/git/BaiduExporter/chrome/ --firefox-profile=/home/amos/.mozilla/firefox/mjgax3c2.default-1459430723151-1505962596288 run --start-url "$(xclip -o)"
