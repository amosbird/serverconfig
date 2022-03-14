#!/usr/bin/env bash

proxychains parallel -j 4 getmail -r ::: gmail
