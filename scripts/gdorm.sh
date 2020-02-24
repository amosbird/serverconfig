#!/usr/bin/env bash

# don't forward for myself
termite -t dorm -e "ssh -t dorm /home/amos/scripts/tstart.sh local"
