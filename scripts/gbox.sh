#!/usr/bin/env bash

# don't forward for myself and use local instead of prefix
termite -t box -e "ssh -t box /home/amos/scripts/tstart.sh local"
