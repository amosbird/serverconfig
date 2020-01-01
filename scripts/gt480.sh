#!/usr/bin/env bash

# don't forward for myself and use local instead of prefix
termite -t t480 -e "ssh -t t480 /home/amos/scripts/tstart.sh local"
