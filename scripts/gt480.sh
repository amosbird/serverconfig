#!/usr/bin/env bash

# don't forward for myself
termite --hold -t t480 -e "ssh -t t480 /home/amos/scripts/tstart.sh"
