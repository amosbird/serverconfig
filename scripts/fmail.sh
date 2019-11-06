#!/usr/bin/env bash

parallel -j 4 getmail -r ::: ict gmail
