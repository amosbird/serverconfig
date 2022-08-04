#!/usr/bin/env bash

sudo ip rule add iif virbr0 ipproto tcp table vms
sudo ip r a default dev tun0 table vms
