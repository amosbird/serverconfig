#!/usr/bin/env bash

sudo ip rule add iif virbr0 ipproto tcp table vms
sudo ip a a 10.10.10.1 dev tun0
sudo ip l s tun0 up
sudo ip r a default dev tun0 table vms
