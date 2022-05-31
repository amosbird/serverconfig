#!/usr/bin/env bash

trap 'kill 0' INT

sudo /home/amos/tun2socks-linux-amd64 --device tun://tun0 --proxy http://127.0.0.1:12639 &

sleep 2

sudo ip rule add iif virbr0 ipproto tcp table vms
sudo ip a a 10.10.10.1 dev tun0
sudo ip l s tun0 up
sudo ip r a default dev tun0 table vms

wait
