#!/usr/bin/env bash

sudo ip l set enp0s26u1u3 up
sudo ip a a 192.168.42.1 dev enp0s26u1u3
sudo ip r a 192.168.42.0/24 dev enp0s26u1u3
