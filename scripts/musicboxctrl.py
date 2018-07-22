#!/usr/bin/env python

import socket
import sys
server_address = '/tmp/musicbox.s'
sock = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM)
sock.sendto(sys.argv[1].encode(), server_address)
