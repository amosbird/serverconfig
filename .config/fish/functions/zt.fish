function zt -d "zerotier routes"
  if ip r | grep -q "0.0.0.0/1 via 172.26.133.192"
    sudo ip r d 0.0.0.0/1
    sudo ip r d 128.0.0.0/1
  else
    sudo ip r a 0.0.0.0/1 via 172.26.133.192
    sudo ip r a 128.0.0.0/1 via 172.26.133.192
  end
end
