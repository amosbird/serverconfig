function ztrt
    if ip r | rg '10.60.0.0/16 via 172.26.138.142 dev zt0' > /dev/null
        sudo ip r d 10.60.0.0/16 via 172.26.138.142 dev zt0
        sudo ip r d 10.61.0.0/16 via 172.26.138.142 dev zt0
        sudo ip r d 172.26.72.26 via 172.26.138.142 dev zt0
        sudo ip r d 172.26.133.85 via 172.26.138.142 dev zt0
    else
        sudo ip r a 10.60.0.0/16 via 172.26.138.142 dev zt0
        sudo ip r a 10.61.0.0/16 via 172.26.138.142 dev zt0
        sudo ip r a 172.26.72.26 via 172.26.138.142 dev zt0
        sudo ip r a 172.26.133.85 via 172.26.138.142 dev zt0
    end
end
