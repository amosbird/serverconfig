function btpid --argument-names 'pid'
    echo 't a a bt' | gdb -q -nx -p $pid 2>/dev/null | sed -E -ne '/^[#T]|^$/p'
end
