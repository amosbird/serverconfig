function osc52browser -d "open url using osc52" --argument-names 'file'
  if test (count $argv) -ne 1
    echo "Usage: $_ <file>"
    return 1
  end
  set addr (ip a s dev zt0 | awk '$1 == "inet" { sub(/\/16/, "", $2); print $2 }')
  set max 74994
  set esc "\033]52;y;"(printf 'http://'$addr':8866'(realpath $file) | head -c $max | base64 | tr -d '\r\n')"\07"
  set esc "\033Ptmux;\033"$esc"\033\\" # tmux
  printf "$esc"
end
