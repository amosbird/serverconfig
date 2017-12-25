function osc52browser -d "open url using osc52" --argument-names 'file'
  if test (count $argv) -ne 1
    echo "Usage: $_ <file>"
    return 1
  end
  set max 74994
  set esc "\033]52;y;"(printf 'file://'(realpath $file) | head -c $max | base64 | tr -d '\r\n')"\07"
  set esc "\033Ptmux;\033"$esc"\033\\" # tmux
  printf "$esc"
end
