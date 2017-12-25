function i3focus --argument-names 'd' 't'
  if test (count $argv) -ne 2
    echo "Usage: $_ <direction> <filename>"
    return 1
  end
  set max 74994
  set esc "\033]52;z;"(echo $d | base64 | tr -d '\r\n')"\07"
  set esc "\033Ptmux;\033"$esc"\033\\" # tmux
  printf "$esc" > $t
end
