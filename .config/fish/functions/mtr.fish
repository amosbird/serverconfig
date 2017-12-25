function mtr
  if command -s mtr > /dev/null
    if isatty 1
      grc (command -s mtr) $argv
    else
      command mtr $argv
    end
  else
    echo fish: Unknown command \'mtr\'
  end
end
