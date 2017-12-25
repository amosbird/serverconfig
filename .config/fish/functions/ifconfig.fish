function ifconfig
  if command -s ifconfig > /dev/null
    if isatty 1
      grc (command -s ifconfig) $argv
    else
      command ifconfig $argv
    end
  else
    echo fish: Unknown command \'ifconfig\'
  end
end
