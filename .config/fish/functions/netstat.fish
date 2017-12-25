function netstat
  if command -s netstat > /dev/null
    if isatty 1
      grc (command -s netstat) $argv
    else
      command netstat $argv
    end
  else
    echo fish: Unknown command \'netstat\'
  end
end
