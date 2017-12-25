function ping
  if command -s ping > /dev/null
    if isatty 1
      grc (command -s ping) $argv
    else
      command ping $argv
    end
  else
    echo fish: Unknown command \'ping\'
  end
end
