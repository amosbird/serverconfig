function ip
  if command -s ip > /dev/null
    if isatty 1
      grc (command -s ip) $argv
    else
      command ip $argv
    end
  else
    echo fish: Unknown command \'ip\'
  end
end
