function uptime
  if command -s uptime > /dev/null
    if isatty 1
      grc (command -s uptime) $argv
    else
      command uptime $argv
    end
  else
    echo fish: Unknown command \'uptime\'
  end
end
