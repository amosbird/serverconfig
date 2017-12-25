function systemctl
  if command -s systemctl > /dev/null
    if isatty 1
      grc (command -s systemctl) $argv
    else
      command systemctl $argv
    end
  else
    echo fish: Unknown command \'systemctl\'
  end
end
