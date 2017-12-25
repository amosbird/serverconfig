function sysctl
  if command -s sysctl > /dev/null
    if isatty 1
      grc (command -s sysctl) $argv
    else
      command sysctl $argv
    end
  else
    echo fish: Unknown command \'sysctl\'
  end
end
