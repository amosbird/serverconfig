function vmstat
  if command -s vmstat > /dev/null
    if isatty 1
      grc (command -s vmstat) $argv
    else
      command vmstat $argv
    end
  else
    echo fish: Unknown command \'vmstat\'
  end
end
