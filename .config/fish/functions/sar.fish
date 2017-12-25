function sar
  if command -s sar > /dev/null
    if isatty 1
      grc (command -s sar) $argv
    else
      command sar $argv
    end
  else
    echo fish: Unknown command \'sar\'
  end
end
