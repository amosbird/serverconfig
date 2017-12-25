function wdiff
  if command -s wdiff > /dev/null
    if isatty 1
      grc (command -s wdiff) $argv
    else
      command wdiff $argv
    end
  else
    echo fish: Unknown command \'wdiff\'
  end
end
