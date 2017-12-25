function gcc
  if command -s gcc > /dev/null
    if isatty 1
      grc (command -s gcc) $argv
    else
      command gcc $argv
    end
  else
    echo fish: Unknown command \'gcc\'
  end
end
