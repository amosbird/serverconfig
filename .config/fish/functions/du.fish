function du
  if command -s du > /dev/null
    if isatty 1
      grc (command -s du) $argv
    else
      command du $argv
    end
  else
    echo fish: Unknown command \'du\'
  end
end
