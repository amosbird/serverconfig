function df
  if command -s df > /dev/null
    if isatty 1
      grc (command -s df) $argv
    else
      command df $argv
    end
  else
    echo fish: Unknown command \'df\'
  end
end
