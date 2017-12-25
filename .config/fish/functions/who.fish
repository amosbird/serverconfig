function who
  if command -s who > /dev/null
    if isatty 1
      grc (command -s who) $argv
    else
      command who $argv
    end
  else
    echo fish: Unknown command \'who\'
  end
end
