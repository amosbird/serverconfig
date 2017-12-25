function id
  if command -s id > /dev/null
    if isatty 1
      grc (command -s id) $argv
    else
      command id $argv
    end
  else
    echo fish: Unknown command \'id\'
  end
end
