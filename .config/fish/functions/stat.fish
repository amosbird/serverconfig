function stat
  if command -s stat > /dev/null
    if isatty 1
      grc (command -s stat) $argv
    else
      command stat $argv
    end
  else
    echo fish: Unknown command \'stat\'
  end
end
