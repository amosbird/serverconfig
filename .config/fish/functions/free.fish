function free
  if command -s free > /dev/null
    if isatty 1
      grc (command -s free) $argv
    else
      command free $argv
    end
  else
    echo fish: Unknown command \'free\'
  end
end
