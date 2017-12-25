function dig
  if command -s dig > /dev/null
    if isatty 1
      grc (command -s dig) $argv
    else
      command dig $argv
    end
  else
    echo fish: Unknown command \'dig\'
  end
end
