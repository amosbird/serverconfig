function ss
  if command -s ss > /dev/null
    if isatty 1
      grc (command -s ss) $argv
    else
      command ss $argv
    end
  else
    echo fish: Unknown command \'ss\'
  end
end
