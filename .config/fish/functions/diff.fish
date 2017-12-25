function diff
  if command -s diff > /dev/null
    if isatty 1
      grc (command -s diff) $argv
    else
      command diff $argv
    end
  else
    echo fish: Unknown command \'diff\'
  end
end
