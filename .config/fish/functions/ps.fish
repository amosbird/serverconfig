function ps
  if command -s ps > /dev/null
    if isatty 1
      grc (command -s ps) $argv
    else
      command ps $argv
    end
  else
    echo fish: Unknown command \'ps\'
  end
end
