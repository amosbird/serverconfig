function env
  if command -s env > /dev/null
    if isatty 1
      grc (command -s env) $argv
    else
      command env $argv
    end
  else
    echo fish: Unknown command \'env\'
  end
end
