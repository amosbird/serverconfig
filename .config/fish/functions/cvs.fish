function cvs
  if command -s cvs > /dev/null
    if isatty 1
      grc (command -s cvs) $argv
    else
      command cvs $argv
    end
  else
    echo fish: Unknown command \'cvs\'
  end
end
