function iostat
  if command -s iostat > /dev/null
    if isatty 1
      grc (command -s iostat) $argv
    else
      command iostat $argv
    end
  else
    echo fish: Unknown command \'iostat\'
  end
end
