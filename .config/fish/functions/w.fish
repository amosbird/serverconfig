function w
  if command -s w > /dev/null
    if isatty 1
      grc (command -s w) $argv
    else
      command w $argv
    end
  else
    echo fish: Unknown command \'w\'
  end
end
