function make
  if command -s make > /dev/null
    if isatty 1
      grc (command -s make) $argv
    else
      command make $argv
    end
  else
    echo fish: Unknown command \'make\'
  end
end
