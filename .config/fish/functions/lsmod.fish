function lsmod
  if command -s lsmod > /dev/null
    if isatty 1
      grc (command -s lsmod) $argv
    else
      command lsmod $argv
    end
  else
    echo fish: Unknown command \'lsmod\'
  end
end
