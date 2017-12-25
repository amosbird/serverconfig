function lspci
  if command -s lspci > /dev/null
    if isatty 1
      grc (command -s lspci) $argv
    else
      command lspci $argv
    end
  else
    echo fish: Unknown command \'lspci\'
  end
end
