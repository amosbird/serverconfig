function lsof
  if command -s lsof > /dev/null
    if isatty 1
      grc (command -s lsof) $argv
    else
      command lsof $argv
    end
  else
    echo fish: Unknown command \'lsof\'
  end
end
