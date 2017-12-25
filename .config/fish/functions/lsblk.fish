function lsblk
  if command -s lsblk > /dev/null
    if isatty 1
      grc (command -s lsblk) $argv
    else
      command lsblk $argv
    end
  else
    echo fish: Unknown command \'lsblk\'
  end
end
