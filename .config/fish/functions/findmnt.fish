function findmnt
  if command -s findmnt > /dev/null
    if isatty 1
      grc (command -s findmnt) $argv
    else
      command findmnt $argv
    end
  else
    echo fish: Unknown command \'findmnt\'
  end
end
