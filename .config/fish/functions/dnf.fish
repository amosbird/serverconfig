function dnf
  if command -s dnf > /dev/null
    if isatty 1
      grc (command -s dnf) $argv
    else
      command dnf $argv
    end
  else
    echo fish: Unknown command \'dnf\'
  end
end
