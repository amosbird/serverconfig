function semanage
  if command -s semanage > /dev/null
    if isatty 1
      grc (command -s semanage) $argv
    else
      command semanage $argv
    end
  else
    echo fish: Unknown command \'semanage\'
  end
end
