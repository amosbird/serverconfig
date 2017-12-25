function getsebool
  if command -s getsebool > /dev/null
    if isatty 1
      grc (command -s getsebool) $argv
    else
      command getsebool $argv
    end
  else
    echo fish: Unknown command \'getsebool\'
  end
end
