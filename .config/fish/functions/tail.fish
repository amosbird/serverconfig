function tail
  if command -s tail > /dev/null
    if isatty 1
      grc (command -s tail) $argv
    else
      command tail $argv
    end
  else
    echo fish: Unknown command \'tail\'
  end
end
