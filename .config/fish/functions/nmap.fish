function nmap
  if command -s nmap > /dev/null
    if isatty 1
      grc (command -s nmap) $argv
    else
      command nmap $argv
    end
  else
    echo fish: Unknown command \'nmap\'
  end
end
