function last
  if command -s last > /dev/null
    if isatty 1
      grc (command -s last) $argv
    else
      command last $argv
    end
  else
    echo fish: Unknown command \'last\'
  end
end
