function traceroute
  if command -s traceroute > /dev/null
    if isatty 1
      grc (command -s traceroute) $argv
    else
      command traceroute $argv
    end
  else
    echo fish: Unknown command \'traceroute\'
  end
end
