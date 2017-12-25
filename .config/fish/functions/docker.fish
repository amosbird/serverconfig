function docker
  if command -s docker > /dev/null
    if isatty 1
      grc (command -s docker) $argv
    else
      command docker $argv
    end
  else
    echo fish: Unknown command \'docker\'
  end
end
