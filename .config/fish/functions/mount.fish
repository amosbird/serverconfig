function mount
  if command -s mount > /dev/null
    if isatty 1
      grc (command -s mount) $argv
    else
      command mount $argv
    end
  else
    echo fish: Unknown command \'mount\'
  end
end
