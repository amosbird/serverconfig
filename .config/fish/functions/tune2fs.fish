function tune2fs
  if command -s tune2fs > /dev/null
    if isatty 1
      grc (command -s tune2fs) $argv
    else
      command tune2fs $argv
    end
  else
    echo fish: Unknown command \'tune2fs\'
  end
end
