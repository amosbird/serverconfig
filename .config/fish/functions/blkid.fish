function blkid
  if command -s blkid > /dev/null
    if isatty 1
      grc (command -s blkid) $argv
    else
      command blkid $argv
    end
  else
    echo fish: Unknown command \'blkid\'
  end
end
