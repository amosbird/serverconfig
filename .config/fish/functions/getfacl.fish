function getfacl
  if command -s getfacl > /dev/null
    if isatty 1
      grc (command -s getfacl) $argv
    else
      command getfacl $argv
    end
  else
    echo fish: Unknown command \'getfacl\'
  end
end
