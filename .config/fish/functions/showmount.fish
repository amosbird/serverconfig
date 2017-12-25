function showmount
  if command -s showmount > /dev/null
    if isatty 1
      grc (command -s showmount) $argv
    else
      command showmount $argv
    end
  else
    echo fish: Unknown command \'showmount\'
  end
end
