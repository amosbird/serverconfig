function g++
  if command -s g++ > /dev/null
    if isatty 1
      grc (command -s g++) $argv
    else
      command g++ $argv
    end
  else
    echo fish: Unknown command \'g++\'
  end
end
