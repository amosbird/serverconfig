function r
  if test (count $argv) -lt 1
    echo "Usage: $_ <filename|dir>*"
    return 1
  end
  mv --backup=numbered $argv ~/.trash
end
