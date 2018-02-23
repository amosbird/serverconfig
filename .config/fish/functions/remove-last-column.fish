function remove-last-column --argument-names 'file'
  if test (count $argv) -ne 2
    echo "Usage: $_ <oldfile> <newfile>"
    return 1
  end
  awk 'NF{NF--};1' < $oldfile > $newfile
end
