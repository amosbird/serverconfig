function unlink --argument file
	if test (count $argv) -ne 1
    echo "Usage: $_ <symbol link filename>"
    return 1
  end
  if test -L $file -a -f $file
	sed -i ';' $file
  else
    echo "File $file is not a valid symbol link file."
    return 1
  end
end
