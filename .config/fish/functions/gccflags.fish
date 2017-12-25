function gccflags --argument-names 'file'
  if test (count $argv) -ne 1
    echo "Usage: $_ <filename>"
    return 1
  end
  if test -f $file
    readelf -p .GCC.command.line $file
  else
    echo "File $file is not an existing file or directory."
    return 1
  end
end
