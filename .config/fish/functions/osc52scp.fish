function osc52scp -d "scp file via osc52" --argument-names 'file'
  if test (count $argv) -ne 1
    echo "Usage: $_ <file>"
    return 1
  end
  set max 74994
  if test -d $file -o -f $file
    if test (stat -c%s $file) -ge 10000000
      echo "File $file is too large."
      return 1
    end
    set path (readlink -f $file)
    set buf "scp "(whoami)"@"(hostname)":"$path" "/tmp/(basename $path)
    set esc "\033]52;y;"(printf $buf | head -c $max | base64 | tr -d '\r\n')"\07"
    set esc "\033Ptmux;\033"$esc"\033\\" # tmux
    printf "$esc"
    osc52browser "file:///tmp/"(basename $path)
  else
    echo "File $file is not an existing file or directory."
    return 1
  end
end
