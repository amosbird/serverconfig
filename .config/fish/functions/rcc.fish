function rcc -d "rsync a file over a cluster" --argument-names 'filename'
  if test (count $argv) -ne 1
    echo "Usage: $_ <file|dir>"
    return 1
  end
  if not test -e $filename
    echo "file not exist"
    return 1
  end
  if test -d $filename
    set filename $filename"/"
  end

  set -e hostfile
  pushd .
  while set -q $hostfile
      if test -e ".hostfile"
          set hostfile (pwd)/.hostfile
      else
          if test (pwd) = "/"
              echo "Cannot find a .hostfile up from here."
              popd
              return 1
          end
          cd ..
      end
  end
  popd

  set syncpath (realpath $filename)
  set dirpath (dirname $syncpath"dummy")
  # while read -la host
  #     rsync --rsync-path="mkdir -p $dirpath && rsync" --delete -a $filename $host:$syncpath
  # end < $hostfile
  # return 0
  parallel "rsync --rsync-path=\"mkdir -p $dirpath; rsync\" -a $filename '{}':$syncpath --delete" :::: "$hostfile"
end
