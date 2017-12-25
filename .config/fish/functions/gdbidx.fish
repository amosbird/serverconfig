function gdbidx --argument-names 'target'
    set -l path (readlink -m target)
    set -l target_dir (dirname $path)
    set -l IFS
    set shared_libraries (ldd -d "$target" | grep "$target_dir" | cut -d '>' -f2 | cut -d ' ' -f2)
    set -e IFS

    function index_one_file --argument-names 'file'
      set -l dir (dirname $file)
      # We don't care if gdb gives an error.
      gdb -nx --batch-silent -ex "file $file" -ex "save gdb-index $dir"
      if test -f "$file.gdb-index"
        objcopy --add-section .gdb_index="$file.gdb-index" --set-section-flags .gdb_index=readonly "$file" "$file"
        rm -f "$file.gdb-index"
      end
    end

    function maybe_index --argument-names 'lib'
      if readelf -e "$lib" | grep '.gdb_index' > /dev/null 2>&1
        echo "Skipping $lib (already has .gdb_index)"
      else
        echo "Adding .gdb_index to $lib"
        index_one_file "$lib"
      end
    end

    maybe_index "$target"
    for lib in $shared_libraries
      maybe_index "$lib"
    end
end
