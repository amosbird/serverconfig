function add-random-int-column --argument-names 'file' 'new-file'
# function add-random-int-column
    awk 'BEGIN { srand(0) } { print $0 = $0 "\t" int( rand( ) * 101 ) }' $file > $new-file
end
