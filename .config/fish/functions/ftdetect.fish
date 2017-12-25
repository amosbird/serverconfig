function ftdetect --argument-names char
    env FC_DEBUG=4 pango-view -qt $char 2>&1 | awk -F \" '/family: / { m = $2 } END { print m }'
end
