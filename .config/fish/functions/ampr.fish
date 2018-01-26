function ampr --argument-names 'url'
    set url (echo $url | sed 's=https://github.com\(.*\)=https://patch-diff.githubusercontent.com\1.patch=')
    curl -L $url | git am -3
end
