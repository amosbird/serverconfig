function __fish_netctl_get_profiles
  command netctl list | sed -e 's/^[ \t*]*//'
end

complete -f -c ncswitch -a '(__fish_netctl_get_profiles)'
