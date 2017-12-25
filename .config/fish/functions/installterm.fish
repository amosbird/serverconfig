function installterm -d "install TERM to remote" --argument-names 'host'
  infocmp -x $TERM | ssh $host 'tmp=$(mktemp /tmp/termcap.XXXXXX); trap "rm -f $tmp" EXIT; cat - > $tmp ; tic -x $tmp;'
end
