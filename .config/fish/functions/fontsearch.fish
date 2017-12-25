# Defined in /tmp/fish.Oa2udU/fontsearch.fish @ line 1
function fontsearch
	env FC_DEBUG=4 pango-view -q --font='sans' -t "$argv[1]" 2>&1 | grep -o 'family: "[^"]\+' | cut -c 10- | tail -n 1
end
