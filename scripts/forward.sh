#!/usr/bin/env bash

if [ ! -v INSIDE_UNSHARE ]
then
   exec /tmp/gentoo/user_mount /tmp bash "$0" "$@"
fi

local_port=$1
shift
remote_port=$1
shift

script=$(forward.pl)
cmd="(echo $script $remote_port; cat) | $*"

temp_script_file=/tmp/script

echo "#!/usr/bin/env bash" > "$temp_script_file"
echo "set -x" >> "$temp_script_file"
echo "$cmd" >> "$temp_script_file"
chmod +x "$temp_script_file"

socat -d -d -d TCP4-LISTEN:$local_port,reuseaddr,fork EXEC:"$temp_script_file"
