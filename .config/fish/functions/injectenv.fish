function injectenv -d "inject a new environment into a running process"
  set -l proc %self
  set -q argv[1]; and set proc $argv[1]
  sudo gdb --nx -batch -ex "attach $proc" -ex "call putenv(\"$argv[2]\")" -ex 'detach' > /dev/null ^&1
end
