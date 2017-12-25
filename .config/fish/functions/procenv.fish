function procenv -d "return process env, default self"
  set -l proc %self
  set -q argv[1]; and set proc $argv[1]
  awk 'BEGIN {RS="\0"; ORS="\n"} $0' /proc/"$proc"/environ;
end
