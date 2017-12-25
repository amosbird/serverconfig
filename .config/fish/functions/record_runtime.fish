function record_runtime --on-event fish_postexec
  set -l duration (echo $CMD_DURATION | humanize_duration)
  echo "$argv       ($duration)" >> $HOME/.commandlog
end
