set fish_command_timer_color blue
set fish_command_timer_time_format '%b %d %I:%M%p'

function fish_command_timer_print_time
    date --date="@$argv[1]" +"$fish_command_timer_time_format"
end

# The fish_postexec event is fired after executing a command line.
function fish_command_timer_postexec -e fish_postexec
  if test "$cmd_duration" -lt 500
      return
  end
  set -l command_end_time (date '+%s')

  set -l SEC 1000
  set -l MIN 60000
  set -l HOUR 3600000
  set -l DAY 86400000

  set -l num_days (math "$cmd_duration / $DAY")
  set -l num_hours (math "$cmd_duration % $DAY / $HOUR")
  set -l num_mins (math "$cmd_duration % $HOUR / $MIN")
  set -l num_secs (math "$cmd_duration % $MIN / $SEC")
  set -l num_millis (math "$cmd_duration % $SEC")
  set -l time_str ""
  if [ $num_days -gt 0 ]
    set time_str {$time_str}{$num_days}"d "
  end
  if [ $num_hours -gt 0 ]
    set time_str {$time_str}{$num_hours}"h "
  end
  if [ $num_mins -gt 0 ]
    set time_str {$time_str}{$num_mins}"m "
  end
  set -l num_millis_pretty (printf '%03d' $num_millis)
  set -l time_str {$time_str}{$num_secs}s{$num_millis_pretty}
  set -x CMD_DURATION_STR "$time_str"
  set -l now_str (fish_command_timer_print_time $command_end_time)
  set -l output_str "[ $time_str | $now_str ]"
  set -l output_str_colored (set_color $fish_command_timer_color)"$output_str"(set_color normal)
  echo
  echo -e "$output_str_colored"
end
