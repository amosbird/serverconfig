# Defined in /tmp/fish.WsvYc1/__fish_stop_bracketed_paste.fish @ line 2
function __fish_stop_bracketed_paste
	# Restore the last bind mode.
  set fish_bind_mode $__fish_last_bind_mode
  set -e __fish_paste_quoted
  set -l cmdline (string trim -- (commandline))
  test -z "$cmdline[-1]"; and set -e cmdline[-1]
  commandline -r -- $cmdline
  commandline -f force-repaint
end
