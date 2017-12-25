function _pwd_with_tilde
  echo $PWD | sed 's|^'$HOME'\(.*\)$|~\1|'
end

function _print_in_color
  set -l string $argv[1]
  set -l color  $argv[2]

  set_color $color
  printf $string
  set_color normal
end

function _prompt_color_for_status
  if test $argv[1] -eq 0
    echo green
  else
    echo red
  end
end

function fish_prompt
  set -l last_status $status

  _print_in_color "\n" blue
  if test -z "$TMUX"
	  _print_in_color (_pwd_with_tilde) blue
  end

  if test -z "$DIRENV_DIFF"
    _print_in_color " ❯ " (_prompt_color_for_status $last_status)
  else
    if true
      set -q envprompt; or set envprompt ENV
      _print_in_color " $envprompt" cyan
    else
	    _print_in_color " ENV" yellow
    end
    _print_in_color " ❯ " (_prompt_color_for_status $last_status)
  end
end
