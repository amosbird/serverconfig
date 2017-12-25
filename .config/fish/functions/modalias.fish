function modalias -d "show modalias via input id" --argument-names id
  cat /sys(udevadm info -q path /dev/input/event$id)/../modalias
end
