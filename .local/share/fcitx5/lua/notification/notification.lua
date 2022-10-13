local fcitx = require("fcitx")

fcitx.watchEvent(fcitx.EventType.InputMethodActivated, "activated")
fcitx.watchEvent(fcitx.EventType.InputMethodDeactivated, "deactivated")

function activated(event_name)
  if event_name == "rime" then
    io.popen("echo 'a' | nc -w 0 -U /tmp/caps-indicator.socket")
  else
    io.popen("echo 'd' | nc -w 0 -U /tmp/caps-indicator.socket")
  end
  return false
end

function deactivated(event_name)
  io.popen("echo 'd' | nc -w 0 -U /tmp/caps-indicator.socket")
  return false
end
