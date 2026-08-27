log = Log.open_topic ("s-system-microphone-mute")

local state = StateMetadata ("audio-mute-intent")
local mixer = nil

local function reconcile (node)
  local intent = state:get ("microphone")
  if intent ~= "0" and intent ~= "1" then
    log:notice (node, "system microphone mute intent is missing or invalid")
    return
  end

  local id = node["bound-id"]
  if not id or not mixer:call ("set-volume", id, { mute = intent == "1" }) then
    log:warning (node, "failed to apply system microphone mute intent")
  end
end

state:activate (Features.ALL, function (_, error)
  if error then
    log:warning ("failed to activate mute-intent state: " .. tostring (error))
    return
  end

  mixer = Plugin.find ("mixer-api")
  if not mixer then
    log:warning ("mixer API is unavailable; microphone intent cannot be restored")
    return
  end

  SimpleEventHook {
    name = "audio/reconcile-system-microphone-mute",
    after = "node/create-item",
    interests = {
      EventInterest {
        Constraint { "event.type", "=", "node-added" },
        Constraint { "media.class", "=", "Audio/Source", type = "pw-global" },
        Constraint { "device.id", "+", type = "pw-global" },
      },
    },
    execute = function (event)
      reconcile (event:get_subject ())
    end,
  }:register ()
end)
