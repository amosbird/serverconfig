log = Log.open_topic ("s-freeclip-mute-intent")

local state = StateMetadata ("audio-mute-intent")
local mixer = nil

local function reconcile (node)
  local intent = state:get ("microphone")
  if intent ~= "0" and intent ~= "1" then
    log:notice (node, "mute intent is missing or invalid; leaving FreeClip unchanged")
    return
  end

  local id = node["bound-id"]
  if not id or not mixer:call ("set-volume", id, { mute = intent == "1" }) then
    log:warning (node, "failed to apply persisted microphone mute intent")
  end
end

state:activate (Features.ALL, function (_, error)
  if error then
    log:warning ("failed to activate mute-intent state: " .. tostring (error))
    return
  end

  mixer = Plugin.find ("mixer-api")
  if not mixer then
    log:warning ("mixer API is unavailable; leaving FreeClip unchanged")
    return
  end

  SimpleEventHook {
    name = "audio/reconcile-freeclip-mute-intent",
    after = "node/create-item",
    interests = {
      EventInterest {
        Constraint { "event.type", "=", "node-added" },
        Constraint {
          "node.name", "=", "bluez_input.C0:DA:5E:EC:FB:7F", type = "pw-global"
        },
        Constraint { "media.class", "=", "Audio/Source", type = "pw-global" },
      },
    },
    execute = function (event)
      reconcile (event:get_subject ())
    end,
  }:register ()
end)
