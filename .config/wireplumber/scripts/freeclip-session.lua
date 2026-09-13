-- FreeClip communication session manager
-- Owns profile changes and routes the persistent stable endpoints.

local cutils = require ("common-utils")
local lutils = require ("linking-utils")
local log = Log.open_topic ("freeclip-session")

local CARD = "bluez_card.C0_DA_5E_EC_FB_7F"
local FREECLIP_OUTPUT = "bluez_output.C0_DA_5E_EC_FB_7F.1"
local FREECLIP_INPUT = "bluez_input.C0:DA:5E:EC:FB:7F"
local OUTPUT_BACKEND = "freeclip_stable_output.backend"
local INPUT_BACKEND = "freeclip_stable_input.backend"

local STEP_MS = 100
local UNMUTE_ATTEMPTS = 20
local NODE_WAIT_STEPS = 20
local HFP_TRANSPORT_WAIT_STEPS = 20
local PARK_MS = 500
local RELEASE_MS = 1200
local RECOVERY_COOLDOWN_MS = 10000
-- HCI-proven trigger: rapid profile switches race BlueZ's connect state
-- machine (it aborts with Create_Connection_Cancel on a live link, then
-- fails to adopt the eSCO it just created -> zombie handle, only an ACL
-- reconnect clears it). Serialize switches and let the headset settle
-- after every completed transition before honoring the next request.
local SWITCH_SETTLE_MS = 4000

-- Persists desired-mode across WirePlumber restarts (the default metadata
-- object is owned by WirePlumber and loses all values on restart).
local intent_meta = StateMetadata ("freeclip-session")

local metadata = cutils.get_default_metadata_object ()
local desired_mode = "a2dp"
local state = "DISCONNECTED"
local generation = 0
local timer = nil
local recovery_generation = nil
local recovery_cooldown = false
local transport_dead_generation = nil
local active_serials = {}
local transacting = false
local switch_settling = false
local pending_transact = nil
local recover
local transact
local transport_dead

local nodes = ObjectManager {
  Interest { type = "node" }
}
local devices = ObjectManager {
  Interest {
    type = "device",
    Constraint { "device.name", "=", CARD }
  }
}
-- Needed to answer "is this node's route actually available", see pick_local ().
local all_devices = ObjectManager {
  Interest { type = "device" }
}

local function publish (key, value)
  metadata:set (0, "freeclip.session." .. key, "Spa:String:JSON",
      string.format ("%q", tostring (value)))
end

local function set_state (value)
  state = value
  publish ("state", value)
  publish ("generation", generation)
  -- Any state transition resolves the previous failure. fallback () is the
  -- only error producer and publishes its reason right after set_state ().
  publish ("error", "")
  log:info ("state=" .. value .. " generation=" .. tostring (generation))
  if value:find ("_READY$") or value == "LOCAL_FALLBACK"
      or value == "DISCONNECTED" then
    if value:find ("_READY$") then
      transport_dead_generation = nil
    end
    -- Terminal state reached: keep the switch gate closed briefly so a
    -- follow-up toggle cannot race BlueZ while the headset settles.
    switch_settling = true
    transacting = false
    Core.timeout_add (SWITCH_SETTLE_MS, function ()
      switch_settling = false
      if pending_transact then
        local pending = pending_transact
        pending_transact = nil
        transact (pending.mode, pending.recovering)
      end
    end)
  end
end

local function cancel_timer ()
  if timer then
    timer:destroy ()
    timer = nil
  end
end

local function later (delay, callback)
  cancel_timer ()
  timer = Core.timeout_add (delay, function ()
    timer = nil
    callback ()
  end)
end

local function find_node (name)
  return nodes:lookup {
    Constraint { "node.name", "=", name, type = "pw" }
  }
end

local function find_profile_node (name, profile)
  return nodes:lookup {
    Constraint { "node.name", "=", name, type = "pw" },
    Constraint { "api.bluez5.profile", "=", profile, type = "pw" }
  }
end

local function find_device ()
  return devices:lookup {
    Constraint { "device.name", "=", CARD, type = "pw" }
  }
end

-- Local fallback targets are resolved at use time, never hardcoded: Speaker
-- and Headphones live in *mutually exclusive* UCM profiles, so the speaker
-- sink disappears the instant a jack is plugged in. A pinned node name would
-- leave the backend (node.dont-fallback = true) aimed at a dead target, i.e.
-- total silence. Rank the same way WirePlumber ranks default-node candidates:
-- highest priority.session among non-bluetooth device nodes whose route is
-- currently available (this is what keeps the unplugged mic jack and the dark
-- HDMI outputs from winning).
local function pick_local (media_class)
  local best, best_priority = nil, -1
  for node in nodes:iterate {
    Constraint { "media.class", "=", media_class, type = "pw" },
    Constraint { "device.api", "!", "bluez5", type = "pw" },
    Constraint { "device.id", "+", type = "pw" },
  } do
    local props = node.properties
    local priority = tonumber (props ["priority.session"]) or 0
    if priority > best_priority
        and lutils.haveAvailableRoutes (props, all_devices) then
      best, best_priority = node, priority
    end
  end
  return best
end

-- Returns true once the node's own route has been inspected (and unmuted if
-- needed), false while that is not yet possible -- the device params are not
-- cached during the first moments after startup.
local function unmute_route (node)
  local device_id = node.properties ["device.id"]
  local card_device = tonumber (node.properties ["card.profile.device"] or "")
  if not device_id or not card_device then return false end
  local device = all_devices:lookup {
    Constraint { "bound-id", "=", device_id, type = "gobject" }
  }
  if not device then return false end
  for p in device:iterate_params ("Route") do
    local route = cutils.parseParam (p, "Route")
    if route and route.device == card_device and route.props then
      local route_props = route.props.properties or {}
      if route_props.mute then
        log:info ("clearing stale mute on route " .. tostring (route.name))
        device:set_param ("Route", Pod.Object {
          "Spa:Pod:Object:Param:Route", "Route",
          index = route.index,
          device = route.device,
          props = Pod.Object {
            "Spa:Pod:Object:Param:Props", "Route",
            mute = false,
          },
          -- Persist it: the stale value otherwise comes back on every start.
          save = true,
        })
      end
      return true
    end
  end
  return false
end

-- Applications only ever see the stable endpoint, so it is the only thing the
-- desktop's volume/mute keys touch. That makes a mute left behind on the
-- physical device both invisible and unreachable: the graph stays fully
-- linked and every stream keeps "playing" while the card silently drops the
-- audio. Physical devices are an implementation detail of the endpoint, so
-- assert them unmuted whenever we aim a backend at one.
local function ensure_unmuted (node, attempt)
  if node.properties ["card.profile.device"] then
    if unmute_route (node) then return end
    attempt = (attempt or 0) + 1
    if attempt <= UNMUTE_ATTEMPTS then
      Core.timeout_add (STEP_MS, function () ensure_unmuted (node, attempt) end)
    end
    return
  end
  -- Bluetooth nodes carry no routes; mute lives on the node itself.
  node:set_param ("Props", Pod.Object {
    "Spa:Pod:Object:Param:Props", "Props",
    mute = false,
  })
end

local function route_node (backend_name, target)
  local backend = find_node (backend_name)
  if not backend or not target then
    return false
  end
  ensure_unmuted (target)
  -- Route by node NAME, not object.serial: the serial dangles as soon as
  -- the bluez node is recreated (every profile switch), leaving the backend
  -- waiting forever for a dead target (node.dont-fallback=True). The name
  -- survives node recreation and re-resolves on the next graph rescan.
  metadata:set (backend["bound-id"], "target.object", "Spa:String",
      target.properties["node.name"])
  return true
end

local function set_node_volume (node, volume)
  if not node then return end
  local props = Pod.Object {
    "Spa:Pod:Object:Param:Props", "Props",
    volume = volume,
  }
  node:set_param ("Props", props)
end

local function normalize_volumes (output, input)
  set_node_volume (find_node ("freeclip_stable_output"), 0.5)
  set_node_volume (output, 1.0)
  if input then set_node_volume (input, 1.0) end
end

local function park ()
  local input_ok = route_node (INPUT_BACKEND, pick_local ("Audio/Source"))
  local output_ok = route_node (OUTPUT_BACKEND, pick_local ("Audio/Sink"))
  return input_ok and output_ok
end

local function find_profile (device, wanted)
  for param in device:iterate_params ("EnumProfile") do
    local profile = cutils.parseParam (param, "EnumProfile")
    if profile and profile.name == wanted then
      return profile
    end
  end
  return nil
end

local function set_profile (name, callback)
  local device = find_device ()
  if not device then
    callback (false)
    return
  end
  local profile = find_profile (device, name)
  if not profile then
    callback (false)
    return
  end
  local param = Pod.Object {
    "Spa:Pod:Object:Param:Profile", "Profile",
    index = profile.index,
    save = false,
  }
  device:set_param ("Profile", param)
  Core.sync (function () callback (true) end)
end

local function remember_node (node)
  local serial = node and node.properties["object.serial"]
  if serial then active_serials[tostring (serial)] = generation end
end

local function fallback (reason)
  cancel_timer ()
  park ()
  -- A failed transition while the device is still present is an explicit
  -- transport failure (profile rejected, or nodes never appeared within the
  -- timeout): allow the single automatic HFP recovery before settling for
  -- local audio.
  if desired_mode == "hfp" and recovery_generation == nil
      and not recovery_cooldown and find_device () then
    log:info ("recovering after failure: " .. reason)
    recover (generation)
    return
  end
  if desired_mode == "hfp" and find_device () then
    -- Recovery was already consumed (or is cooling down) and the transport
    -- still failed: the HCI-proven zombie-eSCO case. Signal the desktop
    -- agent to force an ACL-level reconnect, the only working remedy.
    publish ("transport-dead", generation)
  end
  set_state ("LOCAL_FALLBACK")
  publish ("error", reason)
end

-- Error-driven path (node error on routed HFP nodes = the HCI-proven
-- zombie-eSCO signature). Profile cycling provably cannot clear it, so
-- skip the off->HFP cycle and go straight to the only working remedy:
-- the desktop agent sees transport-dead and reconnects at ACL level.
-- The timeout path (nodes never appeared) still gets the single profile
-- cycle via recover (). Once per incident; a second death lands in
-- LOCAL_FALLBACK instead of looping.
transport_dead = function ()
  if transport_dead_generation then
    fallback ("HFP transport still dead after reconnect")
    return
  end
  transport_dead_generation = generation
  cancel_timer ()
  park ()
  publish ("transport-dead", generation)
  set_state ("HFP_RECONNECTING")
end

local function wait_for_nodes (mode, remaining, callback)
  if generation ~= callback.generation then return end
  local profile = mode == "hfp" and "headset-head-unit" or "a2dp-sink"
  local output = find_profile_node (FREECLIP_OUTPUT, profile)
  local input = mode == "hfp" and find_node (FREECLIP_INPUT) or pick_local ("Audio/Source")
  if output and input then
    callback.run (output, input)
  elseif remaining > 0 then
    later (STEP_MS, function () wait_for_nodes (mode, remaining - 1, callback) end)
  else
    fallback ("timed out waiting for " .. mode .. " nodes")
  end
end

local function route_input (mode, output, input)
  local input_target = mode == "hfp" and input or pick_local ("Audio/Source")
  if not route_node (INPUT_BACKEND, input_target) then
    fallback ("could not route stable input")
    return
  end
  normalize_volumes (output, mode == "hfp" and input or nil)
  Core.sync (function ()
    later (STEP_MS, function ()
      if output["state"] == "error" or
          (mode == "hfp" and input["state"] == "error") then
        transport_dead ()
      else
        set_state (string.upper (mode) .. "_READY")
        if mode == "hfp" then
          -- Vacuous-success guard: a zombie eSCO can let the nodes appear
          -- and even run briefly before the transport fd fails. Re-verify
          -- the routed nodes shortly after declaring readiness.
          Core.timeout_add (3000, function ()
            if output["state"] == "error" or input["state"] == "error" then
              transport_dead ()
            end
          end)
        end
      end
    end)
  end)
end

local function wait_hfp_output_running (output, input, remaining)
  local backend = find_node (OUTPUT_BACKEND)
  if output["state"] == "error" then
    transport_dead ()
  elseif output["state"] == "running" or
      (backend and backend["state"] ~= "running") then
    -- If the stable output graph is inactive there is no concurrent acquire
    -- to serialize. Otherwise wait for the output side to own the SCO socket.
    route_input ("hfp", output, input)
  elseif remaining > 0 then
    later (STEP_MS, function ()
      wait_hfp_output_running (output, input, remaining - 1)
    end)
  else
    fallback ("timed out activating HFP output transport")
  end
end

local function route_ready (mode, output, input)
  remember_node (output)
  if mode == "hfp" then remember_node (input) end
  if output["state"] == "error" or (mode == "hfp" and input["state"] == "error") then
    transport_dead ()
    return
  end
  if not route_node (OUTPUT_BACKEND, output) then
    fallback ("could not route FreeClip output")
    return
  end
  Core.sync (function ()
    later (STEP_MS, function ()
      if mode == "hfp" then
        wait_hfp_output_running (output, input, HFP_TRANSPORT_WAIT_STEPS)
      else
        route_input (mode, output, input)
      end
    end)
  end)
end

local function start_profile (mode)
  local profile = mode == "hfp" and "headset-head-unit" or "a2dp-sink"
  set_profile (profile, function (ok)
    if not ok then
      fallback (profile .. " is unavailable")
      return
    end
    local transaction = {
      generation = generation,
      run = function (output, input) route_ready (mode, output, input) end,
    }
    wait_for_nodes (mode, NODE_WAIT_STEPS, transaction)
  end)
end

transact = function (mode, recovering)
  transacting = true
  generation = generation + 1
  active_serials = {}
  cancel_timer ()
  if not park () then
    fallback ("local fallback devices are unavailable")
    return
  end
  set_state (recovering and "HFP_RECOVERING" or string.upper (mode) .. "_STARTING")
  local delay = mode == "a2dp" and RELEASE_MS or PARK_MS
  if recovering then
    set_profile ("off", function (ok)
      if not ok then
        fallback ("could not release failed HFP profile")
        return
      end
      later (RELEASE_MS, function () start_profile ("hfp") end)
    end)
  else
    later (delay, function () start_profile (mode) end)
  end
end

-- Gate for external triggers (CLI toggles, device reconnection). A second
-- switch request while one is in flight or settling is queued, not raced.
local function request_transact (mode, recovering)
  if transacting or switch_settling then
    pending_transact = { mode = mode, recovering = recovering }
    log:info ("switch request queued: " .. mode)
    return
  end
  transact (mode, recovering)
end

recover = function (failed_generation)
  if desired_mode ~= "hfp" then return end
  if recovery_generation ~= nil or recovery_cooldown then
    fallback ("HFP transport failed after recovery")
    return
  end
  recovery_generation = failed_generation
  recovery_cooldown = true
  Core.timeout_add (RECOVERY_COOLDOWN_MS, function ()
    recovery_generation = nil
    recovery_cooldown = false
  end)
  transact ("hfp", true)
end

local function on_node_state_changed (node, _, new_state)
  if new_state ~= "error" or desired_mode ~= "hfp" then return end
  local serial = tostring (node.properties["object.serial"] or "")
  if active_serials[serial] == generation then
    transport_dead ()
    return
  end
  -- Serial bookkeeping can miss nodes recreated outside a tracked
  -- transaction; if a currently routed HFP node dies while we believe the
  -- session is ready, that is the transport-dead signature.
  if state == "HFP_READY" then
    local name = node.properties["node.name"] or ""
    if name == FREECLIP_OUTPUT or name == FREECLIP_INPUT
        or name == "bluez_input.C0_DA_5E_EC_FB_7F.0" then
      transport_dead ()
    end
  end
end

local repark_scheduled = false

-- A local fallback target is only valid while its node exists, and a UCM
-- profile switch (plugging the headphone jack) destroys and recreates the
-- analog nodes. Since the backend never falls back on its own, re-resolve
-- whenever the pool of local devices changes. Debounced: one jack event
-- churns several nodes at once.
local function on_local_device_churn (_, node)
  local props = node.properties
  if props ["device.api"] == "bluez5" or not props ["device.id"] then return end
  local class = props ["media.class"]
  if class ~= "Audio/Sink" and class ~= "Audio/Source" then return end
  if repark_scheduled then return end
  repark_scheduled = true
  Core.timeout_add (STEP_MS, function ()
    repark_scheduled = false
    if state == "DISCONNECTED" or state == "LOCAL_FALLBACK" then
      park ()
    elseif state == "A2DP_READY" then
      -- The headset owns the output in A2DP; only the mic side is local.
      route_node (INPUT_BACKEND, pick_local ("Audio/Source"))
    end
  end)
end

nodes:connect ("object-added", function (om, node)
  node:connect ("state-changed", on_node_state_changed)
  on_local_device_churn (om, node)
end)

nodes:connect ("object-removed", on_local_device_churn)

devices:connect ("object-added", function ()
  if state == "DISCONNECTED" or state == "LOCAL_FALLBACK" then
    request_transact (desired_mode, false)
  end
end)

devices:connect ("object-removed", function ()
  cancel_timer ()
  park ()
  set_state ("DISCONNECTED")
end)

metadata:connect ("changed", function (_, subject, key, _, value)
  if subject ~= 0 or not key then return end
  if key == "freeclip.session.desired-mode" and value then
    local mode = Json.Raw (value):parse ()
    if mode == "a2dp" or mode == "hfp" then
      desired_mode = mode
      intent_meta:set ("desired-mode", mode)
      recovery_generation = nil
      transport_dead_generation = nil
      publish ("error", "")
    end
  elseif key == "freeclip.session.request-id" and value then
    local command_value = metadata:find (0, "freeclip.session.command")
    local command = command_value and Json.Raw (command_value):parse ()
    if command == "apply" then
      recovery_generation = nil
      recovery_cooldown = false
      request_transact (desired_mode, false)
    elseif command == "retry" then
      recovery_generation = nil
      recovery_cooldown = false
      request_transact (desired_mode, false)
    end
  end
end)

all_devices:activate ()
nodes:activate ()
set_state ("DISCONNECTED")
intent_meta:activate (Features.ALL, function (_, err)
  if not err then
    local stored = intent_meta:get ("desired-mode")
    if stored == "a2dp" or stored == "hfp" then desired_mode = stored end
  end
  publish ("desired-mode", desired_mode)
  devices:activate ()
  later (STEP_MS, function ()
    set_node_volume (find_node ("freeclip_stable_output"), 0.5)
    if find_device () then request_transact (desired_mode, false) else park () end
  end)
end)
