-- FreeClip communication session manager
-- Owns profile changes and routes the persistent stable endpoints.

local cutils = require ("common-utils")
local log = Log.open_topic ("freeclip-session")

local CARD = "bluez_card.C0_DA_5E_EC_FB_7F"
local FREECLIP_OUTPUT = "bluez_output.C0_DA_5E_EC_FB_7F.1"
local FREECLIP_INPUT = "bluez_input.C0:DA:5E:EC:FB:7F"
local LOCAL_OUTPUT =
    "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Speaker__sink"
local LOCAL_INPUT =
    "alsa_input.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Mic1__source"
local OUTPUT_BACKEND = "freeclip_stable_output.backend"
local INPUT_BACKEND = "freeclip_stable_input.backend"

local STEP_MS = 100
local NODE_WAIT_STEPS = 20
local HFP_TRANSPORT_WAIT_STEPS = 20
local PARK_MS = 500
local RELEASE_MS = 1200
local RECOVERY_COOLDOWN_MS = 10000

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
local active_serials = {}
local recover

local nodes = ObjectManager {
  Interest { type = "node" }
}
local devices = ObjectManager {
  Interest {
    type = "device",
    Constraint { "device.name", "=", CARD }
  }
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

local function route_node (backend_name, target)
  local backend = find_node (backend_name)
  if not backend or not target then
    return false
  end
  metadata:set (backend["bound-id"], "target.object", "Spa:Id",
      target.properties["object.serial"])
  return true
end

local function route (backend_name, target_name)
  return route_node (backend_name, find_node (target_name))
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
  local input_ok = route (INPUT_BACKEND, LOCAL_INPUT)
  local output_ok = route (OUTPUT_BACKEND, LOCAL_OUTPUT)
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
  set_state ("LOCAL_FALLBACK")
  publish ("error", reason)
end

local function wait_for_nodes (mode, remaining, callback)
  if generation ~= callback.generation then return end
  local profile = mode == "hfp" and "headset-head-unit" or "a2dp-sink"
  local output = find_profile_node (FREECLIP_OUTPUT, profile)
  local input = mode == "hfp" and find_node (FREECLIP_INPUT) or find_node (LOCAL_INPUT)
  if output and input then
    callback.run (output, input)
  elseif remaining > 0 then
    later (STEP_MS, function () wait_for_nodes (mode, remaining - 1, callback) end)
  else
    fallback ("timed out waiting for " .. mode .. " nodes")
  end
end

local function route_input (mode, output, input)
  local input_target = mode == "hfp" and input or find_node (LOCAL_INPUT)
  if not route_node (INPUT_BACKEND, input_target) then
    fallback ("could not route stable input")
    return
  end
  normalize_volumes (output, mode == "hfp" and input or nil)
  Core.sync (function ()
    later (STEP_MS, function ()
      if output["state"] == "error" or
          (mode == "hfp" and input["state"] == "error") then
        recover (generation)
      else
        set_state (string.upper (mode) .. "_READY")
      end
    end)
  end)
end

local function wait_hfp_output_running (output, input, remaining)
  local backend = find_node (OUTPUT_BACKEND)
  if output["state"] == "error" then
    recover (generation)
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
    recover (generation)
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

local function transact (mode, recovering)
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
  if new_state == "error" and desired_mode == "hfp" then
    local serial = tostring (node.properties["object.serial"] or "")
    local failed_generation = active_serials[serial]
    if failed_generation == generation then recover (failed_generation) end
  end
end

nodes:connect ("object-added", function (_, node)
  node:connect ("state-changed", on_node_state_changed)
end)

devices:connect ("object-added", function ()
  if state == "DISCONNECTED" or state == "LOCAL_FALLBACK" then
    transact (desired_mode, false)
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
      publish ("error", "")
    end
  elseif key == "freeclip.session.request-id" and value then
    local command_value = metadata:find (0, "freeclip.session.command")
    local command = command_value and Json.Raw (command_value):parse ()
    if command == "apply" then
      recovery_generation = nil
      recovery_cooldown = false
      transact (desired_mode, false)
    elseif command == "retry" then
      recovery_generation = nil
      recovery_cooldown = false
      transact (desired_mode, false)
    end
  end
end)

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
    if find_device () then transact (desired_mode, false) else park () end
  end)
end)
