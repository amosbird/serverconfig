-- freeclip-routing.lua — targeted stream routing enforcement
--
-- Design: applications face two invariant virtual endpoints
-- (freeclip_stable_output / freeclip_stable_input) that always exist and are
-- the system default sink/source. Apps that follow the default are already
-- immune to headset churn, and apps that deliberately name a device -- or a
-- stream the user moved by hand -- must keep that choice, otherwise the
-- laptop speakers, HDMI and USB outputs are unreachable.
--
-- That leaves the one case this script exists for: an app that *pins* a stale
-- target cached from a previous, broken session (Wemeet reopening on
-- "built-in speaker"). Such a stream carries target.object and PipeWire
-- refuses external moves (pactl move-sink-input -> EINVAL), so the only way
-- in is WirePlumber's own override channel (linking/find-defined-target.lua):
-- metadata key "target.node" with subject = stream node id takes precedence
-- over the stream's own target.object (unless the stream sets
-- node.dont-move).
--
-- Enforcement is therefore restricted to FORCED_APPS, whose device choice is
-- known to be unreliable and for which the endpoint is always the right
-- answer. For those streams the metadata is written deterministically:
--
--   - when a stream appears            -> assert target
--   - when an endpoint node appears    -> re-assert forced streams
--
-- It never inspects stream state (RUNNING/silence/...), never infers
-- "meeting active", never kills or moves streams beyond this one metadata
-- write. Writing an unchanged value emits no metadata event, so the
-- assertions are idempotent.

local STABLE = {
  ["Stream/Output/Audio"] = "freeclip_stable_output",
  ["Stream/Input/Audio"]  = "freeclip_stable_input",
}

-- Matched case-insensitively as a substring of application.name,
-- application.process.binary and node.name.
local FORCED_APPS = { "wemeet" }

local metadata = nil
local endpoint_ids = {}   -- endpoint node.name -> node.id

local metadata_om = ObjectManager {
  Interest { type = "metadata",
    Constraint { "metadata.name", "=", "default" } },
}

local nodes_om = ObjectManager {
  Interest { type = "node",
    Constraint { "media.class", "c", "Stream/Output/Audio", "Stream/Input/Audio",
                 "Audio/Sink", "Audio/Source" } },
}

local function is_endpoint (name)
  return name == "freeclip_stable_output" or name == "freeclip_stable_input"
end

local function is_forced (props)
  for _, key in ipairs { "application.name", "application.process.binary",
                         "node.name" } do
    local value = props [key]
    if value then
      value = value:lower ()
      for _, app in ipairs (FORCED_APPS) do
        if value:find (app, 1, true) then return true end
      end
    end
  end
  return false
end

local function assert_target (node)
  local props = node.properties
  local endpoint = STABLE [props ["media.class"]]
  if not endpoint then return end
  if not is_forced (props) then return end

  local target_id = endpoint_ids [endpoint]
  if not target_id or not metadata then return end

  local current = metadata:find (node ["bound-id"], "target.node")
  if current ~= tostring (target_id) then
    metadata:set (node ["bound-id"], "target.node", "Spa:Id", tostring (target_id))
  end
end

local function reassert_all ()
  for node in nodes_om:iterate () do
    assert_target (node)
  end
end

nodes_om:connect ("object-added", function (_, node)
  local props = node.properties
  local name = props ["node.name"] or ""
  if is_endpoint (name) then
    endpoint_ids [name] = node ["bound-id"]
    reassert_all ()
  else
    assert_target (node)
  end
end)

nodes_om:connect ("object-removed", function (_, node)
  local name = (node.properties ["node.name"]) or ""
  if is_endpoint (name) then
    endpoint_ids [name] = nil
  end
end)

metadata_om:connect ("object-added", function (_, obj)
  metadata = obj
  reassert_all ()
end)

metadata_om:activate ()
nodes_om:activate ()
