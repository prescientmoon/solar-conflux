local Entity = require("__stdlib__/stdlib/entity/entity")
local Direction = require("__stdlib__/stdlib/area/direction")
local Position = require("__stdlib__/stdlib/area/position")
local Area = require("__stdlib__/stdlib/area/area")

local H = require("scripts.helpers")
local C = require("common.constants")

-- local settings = require("scripts.settings")

local M = {}

---@class DispatcherTrainStopData
---@field entity LuaEntity
---@field input LuaEntity

---Gets the data attached to a dispatcher
---@param entity LuaEntity
---@return DispatcherTrainStopData|nil
---@nodiscard
local function get_dispatcher_data(entity)
  if not global.tws_dispatchers then
    return nil
  end

  return global.tws_dispatchers[entity.unit_number]
end

---Attaches some data to a dispatcher
---@param entity LuaEntity
---@param value DispatcherTrainStopData|nil
local function set_dispatcher_data(entity, value)
  if not global.tws_dispatchers then
    global.tws_dispatchers = {}
  end

  global.tws_dispatchers[entity.unit_number] = value
end

-- {{{ CreateStop
---Runs when a stop gets created
---@param entity LuaEntity
local function CreateStop(entity)
  if get_dispatcher_data(entity) then
    -- TODO: better logs
    game.print("Duplicate unit number, wtf")
    return
  end

  local stop_offset = C.stop_offsets[entity.name]
  local offset_adjustment = (not stop_offset) and stop_offset
    or Direction.to_vector(Direction.next(entity.direction), stop_offset)
  -- local offset_adjustment = 0
  ---@type MapPosition
  local input_position = Position(entity.position)
    + offset_adjustment
    -- Moves the center we rotate around to the
    -- center of the top-left block,
    -- Position(0.5, 0.5) +
    -- then rotates counterclockwise once,
    -- and go 0.5 more in that direction
    -- Direction.to_vector(
    --     Direction.next(entity.direction, true), 0.5)
    + Direction.to_vector(entity.direction, 0.5)

  local search_area = Area.shrink(Area.new({ input_position }), 0.001)

  local input

  -- {{{ Handle blueprint ghosts and existing IO entities preserving circuit connections
  local ghosts = entity.surface.find_entities(search_area)
  for _, ghost in pairs(ghosts) do
    if ghost.valid then
      if ghost.name == "entity-ghost" then
        if ghost.ghost_name == C.input_item_name then
          _, input = ghost.revive()
        end

        -- something has built I/O already (e.g.) Creative Mode Instant Blueprint
      elseif ghost.name == C.input_item_name then
        input = ghost
      end
    end
  end
  -- }}}

  if input == nil then -- create new
    input = entity.surface.create_entity({
      name = C.input_item_name,
      position = input_position,
      force = entity.force,
    })
  end

  if input == nil then
    -- TODO: logging
    game.print("Something went wrong")
    return
  end

  input.operable = false -- disable gui
  input.minable = false
  input.destructible = false -- don't bother checking if alive

  ---@type DispatcherTrainStopData
  local stop_data = { entity = entity, input = input }

  set_dispatcher_data(entity, stop_data)
end

-- }}}
-- {{{ OnEntityCreated
local function OnEntityCreated(event)
  local entity = event.created_entity or event.entity or event.destination

  if not entity or not entity.valid then
    return
  end

  if entity.name == C.item_name then
    CreateStop(entity)
  end
end

-- }}}
-- {{{ RemoveStop
---Runs once a train stop has been removed.
---@param entity LuaEntity
---@param create_ghosts boolean
function RemoveStop(entity, create_ghosts)
  local stop = get_dispatcher_data(entity)

  -- {{{ Destroy io entities
  if stop then
    ---@type LuaEntity
    local input = stop.input

    if input and input.valid then
      if create_ghosts then
        input.destructable = true
        input.die()
      else
        input.destroy()
      end
    end
  end
  -- }}}

  -- Delete entity data
  set_dispatcher_data(entity, nil)
end

-- }}}
-- {{{ OnEntityRemoved
---Runs when any kind of entity has been removed.
---@param event EventData.on_entity_died|EventData.script_raised_destroy|EventData.on_robot_pre_mined|EventData.on_pre_player_mined_item
---@param create_ghosts any
function OnEntityRemoved(event, create_ghosts)
  local entity = event.entity

  if not entity or not entity.valid then
    return
  end

  if entity.name == C.item_name then
    RemoveStop(entity, create_ghosts)
  end
end

-- }}}
-- {{{ Remove entity data when surfaces get removed.
---@param event EventData.on_pre_surface_deleted|EventData.on_pre_surface_cleared
function OnSurfaceRemoved(event)
  -- stop references
  local surfaceID = event.surface_index
  local surface = game.surfaces[surfaceID]

  if surface then
    local train_stops = surface.find_entities_filtered({ type = "train-stop" })

    for _, entity in pairs(train_stops) do
      if entity.name == C.item_name then
        RemoveStop(entity, false)
      end
    end
  end
end

-- }}}
-- {{{ UpdateSchedule
---Runs every tick to update train schedules
---@param stop DispatcherTrainStopData
function UpdateSchedule(stop)
  local control = stop.input.get_or_create_control_behavior()

  if not control or not control.valid then
    return
  end

  local signals = {
    action_create = { type = "virtual", name = "signal-C" },
    action_jump = { type = "virtual", name = "signal-J" },
    target_after = { type = "virtual", name = "signal-A" },
    target_before = { type = "virtual", name = "signal-B" },
    template_past = { type = "virtual", name = "signal-P" },
    template_future = { type = "virtual", name = "signal-F" },
    option_infinite = { type = "virtual", name = "signal-O" },
    option_extend = { type = "virtual", name = "signal-E" },
  }

  local create = H.get_signal_count(control, signals.action_create)
  local jump = H.get_signal_count(control, signals.action_jump)

  local train = stop.entity.get_stopped_train()

  if not train or not train.valid then
    return
  end

  local schedule = train.schedule

  if not schedule then
    return
  end

  schedule = H.copy(schedule)

  if create > 0 then
    local target_after = H.get_signal_count(control, signals.target_after)
    local target_before = H.get_signal_count(control, signals.target_before)
    local target_index = schedule.current + target_after - target_before

    local template_future = H.get_signal_count(control, signals.template_future)
    local template_past = H.get_signal_count(control, signals.template_past)
    local template_index = schedule.current + template_future - template_past

    local template = H.get_cycled(schedule.records, template_index)
    local copy = H.copy(template)

    local is_temporary = 0 == H.get_signal_count(control, signals.option_infinite)
    copy.temporary = is_temporary

    local extend = H.get_signal_count(control, signals.option_extend)

    if extend > 0 and copy.station then
      copy.station = copy.station .. " " .. tostring(extend)
    end

    H.list_insert(schedule.records, target_index, copy)

    train.schedule = schedule
  end

  if jump > 0 and #schedule.records > 0 then
    schedule.current = (schedule.current + jump - 1) % #schedule.records + 1

    train.schedule = schedule
  end
end

-- }}}
-- {{{ OnTick
function OnTick()
  for _, v in pairs(global.tws_dispatchers or {}) do
    UpdateSchedule(v)
  end
end

-- }}}

function M.setup()
  local filters_on_built = { { filter = "type", type = "train-stop" } }
  local filters_on_mined = { { filter = "type", type = "train-stop" } }

  -- {{{ On create events
  local on_create_events = {
    defines.events.on_built_entity,
    defines.events.on_robot_built_entity,
    defines.events.script_raised_built,
    defines.events.script_raised_revive,
    defines.events.on_entity_cloned,
  }

  for _, event in pairs(on_create_events) do
    script.on_event(event, OnEntityCreated, filters_on_built)
  end
  -- }}}
  -- {{{ On remove events
  local on_remove_events = {
    defines.events.on_pre_player_mined_item,
    defines.events.on_robot_pre_mined,
    defines.events.script_raised_destroy,
  }

  for _, event in pairs(on_remove_events) do
    script.on_event(event, OnEntityRemoved, filters_on_mined)
  end

  script.on_event(defines.events.on_entity_died, function(event)
    OnEntityRemoved(event, true)
  end, filters_on_mined)
  -- }}}
  -- {{{ On surface removed
  script.on_event({
    defines.events.on_pre_surface_deleted,
    defines.events.on_pre_surface_cleared,
  }, OnSurfaceRemoved)
  -- }}}

  script.on_event(defines.events.on_tick, OnTick)
end

return M
