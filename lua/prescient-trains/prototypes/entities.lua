local C = require("common.constants")

local function copy(from, name)
    local copied = table.deepcopy(from)
    copied.name = name
    copied.minable.result = name

    return copied
end

-- Train stop
-- {{{
local dispatcher_train_stop = copy(data.raw["train-stop"]["train-stop"],
                                   C.item_name)

dispatcher_train_stop.color = {r = 0.46, g = 0.01, b = 0.98, a = 1}
dispatcher_train_stop.next_upgrade = nil
dispatcher_train_stop.selection_box = {{-0.9, -0.6}, {0.9, 0.9}}
-- }}}

-- Input
-- {{{
local dispatcher_train_stop_in = copy(data.raw["lamp"]["small-lamp"],
                                      C.input_item_name)

dispatcher_train_stop_in.minable = nil
dispatcher_train_stop_in.next_upgrade = nil

dispatcher_train_stop_in.selection_box = {{-0.5, -0.5}, {0.5, 0.5}}
dispatcher_train_stop_in.selection_priority =
    (dispatcher_train_stop_in.selection_priority or 50) + 10

dispatcher_train_stop_in.collision_box = {{-0.15, -0.15}, {0.15, 0.15}}
dispatcher_train_stop_in.collision_mask = {"rail-layer"} -- collide only with rail entities

dispatcher_train_stop_in.light = {intensity = 1, size = 6}
dispatcher_train_stop_in.energy_source = {type = "void"}
dispatcher_train_stop_in.energy_usage_per_tick = "10W"
dispatcher_train_stop_in.selectable_in_game = true

dispatcher_train_stop_in.flags = {"placeable-off-grid", "player-creation"}
-- }}}

data:extend({dispatcher_train_stop, dispatcher_train_stop_in})
