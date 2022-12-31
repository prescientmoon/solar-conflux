local C = require("common.constants")

local item_dispatcher_train_stop =
    table.deepcopy(data.raw["item"]["train-stop"])

item_dispatcher_train_stop.name = C.item_name
item_dispatcher_train_stop.order = item_dispatcher_train_stop.order .. "-c"
item_dispatcher_train_stop.icon_size = 64
item_dispatcher_train_stop.place_result = C.item_name
item_dispatcher_train_stop.icons = {
    {
        icon = "__base__/graphics/icons/train-stop.png",
        tint = {r = 0.46, g = 0.01, b = 0.98, a = 0.1}
    }
}

local item_dispatcher_train_stop_in = table.deepcopy(
                                          data.raw["item"]["small-lamp"])

item_dispatcher_train_stop_in.name = C.input_item_name
item_dispatcher_train_stop_in.place_result = C.input_item_name
item_dispatcher_train_stop_in.flags = {"hidden"}

data:extend({item_dispatcher_train_stop, item_dispatcher_train_stop_in})
