local C = require("common.constants")

data:extend({
    {
        type = "recipe",
        name = C.item_name,
        energy_required = 5,
        enabled = false,
        ingredients = {
            {"train-stop", 1}, {"electronic-circuit", 20}, {"copper-cable", 20}
        },
        result = C.item_name
    }
})
