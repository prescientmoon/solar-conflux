local C = require("common.constants")

data:extend({
  {
    type = "technology",
    name = "tinker-with-schedules",
    icon_size = 256,
    icon = "__base__/graphics/technology/automated-rail-transportation.png",
    effects = { { type = "unlock-recipe", recipe = C.item_name } },
    prerequisites = { "automated-rail-transportation", "circuit-network" },
    unit = {
      count = 100,
      ingredients = {
        { "automation-science-pack", 1 }, { "logistic-science-pack", 1 }
      },
      time = 15
    },
    order = "c-g-aa"
  }
})
