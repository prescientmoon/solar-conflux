data:extend({
    {
        type = "bool-setting",
        name = "tws-interface-debug-logfile",
        order = "ah",
        setting_type = "runtime-global",
        default_value = false
    }, {
        type = "string-setting",
        name = "tws-interface-console-level",
        order = "ad",
        setting_type = "runtime-global",
        default_value = "2",
        allowed_values = {"0", "1", "2", "3"}
    }
})
