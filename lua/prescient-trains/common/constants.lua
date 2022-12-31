local M = {}

M.item_name = "dispatcher-train-stop"
M.input_item_name = M.item_name .. "-input"

-- Distance from rails for each stop type.
-- Useful if, in the future, I decide to
-- add support for cargo ships
M.stop_offsets = { [M.item_name] = 0 }

return M
