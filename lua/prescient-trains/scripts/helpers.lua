local M = {}

-- See [this original implementation](https://github.com/coltonj96/UsefulCombinators/blob/master/UsefulCombinators_0.4.4/control.lua#L2062)
---Get the total count of some signal in some control behavior.
---@param control LuaControlBehavior
---@param signal SignalID
function M.get_signal_count(control, signal)
  if not signal then
    return 0
  end

  local red = control.get_circuit_network(defines.wire_type.red)
  local green = control.get_circuit_network(defines.wire_type.green)

  local total = 0

  if red then
    total = total + (red.get_signal(signal) or 0)
  end

  if green then
    total = total + (green.get_signal(signal) or 0)
  end

  return total
end

---Returns the index if going past #array or under 1 would loop back the other side.
---Assumes the array is nonempty
---@generic T
---@param array T[]
---@param index integer
---@returns integer
---@nodiscard
function M.mod_index(array, index)
  return (index - 1) % #array + 1
end

---Uses the above function to normalize the index between the start and end of the array.
---Assumes the array is nonempty
---@generic T
---@param array T[]
---@param index integer
---@return T
---@nodiscard
function M.get_cycled(array, index)
  return array[M.mod_index(array, index)]
end

---Annotated version of table.inset
---@generic T
---@param list T[]
---@param at integer
---@param value T
function M.list_insert(list, at, value)
  table.insert(list,at, value)
end

---Shallow copies some table
---@generic T :table
---@param object T
---@return T
function M.copy(object)
  local result = {}

  for k, v in pairs(object) do
    result[k] = v end

  return result
end

return M
