local M = {}

local keymaps = {}
local nextKeymapId = 0
local processedKeys = {}
local pressedKeys = {}
local currentTimer = nil
local timeout = 40

local function clearTimer()
  print("Clearing a timer")
  if currentTimer ~= nil then
    currentTimer.cancel()
    currentTimer = nil
  end
end

local function normalExec(command)
  print("Executing:", command)
  print("Replaced:", vim.api.nvim_replace_termcodes(command, true, false, true), "mt", true)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(command, true, false, true), "mt", true)
end

local function contains(table, key)
  for _, v in ipairs(table) do
    if v == key then
      return true
    end
  end
  return false
end

-- TODO: optimize to use the sorted nature of the lists
local function containsAll(big, small)
  for _, v in ipairs(small) do
    if not contains(big, v) then
      return false
    end
  end
  return true
end

local function listsEq(l, r)
  if #l ~= #r then return false end

  for i = 1, #l do
    if l[i] ~= r[i] then
      return false
    end
  end

  return true
end

local function getMatchingChords(keys)
  local results = {
    exact = {},
    longer = {}
  }

  local mode = vim.fn.mode()

  for _, keymap in ipairs(keymaps) do
    if contains(keymap.modes, mode) then
      if listsEq(keys, keymap.keys) then
        table.insert(results.exact, keymap)
      else if containsAll(keymap.keys, keys) then
          table.insert(results.longer, keymap)
        end
      end
    end
  end

  if #results.longer == 0 and #results.exact == 0 then
    return nil
  end

  return { exact = results.exact[#results.exact], longer = results.longer }
end

local execResults = {
  halt = 0,
  wait = 1,
  done = 2
}

local function attemptExec(canWait)
  local matching = getMatchingChords(pressedKeys)

  if matching == nil then
    return execResults.halt
  end

  if matching.exact == nil then
    if canWait and #matching.longer ~= 0 then
      return execResults.wait
    end

    return execResults.halt
  end


  normalExec("<Plug>(slam-exec:" .. matching.exact.id .. ")")

  return execResults.done
end

local function handleExecResult(result)
  if result == execResults.wait then
    print("Setting up timer")

    if currentTimer ~= nil then
      print("WHY TF IS THERE A TIMER HERE")
    end

    local timerControls = { active = true }

    currentTimer = {
      cancel = function()
        timerControls.active = false
      end
    }

    vim.defer_fn(function()
      if not timerControls.active then
        print("Timer got cancelled!")
        return
      end

      print("Timer run!")

      clearTimer()
      handleExecResult(attemptExec(false))
    end, timeout)
  end

  if result == execResults.done then
    print("Execution finished!")
    pressedKeys = {}
  end

  if result == execResults.halt then
    print("Halting execution")
    local defaultActions = ""
    for _, key in pairs(pressedKeys) do
      defaultActions = defaultActions .. "" .. "<Plug>(slam-cancel:" .. key .. ")"
    end

    normalExec(defaultActions)

    pressedKeys = {}
  end

  -- TODO: fail
end

local function processKey(key)
  print("Processing: " .. key)
  clearTimer()

  if contains(pressedKeys, key) then
    table.insert(pressedKeys, key)
    handleExecResult(execResults.halt)
    return
  end


  table.insert(pressedKeys, key)

  local result = attemptExec(true)
  handleExecResult(result)
end

-- TODO: rework this to be more efficient by using counts instead of whatever this is
local function addTrackedKey(key, data)
  local existing = processedKeys[key]
  if existing == nil then
    processedKeys[key] = { data }
    local allModes = { "i", "n", "v", "o" }

    vim.keymap.set(allModes, "<Plug>(slam-cancel:" .. key .. ")", key)
    vim.keymap.set(allModes, key, function()
      local mode = vim.fn.mode()

      for _, entry in ipairs(processedKeys[key]) do
        if contains(entry.modes, mode) then
          processKey(key)
          return
        end
      end

      print("<Plug>(slam-cancel:" .. key .. ")")
      normalExec("<Plug>(slam-cancel:" .. key .. ")")
    end)
  else
    table.insert(processedKeys[key], data)
  end
end

local function splitKeys(keys)
  return vim.fn["split"](keys, [[\(<[^<>]\+>\|.\)\zs]])
end

function M.set(modes, lhs, rhs, opts)
  local id = nextKeymapId

  if type(modes) == "string" then
    modes = { modes }
  end

  nextKeymapId = nextKeymapId + 1

  local keyList = splitKeys(lhs)

  -- TODO: error out on no keys
  for _, key in ipairs(keyList) do
    addTrackedKey(key, {
      id = id,
      modes = modes
    })
  end

  table.insert(keymaps, {
    id = id, keys = keyList, modes = modes
  })

  vim.keymap.set(modes, "<Plug>(slam-exec:" .. id .. ")", rhs, opts)
end

return M
