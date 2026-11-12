--various helper functions

--generates quads from a sprite sheet
function generateQuads(image, w, h, cols, rows)
    local sheetW, sheetH = image:getWidth(), image:getHeight()
    local out = {}

    for row = 0, rows-1 do
        for col = 0, cols-1 do
            local x, y = col * w, row * h
            if x + w <= sheetW and y + h <= sheetH then
                table.insert(out,
                    love.graphics.newQuad(x, y, w, h, sheetW, sheetH)
                )
            end
        end
    end

    return out
end

--linear interpolation function
function lerp(a, b, t)
  return a + (b - a) * t
end


--state swapping w/ transition effect
local swapStateIndex = 1 -- frame of the animation
local desiredNewState = nil -- label for the state
local newStateLoad = nil -- function that loads the state
local isSwappingState = false -- are we in the animation?

function swapState(newState, newStateLoadFunction)
    if isSwappingState == false then
        desiredNewState = newState
        newStateLoad = newStateLoadFunction
        swapStateIndex = 1
        isSwappingState = true
    end
end

function updateSwapState()
    if desiredNewState ~= nil then
        if swapStateIndex < 20 then
            swapStateIndex = swapStateIndex + 1
        else
            desiredNewState = nil
            isSwappingState = false
        end
        if swapStateIndex == 10 then
            swapStateSfx:play()
            if newStateLoad then newStateLoad() end
            newStateLoad = nil
            state = desiredNewState
        end
    end
end

function drawSwapState()
    love.graphics.setColor(1, 1, 1, 1)
    love.graphics.draw(swapStateSprs, swapStateQuads[swapStateIndex], 0, 0, 0, 2, 2)
end

--clamp between 2 values
function clamp(low, n, high)
    return math.min(math.max(low, n), high)
end

--axis aligned bounding box collision
function aabb(x1, y1, w1, h1, x2, y2, w2, h2)
    return x1 < x2 + w2 and
           x2 < x1 + w1 and
           y1 < y2 + h2 and
           y2 < y1 + h1
end

--remap value from a, b to c, d
function remap(val, low, high, low2, high2)
    return low2 + (val - low) * (high2 - low2) / (high - low)
end

function shakeCam(intensity)
    shake = intensity
end

local freezeFrameCount = -1000
function drawFreezeFrames()
    if freezeFrameCount > 0 then
       love.graphics.setColor(1, 1, 1, 1)
       love.graphics.rectangle("fill", 0, 0, screenWidth, screenHeight)
    elseif freezeFrameCount > -20 then
       local t = 1 - math.pow(1 - remap(freezeFrameCount, 0, -20, 1, 0), 2)
       love.graphics.setColor(1, 1, 1, t)
       love.graphics.setBlendMode("add")
       love.graphics.rectangle("fill", 0, 0, screenWidth, screenHeight)
       love.graphics.setBlendMode("alpha")
    end
end

function updateFreezeFrames()
    freezeFrameCount = freezeFrameCount - 1
end

function freezeFrames(count)
    freezeFrameCount = math.max(freezeFrameCount, 0) + count
end

function saveTableToJSON(tbl, filename)
    local contents = json.encode(tbl, { indent = true })
    if not contents then
        return false, "Failed to encode table to JSON"
    end

    local success, err = love.filesystem.write(filename, contents)
    if not success then
        return false, "Error writing file: " .. tostring(err)
    end

    return true
end

function loadTableFromJSON(filename)
    if not love.filesystem.getInfo(filename) then
        return false, "File not found: " .. filename
    end

    local contents, err = love.filesystem.read(filename)
    if not contents then
        return false, "Could not read file: " .. tostring(err)
    end

    local tbl, pos, decodeErr = json.decode(contents, 1, nil)
    if decodeErr then
        return false, "Error parsing JSON: " .. tostring(decodeErr)
    end

    return true, tbl
end

function saveData()
    local dataToSave = {scale, volume}
    saveTableToJSON(dataToSave,"settings.json")
end

function loadData()
    local didWork, returnData = loadTableFromJSON("settings.json")
    if didWork == true then
        scale = returnData[1]
        volume = returnData[2]
    end

end
