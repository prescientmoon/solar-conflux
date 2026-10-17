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
            newStateLoad()
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
