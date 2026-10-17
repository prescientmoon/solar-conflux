--game scene

local playerConfig = {
    scaleX = 32,
    scaleY = 32,
    startX = 150,
    startY = 300,
    acceleration = 0.2,
    rotLerp = 0.1,
    isDebugPlayer = true,
    maxSpeedX = 5
}

local playerData = {
    x = playerConfig.startX,
    y = playerConfig.startY,
    dx = 0,
    dy = 0,
    r = 0,
    rTarg = 0,
}

local stars = {}

local function drawPlayer()
    if playerConfig.isDebugPlayer == true then
        love.graphics.setColor(1,1,1,1)
        love.graphics.rectangle("fill",playerData.x - playerConfig.scaleX/2,playerData.y - playerConfig.scaleY/2,playerConfig.scaleX,playerConfig.scaleY)
    end
end

local function updatePlayer()
    local ix, iy = input:get "move"
    playerData.dx = lerp(playerData.dx, ix * playerConfig.maxSpeedX, playerConfig.acceleration)
    playerData.rTarg = lerp(playerData.rTarg, ix, playerConfig.rotLerp)
    playerData.r = lerp(-10,10, playerData.rTarg)
    playerData.y = lerp(playerData.y, (iy * 50) + playerConfig.startY, 0.05)
    playerData.x = playerData.x + playerData.dx
    playerData.x = clamp(0+ playerConfig.scaleX/2,playerData.x,300- playerConfig.scaleX/2)
end

local function addStars(count)
    for i = 1, count, 1 do
        local star = {
            x = math.random(0,300),
            y = math.random(0,400),
            spd = math.random(2,5),
            s = math.random(2,6),
            c = math.random(5,7)
        }
        table.insert(stars,star)
    end
end

local function updateStars()
    for index, value in ipairs(stars) do
        value.y = value.y + value.spd
        if value.y > 500 then
            value.y = math.random(-50,-100)
        end
    end
end

local function drawStars()
    for index, value in ipairs(stars) do
        ink(pal[value.c],false)
        love.graphics.rectangle("fill",value.x - value.s/2,value.y - value.s/2,value.s,value.s)
    end
end

function drawGame()
    drawStars()
    drawPlayer()
end

function updateGame()
    updatePlayer()
    updateStars()
end

function loadGame()
    addStars(125)
    playerData = {
        x = playerConfig.startX,
        y = playerConfig.startY,
        dx = 0,
        dy = 0,
        r = 0,
        rTarg = 0,
    }
end