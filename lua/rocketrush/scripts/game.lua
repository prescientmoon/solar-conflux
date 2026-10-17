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

local function drawPlayer()
    if playerConfig.isDebugPlayer == true then
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

function drawGame()
    drawPlayer()
end

function updateGame()
    updatePlayer()
end

function loadGame()
    playerData = {
        x = playerConfig.startX,
        y = playerConfig.startY,
        dx = 0,
        dy = 0,
        r = 0,
        rTarg = 0,
    }
end