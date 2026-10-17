--game scene

local playerConfig = {
    scaleX = 32,
    scaleY = 32,
    startX = 150,
    startY = 300,
    acceleration = 0.2,
    rotLerp = 0.1,
    isDebugPlayer = true
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
    
end

local

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