--game scene

local playerConfig = {
    scaleX = 20,
    scaleY = 30,
    startX = 150,
    startY = 300,
    acceleration = 0.2,
    rotLerp = 0.1,
    maxSpeedX = 5,
    startHealth = 3
}

local playerData = {
    x = playerConfig.startX,
    y = playerConfig.startY,
    dx = 0,
    dy = 0,
    r = 0,
    rTarg = 0,
    health = playerConfig.startHealth
}

local stars = {}

local maxTrash = 8
externHealth = 0

charge = 1
local tempCharge = 1
local maxCharge = 50
local isInAura = false
local hasAdddedCharge = false

local outlineVisible = false

trashTimer = 1
local trashInterval = 1800

local trashObjs = {}

local alive = true

local function die()
    alive = false
    playerData.x = 999
end

local auraTimer = 1
local starBoomAnimations = {}

local function addOrRemoveHealth(val)
    playerData.health = playerData.health + val
    externHealth = playerData.health
    if playerData.health < 1 then
        die()
    end

    if val < 0 then
        for i=0,-val-1 do
            table.insert(
                starBoomAnimations,
                { start = tick.frame, index = playerData.health + i }
            )
        end
    end
end


local function addTrash(x)
    local trash = {
        x = x,
        y = math.random(-300, -100),
        tex = math.random(1,3),
        dy = 0,
        spd = 5,
        accel = 0.1,
        r = 0
    }
    table.insert(trashObjs,trash)
end

local function updateTrash()
    if alive == true then
        for index, value in ipairs(trashObjs) do
            value.dy = lerp(value.dy,value.spd,value.accel)
            if value.y < 600 then
                value.y = value.y + value.dy
            else
                value.y = math.random(-300, -100)
                value.x = math.random(32,300-32)
                value.dy = 0
            end
            value.r = value.r + 5
            if aabb(value.x -16, value.y - 16, 32, 32, playerData.x-playerConfig.scaleX/2,playerData.y-playerConfig.scaleY/2,playerConfig.scaleX,playerConfig.scaleY) then
                addOrRemoveHealth(-1)
                shakeCam(2)
                value.y = math.random(-300, -100)
                value.x = math.random(32,300-32)
                value.dy = 0
            end
            if aabb(value.x -16, value.y - 16, 32, 32, playerData.x-(playerConfig.scaleX*4)/2,playerData.y-(playerConfig.scaleY*3)/2,playerConfig.scaleX*4,playerConfig.scaleY*3) then
                isInAura = true
                hasAdddedCharge = false
                if auraTimer < 5 then
                    auraTimer = auraTimer + 1
                else
                    if tempCharge < 10 and charge ~= 50 then
                        auraSfx:setPitch(remap(tempCharge,1,10,0.8,1.2))
                        love.audio.play(auraSfx)
                        tempCharge = tempCharge + 1
                        outlineVisible = true
                    end
                    auraTimer = 1
                end

            else
                outlineVisible = false
                isInAura = false
            end
        end
        if trashTimer < trashInterval then
            trashTimer = trashTimer + 1
        else
            trashTimer = 1
            if #trashObjs < maxTrash then
                addTrash(math.random(32,300-32))
            end
        end     
    end
    if isInAura == false and hasAdddedCharge == false then
        charge = charge + tempCharge
        tempCharge = 1
        hasAdddedCharge = true
    end
    charge = clamp(1,charge,maxCharge)
end

local function drawTrash()
    for index, value in ipairs(trashObjs) do
        love.graphics.setColor(1,0,0,1)
        love.graphics.rectangle("fill",value.x - 16, value.y - 16, 32,32)
    end
end

local function drawPlayer()
    if debug then
        love.graphics.setColor(1,1,1,1)
        love.graphics.rectangle("fill",playerData.x - playerConfig.scaleX/2,playerData.y - playerConfig.scaleY/2,playerConfig.scaleX,playerConfig.scaleY)
    end

    local rot = playerData.r / 180 * math.pi
    love.graphics.setColor(1,1,1,1)
    love.graphics.draw(
        rocketSprite,
        playerData.x, playerData.y,
        rot,
        1, 1,
        rocketSprite:getWidth() / 2,
        rocketSprite:getHeight() / 2
    )

    local flameQuad = flameQuads[1 + math.floor(tick.frame / 2) % #flameQuads]
    love.graphics.draw(
        flameSprites,
        flameQuad,
        playerData.x, playerData.y,
        rot,
        1, 1,
        flameWidth / 2,
        -rocketSprite:getHeight() / 2 + 8
    )
    if debug then
        love.graphics.setColor(0,0,1,1)
        love.graphics.rectangle("line",playerData.x-(playerConfig.scaleX*4)/2,playerData.y-(playerConfig.scaleY*3)/2,playerConfig.scaleX*4,playerConfig.scaleY*3)
    end
end

local function drawHearts()
    local heartW = heartSprite:getWidth()
    local heartH = heartSprite:getHeight()
    local hpSp = 4
    local hpX = hpSp * 2
    local hpY = screenHeight - heartH - hpSp * 2
    love.graphics.setColor(1,1,1,1)
    for i=0,playerData.health-1 do
        love.graphics.draw(
            heartSprite,
            hpX + i * (heartW + hpSp),
            hpY + hpSp / 2
        )
    end

    for i, v in ipairs(starBoomAnimations) do
        local frameIx = math.floor((tick.frame - v.start) / 6)
        if frameIx >= 12 then
            table.remove(starBoomAnimations, i)
        else
            love.graphics.draw(
                heartBoomSprites,
                heartBoomQuads[frameIx + 1],
                hpX + v.index * (heartW + hpSp),
                hpY + hpSp /2
            )
        end
    end
end

local function updatePlayer()
    if alive == true then
        local ix, iy = input:get "move"
        playerData.dx = lerp(playerData.dx, ix * playerConfig.maxSpeedX, playerConfig.acceleration)
        playerData.rTarg = lerp(playerData.rTarg, ix, playerConfig.rotLerp)
        playerData.r = remap(playerData.rTarg, -1, 1, -10,10)
        playerData.y = lerp(playerData.y, (iy * 50) + playerConfig.startY, 0.05)
        playerData.x = playerData.x + playerData.dx
        -- Spacing around the edges such that the rocket doesn't look odd while
        -- rotating!
        local sp = 6
        playerData.x = clamp(
            sp + playerConfig.scaleX / 2,
            playerData.x,
            screenWidth - sp - playerConfig.scaleX / 2
        )
    end
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
    if alive == true then
        for index, value in ipairs(stars) do
            value.y = value.y + value.spd
            if value.y > 500 then
                value.y = math.random(-50,-100)
            end
        end
    end
end

local function drawStars()
    for index, value in ipairs(stars) do
        ink(pal[value.c],false)
        love.graphics.rectangle("fill",value.x - value.s/2,value.y - value.s/2,value.s,value.s)
    end
end

local function drawChargeBar()
    love.graphics.setColor(1,1,1,1)
    if outlineVisible then
        love.graphics.draw(outlineSpr,playerData.x,playerData.y,0,2,2,34/2,44/2)
    end
    love.graphics.draw(chargeBarSprs, chargeBarQuads[charge],playerData.x + 20, playerData.y - 32)
end

function drawGame()
    drawStars()
    drawPlayer()
    drawTrash()
    drawChargeBar()
    drawHearts()
end

function updateGame()
    updatePlayer()
    updateStars()
    updateTrash()
    if input:pressed("back") then
        gotoMenu("pause", 0)
    end
end

function loadGame()
    stars = {}
    trashObjs = {}
    addTrash(math.random(32,300-32))
    addStars(75)
    playerData = {
        x = playerConfig.startX,
        y = playerConfig.startY,
        dx = 0,
        dy = 0,
        r = 0,
        rTarg = 0,
        health = playerConfig.startHealth
    }
    externHealth = playerData.health
    alive = true
end
