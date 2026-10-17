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

local trashTextures = {
    {
        sprite = love.graphics.newImage("sprites/Mungus.png"),
        cx = 6,
        cy = 0,
        cw = 23,
        ch = 34
    },
    {
        sprite = love.graphics.newImage("sprites/Rock.png"),
        cx = 0,
        cy = 9,
        cw = 34,
        ch = 22
    },
    {
        sprite = love.graphics.newImage("sprites/Sphere.png"),
        cx = 7,
        cy = 7,
        cw = 40,
        ch = 40
    }
}

for _,v in ipairs(trashTextures) do v.sprite:setFilter("nearest", "nearest") end

local stars = {}

local maxTrash = 16
externHealth = 0

charge = 1
local tempCharge = 1
local maxCharge = 50
local hasAdddedCharge = false

local outlineVisible = false

trashTimer = 1
local trashInterval = 100

local trashObjs = {}

local alive = true

local function dynamicTrashInterval()
    return math.floor(
        remap(
            math.min(charge, 0.75 * maxCharge),
            1, 0.75 * maxCharge,
            trashInterval, trashInterval / 4
        )
    )
end

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
        tex = math.random(1, #trashTextures),
        dy = 0,
        spd = 5 + math.random(0, 2) * (charge / maxCharge),
        accel = 0.1,
        r = 0
    }
    table.insert(trashObjs,trash)
end

local function trashCollidesWith(trash, x, y, w, h)
    local tex = trashTextures[trash.tex]
    local tw = tex.sprite:getWidth()
    local th = tex.sprite:getHeight()
    return aabb(
        trash.x - tw / 2 + tex.cx,
        trash.y - th / 2 + tex.cy,
        tex.cw, tex.ch,
        x, y, w, h
    )
end

local function updateTrash()
    if not alive then return end

    local isInAura = false
    outlineVisible = false

    if auraTimer < 5 then
        auraTimer = auraTimer + 1
    end

    for index, value in ipairs(trashObjs) do
        value.dy = lerp(value.dy,value.spd,value.accel)
        if value.y < 500 then
            value.y = value.y + value.dy
        else
            table.remove(trashObjs, index)
            goto continue
        end
        value.r = value.r + 5

        if trashCollidesWith(value, playerData.x-playerConfig.scaleX/2,playerData.y-playerConfig.scaleY/2,playerConfig.scaleX,playerConfig.scaleY) then
            addOrRemoveHealth(-1)
            shakeCam(2)
            table.remove(trashObjs, index)
            charge = math.floor(charge / 2)
            goto continue
        end
        if trashCollidesWith(value, playerData.x-(playerConfig.scaleX*4)/2,playerData.y-(playerConfig.scaleY*3)/2,playerConfig.scaleX*4,playerConfig.scaleY*3) then
            isInAura = true
            hasAdddedCharge = false
            if tempCharge < 10 and charge ~= 50 then
                outlineVisible = true
                if auraTimer >= 5 then
                    auraSfx:setPitch(remap(tempCharge,1,10,0.8,1.2))
                    love.audio.play(auraSfx)
                    tempCharge = tempCharge + 1
                    auraTimer = 1
                end
            end
        end

        ::continue::
    end

    if trashTimer < dynamicTrashInterval() then
        trashTimer = trashTimer + 1
    else
        trashTimer = 1
        if #trashObjs < maxTrash then
            addTrash(math.random(32,300-32))
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
    for index, trash in ipairs(trashObjs) do
        local tex = trashTextures[trash.tex]
        local tw = tex.sprite:getWidth()
        local th = tex.sprite:getHeight()
        if debug then
            love.graphics.setColor(1, 0, 0, 1)
            love.graphics.rectangle(
                "fill",
                trash.x - tw / 2 + tex.cx,
                trash.y - th / 2 + tex.cy,
                tex.cw, tex.ch
            )
        end

        love.graphics.setColor(1,1,1,1)
        love.graphics.draw(
            tex.sprite,
            trash.x, trash.y,
            trash.r / 180 * math.pi, 1, 1,
            tw / 2, th / 2
        )
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
    love.graphics.draw(chargeBarSprs, chargeBarQuads[math.min(50, charge+tempCharge)],playerData.x + 20, playerData.y - 32)
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
    charge = 1
    trashTimer = 1
    auraTimer = 1
    hasAdddedCharge = false
    outlineVisible = false
    addTrash(math.random(32,300-32))
end
