menu = "main"
local sp = 10 -- spacing

-- 0-based indices for the focused button in each menu
local menuFocus = { main = 0, options = 0, pause = 0, gameover = 0 }

-- The previous menu we entered each menu from
local menuPrev = { main = nil, options = nil, pause = nil, gameover = 0 }

-- These get reset every frame
local menuX
local menuY
local menuFoci

local scoreX
local scoreY
local scoreRow
local score
local timePerScoreRow
local scoreDone

-- Get reset when switching
local mainMenuTimer = 0
local gameOverTimer = 0

local function drawStrokedText(text, color, outline, ocolor, x, y, r, sx, sy)
    ink(ocolor)
    for i=-outline,outline do
        for j=-outline,outline do
            if i == 0 and j == 0 then goto continue end
            love.graphics.print(text, i + x, j + y, r, sx, sy)
            ::continue::
        end
    end
    ink(color)
    love.graphics.print(text, x, y, r, sx, sy)
end

local function menuText(index, text)
    table.insert(menuFoci, { x = menuX, y = menuY })
    local color = pal[6]
    local ocolor = pal[3]
    if index == menuFocus[menu] then
        color = pal[8]
        ocolor = pal[5]
    end
    drawStrokedText(text, color, 3, ocolor, menuX, menuY + sp / 2 + 4)
    menuY = menuY + sp + mainFontSize
end

local function drawBordered(border, bcolor, color, x, y, w, h)
    ink(bcolor)
    love.graphics.rectangle(
        "fill",
        x - border,
        y - border,
        w + 2 * border,
        h + 2 * border
    )

    ink(color)
    love.graphics.rectangle("fill", x, y, w, h)
end

local function scoreText(text, amount, kind)
    local color = pal[6]
    local ocolor = pal[3]
    local scoreSp = 2
    local mult = 2
    if kind == "final" then 
        color = pal[8]
        ocolor = pal[6]
    end
    if timePerScoreRow * scoreRow > gameOverTimer then return end
    if timePerScoreRow * scoreRow == gameOverTimer then
        clickSfx:clone():play()
        timePerScoreRow = math.floor(timePerScoreRow * 0.9)
        clickSfx:setPitch(clickSfx:getPitch()*1.05)
    end
    drawStrokedText(
        text,
        color, 2, ocolor,
        scoreX, scoreY + scoreSp / 2 + 4,
        0, 1 / mult, 1 / mult
    )
    local w = mainFont:getWidth("" .. amount)
    drawStrokedText(
        "" .. amount,
        color, 2, ocolor,
        screenWidth - w / mult - 20, scoreY + scoreSp / 2 + 4,
        0, 1/mult, 1/mult
    )
    scoreY = scoreY + scoreSp + mainFontSize / 2
    scoreRow = scoreRow + 1

    if kind == nil then score = score + amount end
end

function drawMenus()
    love.graphics.setColor(1, 1, 1, 1)
    love.graphics.draw(
        rocketBgSprites,
        rocketBgQuads[1 + math.floor(tick.frame / 4) % #rocketBgQuads],
        0, 0, 0, 2, 2
    )

    menuFoci = {}
    if menu == "main" then
        love.graphics.setColor(1, 1, 1, 1)
        local startupAnimationTime = 120
        local rocketCanRot = math.pi / 4
        local rcw = rocketCanSprite:getWidth()
        local rch = rocketCanSprite:getHeight()
        if mainMenuTimer < startupAnimationTime then
            local traw = remap(mainMenuTimer, 0, startupAnimationTime, 0, 1)
            local texp
            if traw >= 0.9 then
                texp = remap(
                    1 - math.pow(1 - remap(traw, 0.9, 1, 0, 1), 2),
                    0, 1,
                    0.9, 1
                )
            elseif traw >= 0.7 and traw < 0.9 then
                texp = remap(
                    1 - math.pow(1 - remap(traw, 0.7, 0.9, 0, 1), 2),
                    0, 1,
                    0.93, 0.9
                )
            elseif traw >= 0.3 and traw < 0.7 then
                texp = remap(
                    1 - math.pow(1 - remap(traw, 0.3, 0.7, 0, 1), 2),
                    0, 1,
                    0.9, 0.93
                )
            else
                texp = remap(
                    math.pow(remap(traw, 0, 0.3, 0, 1), 4),
                    0, 1,
                    0, 0.9
                )
            end
            local t = remap(texp, 0, 1, 70, 0)
            local ldx = t - 50 * (1 - math.pow(remap(texp,0, 1, -1,1), 2))
            local ldy = 7 * t
            local rdx = math.floor(t * 2.3)
            local rdy = 8 * t
            local cdx = -6 * t
            local cdy = 0
            love.graphics.draw(
                rocketCanSprite,
                98 + cdx + rcw / 2,
                -14 + cdy + rch / 2,
                rocketCanRot, 1, 1,
                rcw / 2,
                rch / 2
            )
            love.graphics.draw(logoLeftSprite, 1 + ldx, 24 + ldy)
            love.graphics.draw(logoRightSprite, 146 + rdx, 24 + rdy)
        elseif mainMenuTimer == startupAnimationTime then
            freezeFrames(1)
        else
            local shineFrame = 0
            if (mainMenuTimer - startupAnimationTime + 90) % 120 > 105 then
                shineFrame = 1 + math.floor(
                    (mainMenuTimer - startupAnimationTime + 90) % 15 / 3
                )
            end
            love.graphics.draw(
                rocketCanSprite,
                98 + rcw / 2,
                -14 + rch / 2,
                rocketCanRot,
                1, 1,
                rcw / 2,
                rch / 2
            )
            love.graphics.draw(
                logoShineSprites,
                logoShineQuads[1 + shineFrame],
                0, 0, 0, 1, 1
            )

            menuX = 100
            menuY = 175

            menuText(0, "Play")
            menuText(1, "Options")
            menuText(2, "Quit")

            drawStrokedText(
                "A game by:\n - epujellysilly\n - moonberry\n - prescientmoon",
                pal[7], 2, pal[4],
                55, 320, 0, 0.5, 0.5
            )
        end
        mainMenuTimer = mainMenuTimer + 1
    elseif menu == "pause" then
        menuX = 60
        menuY = 30

        menuText(0, "Continue")
        menuText(1, "Options")
        menuText(2, "Quit")
    elseif menu == "options" then
        menuX = 55
        menuY = 30

        menuText(0, "Scale: " .. scale .. "x")
        menuText(1, "Volume")

        local wbar = screenWidth - sp - menuX - 5
        local wseg = wbar / 10
        local barsp = 2
        local barHeight = mainFontSize - 10

        for i=0,9 do
            if menuFocus[menu] == 1 then
                if i >= volume then
                    ink(pal[5])
                else
                    ink(pal[8])
                end
            else
                if i >= volume then
                    ink(pal[3])
                else
                    ink(pal[6])
                end
            end

            love.graphics.rectangle(
                "fill",
                menuX + barsp / 2 + wseg * i,
                menuY + sp / 2,
                wseg - barsp,
                barHeight
            )
        end

        menuX = 165
        menuY = 350
        menuText(2, "Back")
    elseif menu == "gameover" then
        scoreX = 20
        scoreY = 160
        scoreRow = 0
        score = 0

        if trashDodged > 0 then
            scoreText("Dodge x" .. trashDodged .. ":", trashDodged * 2)
        end

        if trashPierced > 0 then
            scoreText("Pierce x" .. trashPierced .. ":", trashPierced * 75)
        end

        if monsterHits > 0 then
            scoreText("Attack x" .. monsterHits .. ":", monsterHits * 15)
        end

        if overheal > 0 then
            scoreText("Overheal x" .. overheal .. ":", overheal * 25)
        end

        if hitsTaken > 0 then
            scoreText("Got hit x" .. hitsTaken .. ":", hitsTaken * (-25))
        else
            scoreText("Untouchable:", 250)
        end

        if gameOver == 2 then
            scoreText("Victory bonus:", 100)
        end

        local t = remap(gameTimer, 0, 3600, 0, 1)
        local timeBonus = math.floor(remap(t * t * t, 0, 1, 1000, 0))
        if timeBonus > 0 and gameOver == 1 then timeBonus = 0 end
        if gameTimer <= 3600 or gameTimer >= 3600 * 2 then
            if timeBonus ~= 0 then
                scoreText("Time bonus:", timeBonus)
            end
        end

        scoreY = 290
        if score >= 3000 then
            scoreText("Catgirl status:", score,"final")
        elseif score >= 2000 then
            scoreText("Beyond infinity:", score,"final")
        elseif score >= 1000 then
            scoreText("Big winner:", score,"final")
        elseif score >= 750 then
            scoreText("Total!!!", score,"final")
        elseif score >= 500 then
            scoreText("Total!", score,"final")
        elseif score > 0 then
            scoreText("Total:", score,"final")
        elseif score < 0 then
            scoreText("Baffling...", score,"final")
        else
            scoreText("A strange void!", ":3","final")
        end

        local sprite
        if gameOver == 1 then
            mainMusic:setFilter { type="highpass",  lowgain=0.01,volume=0.7 }
            mainMusic:setPitch(0.7)
            sprite = gameOverDefeatSprite
        elseif gameOver == 2 then
            mainMusic:setFilter { type="highpass",  lowgain=0.1,volume=0.7 }
            mainMusic:setPitch(1.3)
            sprite = gameOverWinSprite
        else
            error("wtf, the game isn't over...")
        end

        love.graphics.setColor(1, 1, 1, 1)
        love.graphics.draw(
            sprite, 0, 10
        )

        scoreDone = timePerScoreRow * scoreRow <= gameOverTimer
        if scoreDone then
            menuX = 60
            menuY = 310
            menuText(0, "Go again?")
            menuText(1, "Enough...")
        end

        gameOverTimer = gameOverTimer + 1
    end

    -- The keyboard-controllable focus cursor
    local focus = menuFoci[1 + menuFocus[menu]]
    if focus ~= nil then
        drawStrokedText(
            ">",
            pal[8], 3, pal[5],
            focus.x - mainFontSize - sp, focus.y + sp / 2 + 4
        )
    end
end

function gotoMenu(kind, focus, callback)
    if state == "menu" then
        menuPrev[kind] = menu
    else
        menuPrev[kind] = nil

        if kind == "main" then
            mainMenuTimer = 0
        end
    end

    if kind == "gameover" then
        gameOverTimer = 0
        timePerScoreRow = 30
        clickSfx:setPitch(1)
        scoreDone = false
    end

    swapState("menu", function()
        menu = kind
        if focus ~= nil then menuFocus[menu] = focus end
        if callback then callback() end
    end)
end

local ticksSinceVolChange = 0 -- :(

local function changeFocus(delta)
    menuFocus[menu] = menuFocus[menu] + delta
    selectSfx:clone():play()

    -- Normalize the foci
    menuFocus.main = menuFocus.main % 3
    menuFocus.options = menuFocus.options % 3
    menuFocus.pause = menuFocus.pause % 3
end

function updateMenus()
    if input:pressed("up") then changeFocus(-1) end
    if input:pressed("down") then changeFocus(1) end

    local focus = menuFocus[menu]

    -- TODO: sound effects for button clicks
    if menu == "main" then
        if input:pressed("action") then
            if focus == 0 then
                swapState("game", loadGame)
            elseif focus == 1 then
                gotoMenu("options", 0)
            elseif focus == 2 then
                clickSfx:clone():play()
                love.event.quit()
            end
        end

        if input:pressed("back") then
            clickSfx:clone():play()
            love.event.quit()
        end
    elseif menu == "options" then
        ticksSinceVolChange = ticksSinceVolChange + 1

        if input:pressed("action") then
            if focus == 0 then
                clickSfx:clone():play()
                cycleScale(1)
            elseif focus == 2 then
                gotoMenu(menuPrev.options)
            end
        end

        if focus == 0 then
            if input:pressed("left") then
                clickSfx:clone():play()
                cycleScale(-1)
            end

            if input:pressed("right") then
                clickSfx:clone():play()
                cycleScale(1)
            end
        end

        if focus == 1 and ticksSinceVolChange % 4 == 0 then
            if input:down("left") then
                selectSfx:clone():play()
                setVolume(volume - 1)
                ticksSinceVolChange = 0
            end

            if input:down("right") then
                selectSfx:clone():play()
                setVolume(volume + 1)
                ticksSinceVolChange = 0
            end
        end

        if input:pressed("back") then
            gotoMenu(menuPrev.options)
        end
    elseif menu == "pause" then
        if input:pressed("action") then
            if focus == 0 then
                swapState("game")
            elseif focus == 1 then
                gotoMenu("options", 0)
            elseif focus == 2 then
                mainMenuTimer=0
                gotoMenu("main", 0)
            end
        end

        if input:pressed("back") then
            swapState("game")
        end
    elseif menu == "gameover" then
        if input:pressed("action") then
            if focus == 0 then
                swapState("game", loadGame)
            elseif focus == 1 then
                mainMenuTimer=0
                gotoMenu("main")
            end
        end

        if input:pressed("back") then
            mainMenuTimer=0
            gotoMenu("main")
        end
    end
end
