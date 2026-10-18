local menu = "main"
local sp = 10 -- spacing

-- 0-based indices for the focused button in each menu
local menuFocus = { main = 0, options = 0, pause = 0 }

-- The previous menu we entered each menu from
local menuPrev = { main = nil, options = nil, pause = nil }

-- These get reset every frame
local menuX = 0
local menuY = 0
local menuFoci = {}

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

function drawMenus()
    love.graphics.setColor(1, 1, 1, 1)
    love.graphics.draw(
        rocketBgSprites,
        rocketBgQuads[1 + math.floor(tick.frame / 4) % #rocketBgQuads],
        0, 0, 0, 2, 2
    )

    menuFoci = {}
    if menu == "main" then
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

        menuY = 350
        menuX = 165
        menuText(2, "Back")
    end

    -- The keyboard-controllable focus cursor
    local focus = menuFoci[1 + menuFocus[menu]]
    drawStrokedText(
        ">",
        pal[8], 3, pal[5],
        focus.x - mainFontSize - sp, focus.y + sp / 2 + 4
    )
end

function gotoMenu(kind, focus, callback)
    if state == "menu" then
        menuPrev[kind] = menu
    else
        menuPrev[kind] = nil
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
                gotoMenu("main", 0)
            end
        end

        if input:pressed("back") then
            swapState("game")
        end
    end
end
