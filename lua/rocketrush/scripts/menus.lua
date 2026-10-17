local menu = "main"
local sp = 10 -- spacing

-- 0-based indices for the focused button in each menu
local menuFocus = { main = 0, options = 0, pause = 0 }

-- The previous menu we entered each menu from
local menuPrev = { main = nil, options = nil, pause = nil }

local function menuPos()
    if menu == "main" then
        return 70, 125
    elseif menu == "pause" then
        return 60, 30
    elseif menu == "options" then
        return 55, 30
    end
end

local function menuText(index, text)
    local x, y = menuPos()
    if index == menuFocus[menu] then
        ink(pal[8])
    else
        ink(pal[5])
    end
    love.graphics.print(text, x, y + (sp + mainFontSize) * index)
end

function drawMenus()
    if menu == "main" then
        menuText(0, "Play")
        menuText(1, "Options")
        menuText(2, "Quit")

        ink(pal[6])
        love.graphics.print(
            "A game by:\n - epujellysilly\n - moonberry\n - prescientmoon",
            45, 275, 0, 0.5, 0.5
        )
    elseif menu == "pause" then
        menuText(0, "Continue")
        menuText(1, "Options")
        menuText(2, "Quit")
    elseif menu == "options" then
        menuText(0, "Scale: " .. scale .. "x")
        menuText(1, "Volume")

        local x, y = menuPos()
        local ybar = y + 2 * (sp + mainFontSize)
        local wbar = screenWidth - 2 * sp - x
        local wseg = wbar / 10
        local barsp = 2

        for i=0,9 do
            if i >= volume then
                ink(pal[5])
            elseif menuFocus[menu] == 1 then
                ink(pal[8])
            else
                ink(pal[6])
            end

            love.graphics.rectangle(
                "fill",
                x + barsp / 2 + wseg * i,
                ybar + 5,
                wseg - barsp,
                mainFontSize-5
            )
        end
    end

    -- The keyboard-controllable focus cursor
    local x, y = menuPos()
    ink(pal[8])
    love.graphics.print(
        ">",
        x - mainFontSize - sp,
        y + (sp + mainFontSize) * menuFocus[menu]
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

function updateMenus()
    if input:pressed("up") then menuFocus[menu] = menuFocus[menu] - 1 end
    if input:pressed("down") then menuFocus[menu] = menuFocus[menu] + 1 end

    -- Normalize the foci
    menuFocus.main = menuFocus.main % 3
    menuFocus.options = menuFocus.options % 2
    menuFocus.pause = menuFocus.pause % 3

    local focus = menuFocus[menu]

    -- TODO: sound effects for button clicks
    if menu == "main" then
        if input:pressed("action") then
            if focus == 0 then
                swapState("game", loadGame)
            elseif focus == 1 then
                gotoMenu("options", 0)
            elseif focus == 2 then
                love.event.quit()
            end
        end

        if input:pressed("back") then
            love.event.quit()
        end
    elseif menu == "options" then
        ticksSinceVolChange = ticksSinceVolChange + 1

        if focus == 0 then
            if input:pressed("action") then
                cycleScale(1)
            end

            if input:pressed("left") then cycleScale(-1) end
            if input:pressed("right") then cycleScale(1) end
        end

        if focus == 1 and ticksSinceVolChange % 4 == 0 then
            if input:down("left") then
                setVolume(volume - 1)
                ticksSinceVolChange = 0
            end
            if input:down("right") then
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
