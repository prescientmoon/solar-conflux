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

local function menuText(index, text)
    if index == menuFocus[menu] then
        ink(pal[8])
    else
        ink(pal[5])
    end
    table.insert(menuFoci, { x = menuX, y = menuY })
    love.graphics.print(text, menuX, menuY + sp / 2 + 4)
    menuY = menuY + sp + mainFontSize
end

function drawMenus()
    menuFoci = {}
    if menu == "main" then
        menuX = 70
        menuY = 125

        menuText(0, "Play")
        menuText(1, "Options")
        menuText(2, "Quit")

        ink(pal[6])
        love.graphics.print(
            "A game by:\n - epujellysilly\n - moonberry\n - prescientmoon",
            45, 275, 0, 0.5, 0.5
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
            if i >= volume then
                ink(pal[5])
            elseif menuFocus[menu] == 1 then
                ink(pal[8])
            else
                ink(pal[6])
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
    ink(pal[8])
    love.graphics.print(">", focus.x - mainFontSize - sp, focus.y + sp / 2 + 4)
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
