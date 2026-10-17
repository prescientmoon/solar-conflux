--temporary menus, includes settings and credits too
local menu = "main"
local focus = 0 -- the index of the focused button
local sp = 10 -- spacing

local function menuPos()
    if menu == "main" then
        return 70, 125
    end
end

local function menuText(index, text)
    local x, y = menuPos()
    if index == focus then
        ink(pal[8])
    else
        ink(pal[5])
    end
    love.graphics.print(text, x, y + (sp + mainFontSize) * index, rot)
end

function drawMenus()
    if menu == "main" then
        menuText(0, "Play")
        menuText(1, "Options")
        menuText(2, "Quit")

        local x, y = menuPos()
        ink(pal[8])
        love.graphics.print(">", x - mainFontSize - sp, y + (sp + mainFontSize) * focus)

        ink(pal[6])
        love.graphics.print("A game by:\n - epujellysilly\n - moonberry\n - prescientmoon", 60, 275, 0, 0.4, 0.5)
    elseif menu == "options" then
        love.graphics.print("Options menu!", 10, 10,0, 0.125, 0.125)
    end
end

function updateMenus()
    local ix, iy = input:get "move"

    if input:pressed("up") then
        focus = focus - 1
    end
    if input:pressed("down") then
        focus = focus + 1
    end

    if menu == "main" then
        focus = focus % 3
    end

    if input:pressed("action") then
        swapState("game", loadGame)
    end
end

function loadMenus()

end
