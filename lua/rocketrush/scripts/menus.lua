--temporary menus, includes settings and credits too

function drawMenus()
    love.graphics.setFont(exosphere)
    love.graphics.print("Menus go here", 10, 10,0, 0.125, 0.125)
    love.graphics.print("press return or space to load", 10, 50,0, 0.1,0.1)
end

function updateMenus()
    if input:pressed("action") then
        swapState("game", loadGame)
    end
end

function loadMenus()

end
