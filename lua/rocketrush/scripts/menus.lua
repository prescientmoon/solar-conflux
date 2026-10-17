--temporary menus, includes settings and credits too

function drawMenus()
    love.graphics.print("Menus go here", 10, 10,0,1.5,1.5)
    love.graphics.print("press return or space to load", 10, 50,0,1,1)
end

function updateMenus()
    if input:pressed("action") then

    end
end

function loadMenus()
    
end