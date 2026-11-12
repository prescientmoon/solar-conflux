--color palette system

pal = {}
usingPal = 1
palDirs = {
    "palettes/ammo-8.hex",
}

--loads palette file into table
function initColors()
    pal = {}
    for line in love.filesystem.lines(palDirs[usingPal]) do
        table.insert(pal, line)
    end
end

--function to convert hexcode to RGB and return raw color data or set love2d color
function ink(hex,raw)
    local r, g, b, a
    if #hex == 6 then
        a = 0xFF
        r, g, b = hex:match("^(%x%x)(%x%x)(%x%x)$")
    else
        error(("Invalid hex color “%s” (expected 6 hex digits)"):format(hex))
    end
    r = tonumber(r, 16) / 255
    g = tonumber(g, 16) / 255
    b = tonumber(b, 16) / 255
    a = tonumber(a, 16) / 255
    if raw then
        return r, g, b, a
    else
        love.graphics.setColor(r, g, b, a)
    end
end

--draw palette for debug overlay
function drawPal()
    for index, value in ipairs(pal) do
        ink(value,false)
        love.graphics.rectangle("fill", (index -1) * 10, 0, 10, 10)
    end
end

--USAGE:
--  ink(pal[1],false) sets drawing color to palette index 1
--  ink(pal[1],true) does NOT set drawing color but rather returns the r, g, b, a values of that palette index
