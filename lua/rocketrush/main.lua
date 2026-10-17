--main script

local tick = require "libs/tick" --framerate lock library
local baton = require 'libs/baton' --input manager library

--helper scripts
require "scripts/helpers"
require "scripts/palette"

--scenes
require "scripts/menus"
require "scripts/game"

--states and screen stuff
local screenWidth = 300
local screenHeight = 400
local scale = 1
state = "none"
local gameCanvas = nil
debug = false

--sprites and spritesheets
swapStateSprs = love.graphics.newImage("sprites/scrnTrnsGB.png")
swapStateQuads = generateQuads(swapStateSprs, 150, 200, 4, 5)
swapStateSprs:setFilter("nearest", "nearest")

--audio
swapStateSfx = love.audio.newSource("audio/swapState.wav", "static")

--disable AA
love.graphics.setDefaultFilter('nearest', 'nearest')

--config input
input = baton.new {
  controls = {
    left = {'key:left', 'key:a', 'axis:leftx-', 'button:dpleft'},
    right = {'key:right', 'key:d', 'axis:leftx+', 'button:dpright'},
    up = {'key:up', 'key:w', 'axis:lefty-', 'button:dpup'},
    down = {'key:down', 'key:s', 'axis:lefty+', 'button:dpdown'},
    action = {'key:space','key:return', 'button:a'},
    back = {'key:escape', 'button:b', 'button:start'},
    debug = {'key:f12'},
    scale = {'key:f11'}
  },
  pairs = {
    move = {'left', 'right', 'up', 'down'}
  },
  joystick = love.joystick.getJoysticks()[1],
}

--re generate drawing canvas with screen scale
local function recreateCanvases()
    gameCanvas = love.graphics.newCanvas(screenWidth, screenHeight, { type = '2d', readable = true })
end

--draws debug stuff like the color palette and various metrics
local function drawDebug()
    drawPal()
    love.graphics.setColor(1, 1, 1, 1)
    love.graphics.print("FPS: "..love.timer.getFPS(), 10, 10)
    love.graphics.print("Scale: "..scale, 10, 30)
    love.graphics.print("State: "..state, 10, 50)
end

--main init
function love.load()
    --locks FR to 60
    tick.framerate = 60
    tick.rate = 0.0166667
    --setup window
    love.window.setTitle('MJG-GAME')
    recreateCanvases()
    love.window.setMode( screenWidth*scale,screenHeight*scale,{vsync = true})
    --initialize colors and palette
    initColors()
    --swap scene to menus
    swapState("menu", loadMenus)
end

--main update
function love.update(dt)
    --update input
    input:update()
    --toggle debug mode with F12
    if input:pressed("debug") then
        debug = not debug
    end
    --cycle scale with F11
    if input:pressed("scale") then
        if scale < 6 then
            scale = scale + 1
            love.window.setMode( screenWidth*scale,screenHeight*scale,{vsync = true})
            recreateCanvases()
        else
            scale = 1
            love.window.setMode( screenWidth*scale,screenHeight*scale,{vsync = true})
            recreateCanvases()
        end
    end
    --update scene swapper
    updateSwapState()
    --update various scenes
    if state == "menu" then
        updateMenus()
    end
end

function drawStuffHere()
    --main drawing function, draw things here
    love.graphics.clear(ink(pal[1],true))
    if state == "menu" then
        drawMenus()
    end
    drawSwapState()
end

function love.draw()
    --drawing function to draw canvas and debug overlay
    love.graphics.clear(ink(pal[1],true))
    love.graphics.setCanvas{gameCanvas, stencil=true}
    drawStuffHere()
    love.graphics.setCanvas()
    love.graphics.draw(gameCanvas, 0, 0, 0, scale, scale)
    if debug then
        drawDebug()
    end
end

