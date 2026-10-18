--main script

tick = require "libs/tick" --framerate lock library
local baton = require 'libs/baton' --input manager library

--helper scripts
require "scripts/helpers"
require "scripts/palette"

--scenes
require "scripts/menus"
require "scripts/game"

--states and screen stuff
screenWidth = 300
screenHeight = 400
scale = 1
state = "none"
local gameCanvas = nil
debug = false
volume = 10 -- min is 0, max is 10
camOX = 0
camOY = 0
shake = 0

--sprites and spritesheets
swapStateSprs = love.graphics.newImage("sprites/scrnTrnsGB.png")
swapStateQuads = generateQuads(swapStateSprs, 150, 200, 4, 5)
swapStateSprs:setFilter("nearest", "nearest")

chargeBarSprs = love.graphics.newImage("sprites/chargeBar.png")
chargeBarQuads = generateQuads(chargeBarSprs, 64, 64, 5, 10)
chargeBarSprs:setFilter("nearest", "nearest")

rocketSprite = love.graphics.newImage("sprites/Rocket.png")
rocketSprite:setFilter("nearest", "nearest")

outlineSpr = love.graphics.newImage("sprites/shipOutline.png")
outlineSpr:setFilter("nearest","nearest")

heartSprite = love.graphics.newImage("sprites/Spaceheart.png")
heartSprite:setFilter("nearest", "nearest")

heartBoomSprites = love.graphics.newImage("sprites/SpaceheartBoom.png")
heartBoomQuads = generateQuads(heartBoomSprites, 19, 17, 12, 1)
heartBoomSprites:setFilter("nearest", "nearest")

rocketCanSprite = love.graphics.newImage("sprites/RocketCan.png")
rocketCanSprite:setFilter("nearest", "nearest")

logoRightSprite = love.graphics.newImage("sprites/LogoRight.png")
logoRightSprite:setFilter("nearest", "nearest")

logoRightSprite = love.graphics.newImage("sprites/LogoRight.png")
logoRightSprite:setFilter("nearest", "nearest")

logoShineSprites = love.graphics.newImage("sprites/LogoShine.png")
logoShineQuads = generateQuads(logoShineSprites, 300, 62, 1, 6)
logoShineSprites:setFilter("nearest", "nearest")

flameWidth = 16
flameHeight = 18
flameSprites = love.graphics.newImage("sprites/Flames.png")
flameQuads = generateQuads(flameSprites, flameWidth, flameHeight, 5, 1)
flameSprites:setFilter("nearest", "nearest")

--audio
swapStateSfx = love.audio.newSource("audio/swapState.wav", "static")
auraSfx = love.audio.newSource("audio/aura.wav", "static")
clickSfx = love.audio.newSource("audio/click.wav", "static")
selectSfx = love.audio.newSource("audio/select.wav", "static")
local sounds = { swapStateSfx, clickSfx, selectSfx, auraSfx }

function setVolume(newVolume)
    volume = clamp(newVolume, 0, 10)
    for _, effect in pairs(sounds) do
        effect:setVolume(volume / 10)
    end
end

--fonts
mainFontSize = 32
mainFont = love.graphics.newFont("fonts/Nechao Sharp.ttf", mainFontSize, "normal", 20)
mainFont:setFilter("nearest", "nearest")

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
    debug = {'key:='},
    scale = {'key:-'}
  },
  pairs = {
    move = {'left', 'right', 'up', 'down'}
  },
  joystick = love.joystick.getJoysticks()[1],
}

local function updateShake()
    camOX = math.random(-16,16)
    camOY = math.random(-16,16)
    camOX = camOX * shake
    camOY = camOY * shake
    shake = shake*0.95
    if (shake<0.05) then shake=0 end
end

--re generate drawing canvas with screen scale
local function recreateCanvases()
    gameCanvas = love.graphics.newCanvas(screenWidth, screenHeight, { type = '2d', readable = true })
end

--draws debug stuff like the color palette and various metrics
local function drawDebug()
    local sp = 10 -- spacing
    drawPal()
    love.graphics.setColor(1, 1, 1, 1)
    love.graphics.print("FPS: "..love.timer.getFPS(), sp, 2 * sp)
    love.graphics.print("Scale: "..scale, sp, 3 * sp + mainFontSize)
    love.graphics.print("State: "..state, sp, 4 * sp + mainFontSize * 2)
    love.graphics.print("TrashTimer: "..trashTimer, sp, 5 *sp + mainFontSize* 3)
    love.graphics.print("PHealth: "..externHealth, sp, 6 *sp + mainFontSize* 4)
    love.graphics.print("Charge: "..charge, sp, 7 *sp + mainFontSize* 5)
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
    swapState("menu")
end

--main update
function love.update(dt)
    updateShake()
    --update input
    input:update()
    --toggle debug mode with F12
    if input:pressed("debug") then
        debug = not debug
    end
    --cycle scale with F11
    if input:pressed("scale") then
        cycleScale(1)
    end
    --update scene swapper
    updateSwapState()
    --update various scenes
    if state == "menu" then
        updateMenus()
    elseif state == "game" then
        updateGame()
    end
end

function cycleScale(delta)
    scale = (scale - 1 + delta) % 6 + 1
    love.window.setMode(screenWidth * scale, screenHeight * scale, {vsync = true})
    recreateCanvases()
end


function drawStuffHere()
    --main drawing function, draw things here
    love.graphics.clear(ink(pal[2],true))
    if state == "menu" then
        drawMenus()
    elseif state == "game" then
        drawGame()
    end
    drawSwapState()
end

function love.draw()
    love.graphics.setFont(mainFont)
    --drawing function to draw canvas and debug overlay
    love.graphics.clear(ink(pal[1],true))
    love.graphics.setCanvas{gameCanvas, stencil=true}
    drawStuffHere()
    love.graphics.setCanvas()
    love.graphics.draw(gameCanvas, camOX, camOY, 0, scale, scale)
    if debug then
        drawDebug()
    end
end
