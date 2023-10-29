module Game where

import Prelude

import Data.Array (filter)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Random (randomRange)
import Graphics.Canvas (Context2D, arc, clearRect, fillPath, fillRect, setFillStyle)
import Math (tau)

type Pole =
  { width :: Number
  , gapY :: Number
  , gapHeight :: Number
  , x :: Number
  }

type GameState =
  { birdY :: Number
  , birdVelocity :: Number
  , poleVelocity :: Number
  , birdRadius :: Number
  , poles :: Array Pole
  , generationSettings :: GenerationSettings
  , gravity :: Number
  , player :: Player
  , jumpForce :: Number
  }

type GenerationSettings
  =
  { minYGap :: Number
  , minXGap :: Number
  , screen :: Number /\ Number
  , minPoleHeight :: Number
  , maxPoleWidth :: Number
  , birdX :: Number
  }

newtype Player = Player (GameState -> PlayerCommand)

data PlayerCommand
  = Jump
  | Wait

renderState :: Context2D -> GameState -> Effect Unit
renderState ctx state = do
  clearRect ctx
    { x: 0.0
    , y: 0.0
    , width: fst state.generationSettings.screen
    , height: snd state.generationSettings.screen
    }
  setFillStyle ctx "green"
  for_ state.poles \pole -> do
    fillRect ctx 
     {  x: pole.                          }

  setFillStyle ctx "blue"
  fillPath ctx do
    arc ctx
      { start: 0.0
      , end: tau
      , radius: state.birdRadius
      , x: state.generationSettings.birdX
      , y: state.birdY
      }

simulate :: Number -> GameState -> Effect GameState
simulate delta { jumpForce, player, birdRadius, generationSettings, gravity, birdY, birdVelocity, poleVelocity, poles } = do
  let
    newVelocity = birdVelocity + gravity * delta
    state =
      { birdVelocity: newVelocity
      , poleVelocity
      , gravity
      , birdRadius
      , generationSettings
      , jumpForce
      , birdY: clamp birdRadius
          (snd generationSettings.screen - birdRadius)
          (birdY + delta * newVelocity)
      , poles: poles # movePoles (delta * poleVelocity)
          # deleteOffScreenPoles
      , player
      }

  newPoles <- generatePoles state
  let
    state' = state
      { poles = state.poles <> newPoles
      }
  pure $ runPlayer state'

runPlayer :: GameState -> GameState
runPlayer state | Player player <- state.player = case player state of
  Wait -> state
  Jump -> state
    { birdVelocity = state.jumpForce
    }

movePoles :: Number -> Array Pole -> Array Pole
movePoles amount = map (movePole amount)

movePole :: Number -> Pole -> Pole
movePole amount pole = pole
  { x = pole.x - amount }

deleteOffScreenPoles :: Array Pole -> Array Pole
deleteOffScreenPoles = filter isOffScreen
  where
  isOffScreen { x, width } = x + width < 0.0

generatePoles :: GameState -> Effect (Array Pole)
generatePoles state =
  if lastPoleBound + state.generationSettings.minXGap < fst state.generationSettings.screen then
    do
      y <- randomRange state.generationSettings.minPoleHeight
        ( snd state.generationSettings.screen
            - state.generationSettings.minPoleHeight
            - state.generationSettings.minYGap
        )
      pure
        [ { width: state.generationSettings.maxPoleWidth
          , gapHeight: state.generationSettings.minYGap
          , gapY: y
          , x: fst state.generationSettings.screen + state.generationSettings.minXGap
          }
        ]
  else pure []
  where
  lastPoleBound :: Number
  lastPoleBound = case Array.last state.poles of
    Nothing -> 0.0
    Just pole -> pole.x + pole.width