module Main where

import Prelude

import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Ref as Ref
import Game (GameState, Player(..), PlayerCommand(..), renderState, simulate)
import Graphics.Canvas (Context2D)

type AppConfig =
  { context :: Context2D
  , loop :: (Number -> Effect Unit) -> Effect Unit
  }

screenSize :: Number /\ Number
screenSize = 1000.0 /\ 700.0

initialState :: GameState
initialState =
  { birdRadius: 10.0
  , birdVelocity: 0.0
  , birdY: snd screenSize / 2.0
  , generationSettings:
      { maxPoleWidth: 40.0
      , minPoleHeight: 70.0
      , minXGap: 100.0
      , minYGap: 100.0
      , screen: screenSize
      , birdX: 100.0
      }
  , poleVelocity: 3.0
  , poles: []
  , gravity: 0.003
  , jumpForce: -0.5
  , player: Player \{ birdVelocity } -> if birdVelocity > 0.5 then Jump else Wait
  }

main :: AppConfig -> Effect Unit
main { context, loop } = do
  state <- Ref.new initialState
  loop \delta -> do
    current <- Ref.read state
    updated <- simulate delta current
    Ref.write updated state
    renderState context updated