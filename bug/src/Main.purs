module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Foo (foo)

main :: Effect Unit
main = liftEffect $ logShow 12
