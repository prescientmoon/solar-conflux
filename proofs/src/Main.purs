module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Naturals (reflectNat, two)

main :: Effect Unit
main = do
  logShow $ reflectNat two
