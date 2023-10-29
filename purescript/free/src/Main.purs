module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Lambda (eval, test)

main :: Effect Unit
main = do
  logShow $ eval test
