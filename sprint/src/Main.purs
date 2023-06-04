module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Sprint (Sprint(..), State(..), evalState, extract)
import Type.Data.List (type (:>), Nil')

myProgram :: Sprint (State Int :> Nil') Int
myProgram = Bind \f -> f "State" $ Put 2 \_ -> Bind \f' -> f' "State" $ Get \s -> Pure s

main :: Effect Unit
main = do
  logShow $ extract $ evalState 0 myProgram
