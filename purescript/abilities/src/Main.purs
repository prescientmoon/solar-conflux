module Main where

import Prelude

import Ask (class Ask, handleAsk)
import Ask as Ask
import Effect (Effect)
import Effect.Console (logShow)
import Io (class Io, handleIo)
import Io as Io
import Abilities as Abilities

something :: Ask Int => Io => Int
something = Abilities.do
  let a = 3

  Io.assert $ a + 1 == 4
  Io.debugLog $ show a

  -- Let works just as well
  b <- Io.readFile "b.txt"

  Io.debugLog $ Io.readFile "a.txt" <> "!!"
  Io.debugLog b

  a + Ask.ask * 2

main :: Effect Unit
main = do
  result <- handleAsk 3 $ handleIo something
  result' <- handleAsk 5 $ handleIo something
  logShow [result, result']
