module Main where

import Prelude

import Benchmarks (benchSum)
import Benchotron.UI.Console (runSuite)
import Control.Monad.ST.Internal as ST
import Control.Monad.ST.Internal as STRef
import Effect (Effect)

factorial :: Int -> Int
factorial to = ST.run do
  result <- STRef.new 1
  ST.for 2 to \index -> do
    current <- STRef.read result
    STRef.write (current * index) result
  STRef.read result

main :: Effect Unit
main = runSuite [benchSum]