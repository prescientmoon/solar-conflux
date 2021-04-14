module Main where

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class.Console (error)
import Moonlog.Repl (emptyState, loop)
import Node.ReadLine (createConsoleInterface, noCompletion)
import Node.ReadLine.Aff as RL

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  runAff_ 
    (either
      (\err -> showError err *> RL.close interface)
      (const $ RL.close interface))
    (loop $ emptyState interface)
  where
    showError err = error (show err)