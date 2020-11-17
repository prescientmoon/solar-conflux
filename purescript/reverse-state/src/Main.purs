module Main where

import Prelude
import BackwardsState (BackwardsState, get, modify, put, runBackwardsState)
import Control.Monad.Reader (Reader, runReader)
import Data.Lazy (Lazy, force)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)

type LabM a
  = BackwardsState Int (Reader String) (Lazy a)

state :: LabM Int
state = do
  future <- get -- 4
  modify \a -> a * 2
  future' <- get -- 2
  put 2
  pure future

main :: Effect Unit
main = do
  let
    r = flip runReader "env" $ runBackwardsState state $ pure 0
  log "State:"
  logShow (snd r)
  log "Result:"
  logShow (force $ fst r)
