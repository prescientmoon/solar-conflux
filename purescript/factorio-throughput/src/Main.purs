module Main where

import Prelude

import Data.Compactable (compact)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.HashMap as HashMap
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import RealFunction (PortSide(..), RealFunction, SolveM, _constraints, collectConstraints, myFactory, runSolveM, tryFindBound)
import Run.Except (runFail)
import Run.Reader.Extra (fromState')

p :: SolveM (Array RealFunction)
p = do
  collectConstraints
  a <- fromState' _constraints $ runFail $ tryFindBound (0 /\ Input)
  b <- fromState' _constraints $ runFail $ tryFindBound (0 /\ Output)
  c <- fromState' _constraints $ runFail $ tryFindBound (1 /\ Input)
  d <- fromState' _constraints $ runFail $ tryFindBound (1 /\ Output)
  e <- fromState' _constraints $ runFail $ tryFindBound (2 /\ Input)
  f <- fromState' _constraints $ runFail $ tryFindBound (2 /\ Output)
  g <- fromState' _constraints $ runFail $ tryFindBound (4 /\ Input)
  h <- fromState' _constraints $ runFail $ tryFindBound (4 /\ Output)
  pure $ compact [a, b, c, d, e, f, g, h]

main :: Effect Unit
main = do
  for_ (HashMap.toArrayBy Tuple myFactory) \(Tuple key value) -> log $ show key <> ": " <> show value

  case runSolveM myFactory p of
    Left err -> log err
    Right (Tuple s f) -> do
      log $ joinWith "\n" $ show <$> s.constraints
      logShow $ f <*> pure 0.0