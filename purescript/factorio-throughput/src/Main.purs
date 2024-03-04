module Main where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Moontorio.Render (RenderFn, renderFactory)
import RealFunction (PortSide(..), RealFunction, SolveM, collectConstraints, myFactory, runSolveM, tryFindBoundSolveM)

p :: SolveM (Array RealFunction)
p = do
  collectConstraints
  a <- tryFindBoundSolveM (0 /\ Input)
  b <- tryFindBoundSolveM (0 /\ Output)
  c <- tryFindBoundSolveM (1 /\ Input)
  d <- tryFindBoundSolveM (1 /\ Output)
  e <- tryFindBoundSolveM (2 /\ Input)
  f <- tryFindBoundSolveM (2 /\ Output)
  g <- tryFindBoundSolveM (4 /\ Input)
  h <- tryFindBoundSolveM (4 /\ Output)
  pure [a, b, c, d, e, f, g, h]

main :: RenderFn -> Effect Unit
main render = do
  -- for_ (HashMap.toArrayBy Tuple myFactory) \(Tuple key value) -> log $ show key <> ": " <> show value

  case runSolveM myFactory p of
    Left err -> log err
    Right (Tuple s f) -> do
      renderFactory render myFactory s.constraints
      -- log $ joinWith "\n" $ show <$> s.constraints
      -- logShow $ f <*> pure 0.0