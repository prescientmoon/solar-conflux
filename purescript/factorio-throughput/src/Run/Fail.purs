module Run.Fail.Extra where

import Prelude

import Data.Compactable (class Compactable, compact)
import Data.Traversable (class Traversable, traverse)
import Run (Run)
import Run.Except (FAIL, runFail)

-- | `Compact` / `MapMaybe` usnig the `Fail` ability
traverseFail :: forall r t a b. Compactable t => Traversable t => (a -> Run (FAIL r) b) -> t a -> Run r (t b)
traverseFail f = traverse (f >>> runFail) >>> map compact