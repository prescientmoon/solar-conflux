module Kombinator.Pair where

import Data.Lens (Traversal', wander)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude

---------- Types
-- | Tuple with both elements having the same type
type Pair a = a /\ a

---------- Lenses
-- | Focus on both elements of a tuple
_pair :: forall a. Traversal' (Pair a) a
_pair = wander \f s -> ado
  a <- f (fst s)
  b <- f (snd s)
  in a /\ b