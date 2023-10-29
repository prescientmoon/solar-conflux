module EG.Data.Tuple (TupleIndex, left, right, lookupPair, lookup, other) where

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.HeytingAlgebra (not)
import Data.Tuple.Nested (type (/\), (/\))
import Safe.Coerce (coerce)

-- | Position in a tuple. 
-- | Logically equivalent to a boolean
-- | Constructable using `left` and `right`
newtype TupleIndex = TupleIndex Boolean

other :: TupleIndex -> TupleIndex
other = coerce (not :: Boolean -> Boolean)

left :: TupleIndex
left = TupleIndex false

right :: TupleIndex
right = TupleIndex true

lookupPair :: forall a. TupleIndex -> a /\ a -> a
lookupPair (TupleIndex inner) (a /\ b) =
  if inner then b
  else a

lookup :: forall a b. TupleIndex -> a /\ b -> a \/ b
lookup (TupleIndex inner) (a /\ b) =
  if inner then Right b
  else Left a

