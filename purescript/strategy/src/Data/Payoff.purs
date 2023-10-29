module EG.Data.Payoff where

import Prelude
import Data.FastVect.FastVect (Vect)

newtype Payoff = Payoff Number

newtype PayoffMatrix p1 p2 = PayoffMatrix (Vect p1 (Vect p2 Payoff))

---------- Typeclass instances
derive instance Eq Payoff
derive instance Ord Payoff
derive newtype instance Show Payoff
