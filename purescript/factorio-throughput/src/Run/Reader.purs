module Run.Reader.Extra where

import Prelude

import Data.Lens (AGetter)
import Functorio.Lens (getAt)
import Run (Run)
import Run.Reader (READER, runReader)
import Run.State (STATE, get)
import Type.Row (type (+))

-- | Use state from the environemtn to eliminate a reader monad.
fromState :: forall r s a. Run (STATE s + READER s r) a -> Run (STATE s r) a
fromState m = get >>= flip runReader  m

-- | Focus on some state in the environemtn to eliminate a reader monad.
fromState' :: forall s t a b r x. AGetter s t a b -> Run (STATE s + READER a r) x -> Run (STATE s r) x 
fromState' optic m = getAt optic >>= flip runReader m