-- | Allows the programmer to limit a monad to only run once (using a key)
module Visited (VISITED, runVisited, once) where

import Prelude

import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Run (Run)
import Run.State (State, evalStateAt, getAt, modifyAt)
import Type.Proxy (Proxy(..))

-- | Monad keeping track of all the runned monad' keys
type VISITED a r = ( visited :: State (HashSet a) | r )

-- | Eliminate the Visited effect
runVisited :: forall d a r. Hashable d => Run (VISITED d r) a -> Run r a
runVisited = evalStateAt _visited mempty

-- | Mark a key as visited
visit :: forall a r. Hashable a => a -> Run (VISITED a r) Unit
visit e = modifyAt _visited $ HashSet.insert e

-- | Condition a monad to only run once. 
-- | The first argument is a key, 
-- | and the second is a default value to use when the monad has already run.
once :: forall d a r. Hashable d => d -> Run (VISITED d r) a -> Run (VISITED d r) a -> Run (VISITED d r) a 
once at default m = do
    visited <- getAt _visited
    if HashSet.member at visited
        then default
        else visit at *> m

_visited :: Proxy "visited"
_visited = Proxy