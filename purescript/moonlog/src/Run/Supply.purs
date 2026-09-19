module Run.Supply where

import Prelude

import Data.Tuple (Tuple(..))
import Run (Run, Step(..), interpret, lift, on, runAccumPure, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | Monad providing an infinite supply of values of a particular type.
-- | Example use cases: generating unique ids.
data SupplyF s a = Supply (s -> a)

type SUPPLY s r = ( supply :: SupplyF s | r )

generate :: forall r s. Run (SUPPLY s r) s
generate = lift _supply (Supply identity)

-- | Elimininate the supply monad using a function generating the next value
runSupply :: forall r s a. (s -> s) -> s -> Run (SUPPLY s r) a -> Run r a
runSupply next
    = runAccumPure
        (next >>> \current -> on _supply (Loop <<< handleSupply current) Done)
        (\s a -> a)
    where
    handleSupply :: forall i. s -> SupplyF s i -> Tuple s i
    handleSupply current (Supply continue) = Tuple current (continue current)

-- | Like local but for supplies
localSupply :: forall r s z a. (s -> z) -> Run (SUPPLY z + SUPPLY s r) a -> Run (SUPPLY s r) a
localSupply make 
    = interpret (on _supply handleSupply send)
    where
    handleSupply :: SupplyF z ~> Run (SUPPLY s r)
    handleSupply (Supply continue) = ado
        outer <- generate
        in continue $ make outer

---------- Typeclass instances
derive instance functorSupply :: Functor (SupplyF s)

--------- SProxies
_supply :: Proxy "supply"
_supply = Proxy               