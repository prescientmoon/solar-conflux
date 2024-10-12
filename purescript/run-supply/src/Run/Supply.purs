-- | Effect for generating an infinite supply of values.
-- | There are a lot of helpers I could implement (skipN, fromFoldable, filter, lens integration etc), 
-- | but I haven't felt the need for any of them. If you happen to find any of them helpful you can open an issue.
module Run.Supply where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Run (Run, Step(..), interpret, lift, on, runAccumPure, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | Monad providing an infinite supply of values of a particular type.
-- | Example use cases: generating unique ids.
data SupplyF s a = Supply (s -> a)

type SUPPLY s r = ( supply :: SupplyF s | r )

-- | Generate an unique value
generate :: forall r s. Run (SUPPLY s r) s
generate = lift _supply (Supply identity)

-- | Elimininate the supply monad using a function generating the next value
runSupply :: forall r s a. (s -> s) -> s -> Run (SUPPLY s r) a -> Run r a
runSupply next
    = runAccumPure
        (\current -> on _supply (handleSupply current >>> Loop) Done)
        (\_ a -> a)
    where
    handleSupply :: s -> SupplyF s ~> Tuple s
    handleSupply current (Supply continue) = next current /\ continue current

-- | Like local but for supplies
localSupply :: forall r s z a. (s -> z) -> Run (SUPPLY z + SUPPLY s r) a -> Run (SUPPLY s r) a
localSupply make 
    = interpret (on _supply handleSupply send)
    where
    handleSupply :: SupplyF z ~> Run (SUPPLY s r)
    handleSupply (Supply continue) = ado
        outer <- generate
        in continue (make outer)

-- | Keep generating values until one of them exists. 
-- | Probably causes an infinite loop if the supplier never generates a Just.
ignoreMaybes :: forall r s. Run (SUPPLY s + SUPPLY (Maybe s) r) ~> Run (SUPPLY (Maybe s) r)
ignoreMaybes = interpret (on _supply handleSupply send)
    where
    handleSupply :: SupplyF s ~> Run (SUPPLY (Maybe s) r)
    handleSupply m@(Supply continue) = generate >>= case _ of
        Just outer -> pure $ continue outer
        Nothing -> handleSupply m

---------- Typeclass instances
derive instance functorSupply :: Functor (SupplyF s)

--------- SProxies
_supply :: Proxy "supply"
_supply = Proxy               
