module RunLift where

import Prelude

import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Boolean (False, True)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

{-
Get :: forall s a. (s -> a) -> State s a
-}
data State a
    = Get (Int -> a)
    | Set Int a


{-

Take each arg.
Notice an "a"? Replace it with Unit

(!s -> a) -> State !s a

=>

State !s !s



-}

class TypeEqualsBool left right res | left right -> res

instance tebT :: TypeEqualsBool l l True
else instance tebF :: TypeEqualsBool l r False

class ReturnOf :: forall k1 k2. k1 -> k2 -> Constraint
class ReturnOf f r | f -> r

instance ro :: ReturnOf b c => ReturnOf (a -> b) c
else instance ro' :: ReturnOf a a

class LiftConstructor :: (Type -> Type) -> Type -> Type -> Type -> Constraint
class LiftConstructor f from to arg | f from -> arg to 

instance lc :: (ReturnOf from (f arg), LiftConstructorImpl from to arg, ReturnOf to (f arg)) => LiftConstructor f from to arg

class LiftConstructorImpl :: Type -> Type -> Type -> Constraint
class LiftConstructorImpl from to arg | from -> to 

instance lc'' :: TypeEquals d d' => LiftConstructorImpl ((d -> d') -> result) result d'
else instance lc' :: (LiftConstructorImpl from to arg) => LiftConstructorImpl (Unit -> from) to arg
else instance lc''' :: (TypeEqualsBool f Unit isUnit ,LiftConstructorFunc isUnit (f -> from) to arg) => LiftConstructorImpl (f -> from) to arg

class LiftConstructorFunc isUnit from to arg | isUnit from -> to

instance lcf :: LiftConstructorImpl from to arg => LiftConstructorFunc False (ignore -> from) (ignore -> to) arg
else instance lct :: LiftConstructorFunc True (ignore -> result) result Unit

testReturn :: forall from f a. ReturnOf from (f a) => from -> Proxy f /\ Proxy a
testReturn _ = Proxy /\ Proxy

testLift :: forall f from to arg. LiftConstructor f from to arg => Proxy f -> from -> Proxy to /\ Proxy arg
testLift _ _ = Proxy /\ Proxy

get :: forall a. Tuple (Proxy State) (Proxy a) 
get = testReturn Get

b :: Proxy (State Int) /\ Proxy Int
b = testLift _state Get

c :: Proxy _ /\ Proxy _
c = testLift _state Set

_state :: Proxy State
_state = Proxy


-- class Basic f arg | f -> arg

-- instance basic :: Basic ((arg -> arg) -> r) arg

-- testBasic :: forall f arg. Basic f arg => f -> Proxy arg 
-- testBasic _ = Proxy

-- hmm :: Proxy _
-- hmm = testBasic (\f -> f 0 :: Int)