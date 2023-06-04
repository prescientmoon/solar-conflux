module Data.Fin where

import Prelude

import Data.Leibniz (type (~), coerce, liftLeibniz, liftLeibniz1of2, lowerLeibniz, refute, symm)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

data Nat

foreign import data Z :: Nat
foreign import data S :: Nat -> Nat

foreign import data Add :: Nat -> Nat -> Nat

addZ :: forall a. Add Z a ~ a
addZ = unsafeCoerce \u -> u

addS :: forall a b. Add (S a) b ~ S (Add a b)
addS = unsafeCoerce \u -> u

data Vec s a
  = Nil (s ~ Z)
  | Cons a (forall r. (forall p. S p ~ s -> Vec p a -> r) -> r)

nil :: forall a. Vec Z a
nil = Nil identity

cons :: forall a n. a -> Vec n a -> Vec (S n) a
cons head tail = Cons head \f -> f identity tail

data Fin n
  = FZ (forall r. (forall p. S p ~ n -> r) -> r)
  | FS (forall r. (forall p. S p ~ n -> Fin p -> r) -> r)

fz :: forall n. Fin (S n)
fz = FZ \f -> f identity

fs :: forall n. Fin n -> Fin (S n)
fs p = FS \f -> f identity p

refuteFinZero :: Fin Z -> Void
refuteFinZero (FZ oops) = oops \eq -> refute (liftLeibniz eq :: Proxy _ ~ Proxy _)
refuteFinZero (FS oops) = oops \eq _ -> refute (liftLeibniz eq :: Proxy _ ~ Proxy _)

succInj :: forall a b. S a ~ S b -> a ~ b
succInj = lowerLeibniz

lookup :: forall l a. Vec l a -> Fin l -> a
lookup (Cons a _) (FZ _) = a
lookup (Cons _ nextVec) (FS nextFin) =
  nextFin \eq finP ->
    nextVec \eq' vecP -> do
      let trans = liftLeibniz (succInj (eq >>> symm eq'))
      lookup vecP (coerce trans finP)
lookup (Nil lenIsZero) fin = absurd $ refuteFinZero (coerce finIsFinZero fin)
  where
  finIsFinZero :: Fin l ~ Fin Z
  finIsFinZero = liftLeibniz lenIsZero

concat :: forall n m a. Vec n a -> Vec m a -> Vec (Add n m) a
concat (Nil nIsZ) other = coerce
  (liftLeibniz1of2 (symm addZ >>> liftLeibniz1of2 (symm nIsZ)))
  other
concat (Cons e nextVec) other = nextVec \eq vecP -> do
  coerce
    (liftLeibniz1of2 (symm addS >>> liftLeibniz1of2 eq))
    (cons e (concat vecP other))

