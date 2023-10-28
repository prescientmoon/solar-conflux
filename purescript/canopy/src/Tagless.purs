module Canopy.Tagless where

import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

foreign import data Nat :: Type
foreign import data S :: Nat -> Nat
foreign import data Z :: Nat

type FinDict f =
  { fz :: forall (n :: Nat). f (S n)
  , fs :: forall (n :: Nat). f n -> f (S n)
  }

type VecDict f a =
  { empty :: f Z
  , cons :: forall a (n :: Nat). a -> f n -> f (S n)
  }

type Fin n = forall f. Proxy f -> FinDict f -> f n
type Vec n a = forall f. Proxy f -> VecDict f a -> f n

newtype Const a b = Const a
newtype IndexT a n = IndexT (Vec n a -> a)

index :: forall n a. Fin n -> Const a n
index  fin = ?f
  where
  firstStep :: IndexT a n
  firstStep = fin (Proxy :: _ (IndexT a)) finDict

  finDict :: FinDict (IndexT a)
  finDict =
    { fz: IndexT \vec -> caseZero vec
    , fs: ?s
    }
    where
      caseZero :: forall n. Vec (S n) a -> a
      caseZero vec = coerce const
         where 
           const :: Const a (S n)
           const = vec (Proxy :: _ (Const a)) dict

           dict :: VecDict (Const a) a
           dict = {
              empty: ?e, 
              cons: ?c
             }


test :: Vec _ String
test = \_ d -> d.cons "hey" (d.cons "ho" d.empty)
