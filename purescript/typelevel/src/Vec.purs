module Vec where

import Num

import Data.Tuple (Tuple)
import Prim.Ordering (LT)
import Undefined (undefined)

foreign import data Vec :: Num -> Type -> Type

-- Helpers
cons :: forall length a. a -> Vec length a -> Vec (Succ length) a
cons = undefined

nil :: forall a. Vec Zero a 
nil = undefined

merge :: forall l1 l2 l3 a. Add l1 l2 l3 => Vec l1 a -> Vec l2 a -> Vec l3 a
merge = undefined

lookup :: forall length index a. Compare index length LT => Vec length a -> NProxy index -> a
lookup = undefined

take :: forall l i a. Compare i l LT => NProxy i -> Vec l a -> Vec i a
take = undefined

drop :: forall l l' i a. Sub l i l' => NProxy i -> Vec l a -> Vec l' a
drop = undefined

product :: forall l l' l'' a b. Multiply l l' l'' => Vec l a -> Vec l' b -> Vec l'' (Tuple a b)
product = undefined

-- Test
myVec :: Vec Three String
myVec = undefined

first :: String
first = lookup myVec zero 

second :: String
second = lookup myVec one

third :: String
third = lookup myVec two

getFourth :: forall a l. Compare Three l LT => Vec l a -> a
getFourth a = lookup a three 

lastTwo :: Vec Two String
lastTwo = drop two (cons "something" myVec)

productTest :: Vec (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))) (Tuple String Int)
productTest = product (myVec) (cons 1 (cons 4 (cons 7 nil)))