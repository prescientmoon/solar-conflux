module Existsential where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data Test :: forall k. (k -> Type) -> Type
foreign import data Exists :: forall a b. (a -> b) -> b

mkExists :: forall f a. f a -> Exists f
mkExists = unsafeCoerce

runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r
runExists = unsafeCoerce
