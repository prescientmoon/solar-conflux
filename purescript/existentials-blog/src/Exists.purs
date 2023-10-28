module Exists where


import Prelude

import Data.Exists (Exists, mkExists)

-- type Exists f = forall result. (forall a. f a (a -> result)) -> result

type Forall :: forall k. (k -> Type) -> Type -> Type
type Forall f r = forall a. f a -> r

{- type Showable a f = Show a => f
type EConstructor f = forall a. f a (a -> Exists f)

mkShowable :: EConstructor Showable
mkShowable a f = f a  -}