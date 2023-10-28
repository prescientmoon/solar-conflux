module Abilities where

import Prelude hiding (bind, discard)

bind :: forall a b. a -> (a -> b) -> b
bind = (#)

discard :: forall a b. a -> (a -> b) -> b
discard = bind