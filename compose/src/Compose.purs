module Compose where

import Prelude hiding (bind, discard)


bind :: forall a b c. (a -> b) -> (b -> c) -> (a -> c)
bind = (>>>)

discard :: forall a b c. (a -> b) -> (Unit -> b -> c) -> (a -> c)
discard f g = f >>> g unit