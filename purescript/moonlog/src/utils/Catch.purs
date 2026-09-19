module Moonlog.Utils.Catch where

import Data.Either (Either(..))

foreign import catchErrorsImpl :: forall a b e r. (e -> r) -> (b -> r) -> (a -> b) -> (a -> r)

-- | Surround a function with a try catch block. Useful for ffi stuff.
catchErrors :: forall a b e. (a -> b) -> a -> Either e b
catchErrors = catchErrorsImpl Left Right