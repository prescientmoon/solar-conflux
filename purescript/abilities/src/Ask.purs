module Ask where

import Prelude hiding (bind,discard)

import Unsafe.Coerce (unsafeCoerce)

class Ask a where
    ctx :: a

pure :: forall ctx a. a -> Ask ctx => a
pure a = a

foreign import ask :: forall ctx. ((Ask ctx) => ctx)

handleAsk :: forall a ctx. ctx -> (Ask ctx => a) -> a
handleAsk = unsafeCoerce \c f -> f { ctx: c }
