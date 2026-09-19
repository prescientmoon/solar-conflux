-- | Typeclass based implicit parameters
module Control.Ask (class Asks, class Ask, ask, askFor, provide, local) where

import Unsafe.Coerce (unsafeCoerce)

-- | Constraint created in order to make `Ask` un-implementable 
class Asks

-- | Ask is the type class representation of an implicit parameter.
-- | 
-- | Example:
-- | ```purescript
-- | foo :: Ask Int => Int -> String
-- | foo bar = show (bar + ask)
-- | ```
-- | The value can then later be supplied using `provide`:
-- | ```purescript
-- | (provide 4 foo) 2 -- "6"
-- | provide 4 (foo 3) -- "7"
-- | ```
class Asks <= Ask a where
    -- | Retrives an implicit parameter from the context
    ask :: a

-- | Provide an implicit parameter to a computation which requires it
provide :: forall result a. a -> (Ask a => result) -> result
provide = unsafeCoerce \context f -> f { ask: context }

-- | Run a function over an implicit parameter
-- | 
-- | > Note: Be careful while using this to map over the value without updating the type.
-- | > ```purescript
-- | > -- evaluates to `1`, not `2`
-- | > provide 1 (local ((*) 2) (ask @Int))
-- | > ``` 
local :: forall a b r. (a -> b) -> (Ask b => r) -> (Ask a => r)
local f m = provide (f ask) m

-- | Equivalent to `ask` except for accepting a `Proxy` for specifying the exact requested type
askFor :: forall a p. Ask a => p a -> a
askFor _ = ask