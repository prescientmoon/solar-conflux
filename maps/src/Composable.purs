module Composable where

import Prelude

class Transform a b where
    from :: a -> b

instance tbi :: Transform Boolean Int where
    from true = 1
    from false = 0

else instance tis :: Transform Int String where
    from = show

else instance ti :: Transform i i where
    from = identity

else instance tc :: (Transform a b, Transform b c) => Transform a c where
    from = (from :: a -> b) >>> from

{- This doensn't work:

a :: String
a = from true

 -}
