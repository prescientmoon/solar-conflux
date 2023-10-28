module Ordering where

import Prim.Boolean (False, True, kind Boolean)
import Prim.Ordering (kind Ordering)

-- Compare 2 orderings
class OrdsAreEqual (a :: Ordering) (b :: Ordering) (result :: Boolean) | a b -> result

instance oaeGT :: OrdsAreEqual a a True
else instance oaeGeneral :: OrdsAreEqual a b False