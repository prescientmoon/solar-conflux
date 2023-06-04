module Oof where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons)
import Safe.Coerce (coerce)

data MapId

newtype SafeMap :: Row MapId -> Type -> Type -> Type
newtype SafeMap t k v = SafeMap (Map k v)

newtype Key :: MapId -> Type -> Type
newtype Key t a = Key a

type AddKey :: forall k1. k1 -> Row k1 -> Row k1
type AddKey k r = ( includes :: k | r )

lookup :: forall t ts ts' k v. Cons "includes" t ts ts' => Ord k => Key t k -> SafeMap ts' k v -> v 
lookup (Key k) (SafeMap m) = unsafePartial $ fromJust $ Map.lookup k m

insert :: forall ts k v r. Ord k => k -> v -> SafeMap ts k v -> (forall t. Key t k /\ SafeMap (AddKey t ts) k v -> r) -> r  
insert k v m next = next $ Key k /\ coerce (Map.insert k v $ coerce m)

empty :: forall k v. SafeMap () k v
empty = SafeMap Map.empty

test :: String
test = 
    insert 0 "first" empty \(first /\ dict) -> 
        insert 1 "second" dict \(second /\ dict') ->
            insert 2 "third" dict' \(third /\ dict'') -> 
                lookup first dict'
