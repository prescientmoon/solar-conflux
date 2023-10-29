module Kombinator.Graph.Undirected
  ( Graph
  , lookup
  , insert
  , delete
  , deleteConnection
  , empty
  , half
  , toHashMap
  , insertMany
  , connections
  , _atGraph
  , _atGraphConnection
  ) where

import Prelude

import Data.Array as Array
import Data.Debug (class Debug, collection, constructor, debug)
import Data.Foldable (foldr)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Lens (Lens, Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe, maybe')
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Safe.Coerce (coerce)

-- | A hashmap where `member a (lookup b h)` implies `member b (lookup a h)`
newtype Graph key = Graph (HashMap key (HashSet key))

lookup :: forall key. Hashable key => key -> Graph key -> HashSet key
lookup key (Graph hm) = fromMaybe HashSet.empty $ HashMap.lookup key hm

-- | Add a key to a Graph.
-- | insert k v == insert v k
insert :: forall key. Hashable key => key -> key -> Graph key -> Graph key
insert from to = coerce (addToSet from to >>> addToSet to from)
  where
  addToSet from to = HashMap.insertWith HashSet.union from (HashSet.singleton to)

insertMany :: forall key. Hashable key => key -> HashSet key -> Graph key -> Graph key
insertMany from = flip $ foldr (insert from)

delete :: forall key. Hashable key => key -> Graph key -> Graph key
delete key = coerce (HashMap.delete key >>> map (HashSet.delete key))

deleteConnection :: forall key. Hashable key => key -> key -> Graph key -> Graph key
deleteConnection from to = coerce (removeFromSet from to >>> removeFromSet to from)
  where
  removeFromSet from to = HashMap.update (HashSet.delete to >>> Just) from

empty :: forall t. Graph t
empty = Graph HashMap.empty

toHashMap :: forall key. Graph key -> HashMap key (HashSet key)
toHashMap (Graph hm) = hm

connections :: forall key. Ord key => Graph key -> Array (key /\ key)
connections = toHashMap >>> HashMap.toArrayBy connections >>> join >>> map orderEach >>> Array.nub
  where
  connections k v = map (k /\ _) $ HashSet.toArray v
  orderEach (a /\ b) = if a > b then a /\ b else b /\ a

half :: forall key. Ord key => Graph key -> Array key
half = connections >>> map fst

hasConnection :: forall key. Hashable key => key -> key -> Graph key -> Boolean
hasConnection from to = lookup from >>> HashSet.member to

_atGraph :: forall k. Hashable k => k -> Lens (Graph k) (Graph k) (HashSet k) (Maybe k)
_atGraph k =
  lens (lookup k) \m ->
    maybe' (\_ -> delete k m) \v -> insert k v m

_atGraphConnection :: forall k. Hashable k => k /\ k -> Lens' (Graph k) Boolean
_atGraphConnection (from /\ to) =
  lens (hasConnection from to) \whole isThere ->
    if isThere then insert from to whole
    else deleteConnection from to whole

---------- Typeclass instances
instance (Debug d, Ord d, Hashable d) => Debug (Graph d) where
  debug hm
    = connections hm
    # map (\(k /\ v) -> constructor "Pair" [ debug k, debug v ])
    # collection "Graph"