module Functorio.Lens where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as H
import Data.HashSet (HashSet)
import Data.HashSet as S
import Data.Hashable (class Hashable)
import Data.Lens (AGetter, Fold, Iso', Lens', Setter, iso, lens, over, preview, set, view)
import Data.Maybe (Maybe(..), maybe')
import Data.Maybe.First (First)
import Run (Run)
import Run.Reader (READER, ask)
import Run.State (STATE, get, modify)

---------- Missing instances
atHashMap :: forall k v. Hashable k => k -> Lens' (HashMap k v) (Maybe v)
atHashMap k =
    lens (H.lookup k) \m ->
      maybe' (\_ -> H.delete k m) \v -> H.insert k v m

-- | At implementation for hash sets
atHashSetRaw :: forall v. Hashable v => v -> Lens' (HashSet v) (Maybe Unit)
atHashSetRaw x = lens get (flip update)
    where
      get xs =
        if S.member x xs
           then Just unit
           else Nothing
      update Nothing = S.delete x
      update (Just _) = S.insert x

-- | Boolean implementation for AT on hash sets
atHashSet :: forall v. Hashable v => v -> Lens' (HashSet v) Boolean
atHashSet v = atHashSetRaw v <<< maybeUnitToBoolean

-- | Helper fro implementing atHashSet'
maybeUnitToBoolean :: Iso' (Maybe Unit) Boolean
maybeUnitToBoolean = iso to from
    where
    from true = Just unit
    from false = Nothing 
    
    to Nothing = false
    to _ = true 

--------- Helpers for monadic state
getAt :: forall s t a b r. AGetter s t a b -> Run (STATE s r) a
getAt optic = view optic <$> get

getPreview :: forall r s t a b. Fold (First a) s t a b -> Run (STATE s r) (Maybe a)
getPreview optic = preview optic <$> get

setAt :: forall s a b r. Setter s s a b -> b -> Run (STATE s r) Unit
setAt optic value = set optic value # modify

modifyAt :: forall s a b r. Setter s s a b -> (a -> b) -> Run (STATE s r) Unit
modifyAt optic f = over optic f # modify

askAt :: forall s t a b r. AGetter s t a b -> Run (READER s r) a
askAt optic = ask <#> view optic  