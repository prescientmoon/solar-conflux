module Canopy.Adjudecate where

import Prelude

import Control.Bind (bind)
import Data.Foldable (class Foldable, any, foldMap, foldlDefault, foldrDefault)
import Data.List (List(..))
import Data.List as List
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons)

type Index = Int

data Proposition a
  = False
  | True
  | Disj (Proposition a) (Proposition a)
  | Conj (Proposition a) (Proposition a)
  | Negation (Proposition a)
  | Other a

derive instance Functor Proposition

instance Apply Proposition where
  apply = ap

instance Applicative Proposition where
  pure = Other

instance Bind Proposition where
  bind (Other a) f = f a
  bind False _ = False
  bind True _ = True
  bind (Negation p) f = Negation (bind p f)
  bind (Conj l r) f = Conj (bind l f) (bind r f)
  bind (Disj l r) f = Disj (bind l f) (bind r f)

instance Foldable Proposition where
  foldMap f (Other a) = f a
  foldMap f (Disj l r) = foldMap f l <> foldMap f r
  foldMap f (Conj l r) = foldMap f l <> foldMap f r
  foldMap f (Negation p) = foldMap f p
  foldMap _ _ = mempty

  foldr = foldrDefault
  foldl = foldlDefault

instance Monad Proposition

derive instance Eq a => Eq (Proposition a)

simp :: forall a. Eq a => Proposition a -> Proposition a
simp (Conj False _) = False
simp (Conj _ False) = False
simp (Disj True _) = True
simp (Disj _ True) = True
simp (Negation True) = False
simp (Negation False) = True
simp (Negation (Negation p)) = simp p
simp (Conj l r) | l == r = simp l
simp (Disj l r) | l == r = simp l
simp (Negation (Disj l r)) = simp $ Conj (simp $ Negation l) (simp $ Negation r)
simp (Negation (Conj l r)) = simp $ Disj (simp $ Negation l) (simp $ Negation r)
simp (Conj l (Negation r)) | l == r = False
simp (Disj l (Negation r)) | l == r = True
simp (Negation p) = Negation (simp p)
simp (Conj l r) = Conj (simp l) (simp r)
simp (Disj l r) = Disj (simp l) (simp r)
simp o = o

-- Put this inside a proposition to have
-- two wildcard (self and target)
data BinaryWildcard = BSelf | BTarget | BRef Index

-- Put this inside a proposition to have
-- a single wildcard (target)
data UnaryWildcard = UTarget | URef Index

newtype Entry = Entry
  -- Expression which must be true for us to also be true.
  { requires :: Proposition Index

  -- We can add arbitrary logic to entries already in the graph
  , contributes :: List (Proposition BinaryWildcard)
  }

data MoveImplications
  = Empty
  | AddEntry Entry MoveImplications

referencesSelf :: Proposition UnaryWildcard -> Boolean
referencesSelf = any case _ of
    URef 0 -> true
    _ -> false

applyContributions :: List (Proposition UnaryWildcard) -> MoveImplications -> MoveImplications
applyContributions Nil Empty = Empty
applyContributions (Cons contribution rest) (AddEntry entry implications) = AddEntry entry' (applyContributions rest implications')
  where
  entry' = ?e
  implications' = ?i

  contributions = implications # List.partition referencesSelf

applyContributions _ _ = unsafeCrashWith "Different amounts of contributions and implications"


resolveMoveImplications :: MoveImplications -> List Boolean
resolveMoveImplications Empty = List.Nil
resolveMoveImplications (AddEntry (Entry { requires, contributes }) implications) =
  ?w
  where
  unaryContributions :: List (Proposition UnaryWildcard)
  unaryContributions = contributes <#> \proposition -> do
    wildcard <- proposition
    case wildcard of
      BTarget -> pure UTarget
      BRef i -> pure $ URef i
      BSelf -> map URef requires
