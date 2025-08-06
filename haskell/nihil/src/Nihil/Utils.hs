module Nihil.Utils where

import Relude

import Optics qualified as O
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP

textPretty ∷ ∀ a. (PP.Pretty a) ⇒ a → Text
textPretty = textPretty' . PP.pretty

textPretty' ∷ ∀ a. PP.Doc a → Text
textPretty' =
  PP.renderStrict
    . PP.layoutPretty
      ( PP.LayoutOptions
          { layoutPageWidth =
              PP.AvailablePerLine
                80
                1
          }
      )

adjoinMany ∷ ∀ s a. [O.Traversal' s a] → O.Traversal' s a
adjoinMany (h : t) = O.adjoin h (adjoinMany t)
adjoinMany [] = O.castOptic @O.A_Traversal $ O.noIx O.ignored

-- | Similar to @`modifying`, except handles the cases where the monadic action
-- changes the state mid-update gracefully. Only works with affine folds, though.
modifyingM
  ∷ ∀ m k is s a
   . ( O.Is k O.An_AffineFold
     , O.Is k O.A_Setter
     , MonadState s m
     )
  ⇒ O.Optic' k is s a
  → (a → m a)
  → m ()
modifyingM o f = do
  r ← gets $ O.preview o
  for_ r \r' → do
    r'' ← f r'
    O.assign o r''

chooseFirst ∷ ∀ a. [Maybe a] → Maybe a
chooseFirst [] = Nothing
chooseFirst (Nothing : rest) = chooseFirst rest
chooseFirst (Just r : _) = Just r

chooseFirstM ∷ ∀ m a. (Monad m) ⇒ [m (Maybe a)] → m (Maybe a)
chooseFirstM [] = pure Nothing
chooseFirstM (h : rest) =
  h >>= \case
    Nothing → chooseFirstM rest
    Just r → pure $ Just r

-- | Implicitness annotation
data Icit
  = Implicit
  | Auto
  | Explicit
  deriving (Eq, Ord, Generic, Show)
