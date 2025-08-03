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
