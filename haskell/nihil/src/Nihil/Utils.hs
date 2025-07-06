module Nihil.Utils where

import Relude

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
