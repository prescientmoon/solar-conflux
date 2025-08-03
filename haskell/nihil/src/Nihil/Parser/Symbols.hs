module Nihil.Parser.Symbols
  ( arrow
  , fatArrow
  , lambda
  ) where

import Relude

import Nihil.Parser.Core qualified as Core
import Text.Megaparsec.Char qualified as MC

arrow ∷ Core.Parser Text
arrow = MC.string "->" <|> MC.string "→"

fatArrow ∷ Core.Parser Text
fatArrow = MC.string "=>" <|> MC.string "⇒"

lambda ∷ Core.Parser Text
lambda = MC.string "lam" <|> MC.string "λ"
