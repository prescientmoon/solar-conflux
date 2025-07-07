module Nihil.Parser.Expr (pPattern) where

import Relude

import Data.Foldable1 (foldl1)
import Error.Diagnose qualified as DG
import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Expr
  ( Pattern (..)
  , PatternProj (..)
  )
import Nihil.Error qualified as Error
import Nihil.Parser.Combinators qualified as Core
import Nihil.Parser.Core qualified as Core
import Optics qualified as O
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

pPattern ∷ Core.Parser Pattern
pPattern = pPatProjApp <|> pPatternAtom

pPatternAtom ∷ Core.Parser Pattern
pPatternAtom =
  M.choice
    [ pPatParens
    , pPatWildcard
    , PProj <$> pPatProj
    , pPatName
    ]

pPatWildcard ∷ Core.Parser Pattern
pPatWildcard = fmap PWildcard $ snd $ Core.string "_"

pPatParens ∷ Core.Parser Pattern
pPatParens =
  PParens
    <$> Core.delimited
      (Core.string "(")
      (Core.string ")")
      (Core.label "Pattern" pPattern)

pPatName ∷ Core.Parser Pattern
pPatName = fmap PName $ Core.name

pPatProj ∷ Core.Parser PatternProj
pPatProj = do
  dot ← snd $ Core.string "."
  h ←
    Core.alsoStopOnPre (Core.string ".")
      . Core.alsoStopOnPre (Core.string "_")
      . Core.alsoStopOnPre (Core.string "(")
      . Core.tryJunkTill
      . Core.label "Pattern head"
      $ Core.name

  pure $
    PatternProj
      { dot = dot
      , head = h
      , args = mempty
      }

pPatProjApp ∷ Core.Parser Pattern
pPatProjApp = do
  proj ← pPatProj
  args ←
    Core.many' . Core.tryJunkTill $
      Core.label "Pattern atom" pPatternAtom

  pure $ PProj $ O.set #args args proj
