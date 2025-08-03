module Nihil.Parser.Type (pType) where

import Relude

import Data.Foldable1 (foldl1)
import Error.Diagnose qualified as DG
import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Type
  ( App (..)
  , Arrow (..)
  , ArrowKind (..)
  , Forall (..)
  , Type' (..)
  , Var (..)
  )
import Nihil.Parser.Combinators qualified as Core
import Nihil.Parser.Core qualified as Core
import Nihil.Parser.Symbols qualified as Symbols
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

pType ∷ Core.Parser Type'
pType = tyArr

tyArr ∷ Core.Parser Type'
tyArr = do
  mbBase ← optional . Core.alsoStopOnPost (Core.label "arrow" arrow) $ tyApp

  let parseKind =
        if isJust mbBase
          then Core.tryJunkTill . Core.label "arrow"
          else optional

  mbKind ← parseKind $ Core.token arrow

  case (mbBase, mbKind) of
    (Nothing, Nothing) → fail "no type found ;-;"
    (Just base, Nothing) → pure base
    (_, Just kind) → do
      when (isNothing mbBase) do
        Core.reportError
          "MissingArrowDomain"
          "Function arrow is missing domain."
          [(Base.spanOf kind, DG.This "I was expecting a type before this arrow.")]
          []

      to ←
        Core.alsoStopOnPost ("arrow", arrow)
          . Core.tryJunkTill
          $ Core.label "Type" pType

      when (isNothing to) do
        Core.reportError
          "MissingArrowCodomain"
          "Function arrow is missing codomain."
          [(Base.spanOf kind, DG.This "I was expecting a type after this arrow.")]
          []

      pure . TyArrow $
        Arrow
          { from = mbBase
          , kind = kind
          , to = to
          }
 where
  arrow = normalArrow <|> traitArrow
  normalArrow = Normal <$ Symbols.arrow
  traitArrow = Trait <$ Symbols.fatArrow

tyApp ∷ Core.Parser Type'
tyApp = do
  base ← tyAtom
  args ← Core.many' $ Core.tryJunkTill $ Core.label "Type atom" tyAtom
  pure $ foldl' (\f a → TyApp $ App f a) base args

tyAtom ∷ Core.Parser Type'
tyAtom =
  M.choice $
    [ TyForall <$> tyForall
    , TyVar <$> tyVar
    , TyParens <$> tyParens
    ]

tyForall ∷ Core.Parser Forall
tyForall = do
  let pForall = MC.string "forall" <|> MC.string "∀"
  tForall ← Core.token pForall

  (names, comma) ←
    Core.alsoStopOnPost (Core.string "(")
      . Core.alsoStopOnPre ("forall", pForall)
      $ Core.manyTill
        (Core.label "type variable" Core.name)
        (Core.string ",")

  when (null names) do
    Core.reportError
      "EmptyForall"
      "Universal quantifiers must bind at least one variable."
      ( reverse . catMaybes $
          [ Just (Base.spanOf tForall, DG.This "This one binds none.")
          , comma <&> \c →
              (Base.spanOf c, DG.Where "I got this far while looking for type variables.")
          ]
      )
      [DG.Hint "You should be able to simply remove this quantifier."]

  when (isNothing $ comma) do
    Core.reportError
      "NoForallComma"
      "Universal quantifiers must end their list of variables with a comma."
      ( reverse . catMaybes $
          [ Just (Base.spanOf tForall, DG.Where "I encountered the issue while parsing this universal quantifier.")
          , nonEmpty (toList names) <&> \neNames →
              ( foldl1 (<>) $ Base.spanOf <$> neNames
              , DG.This "This is the list of variable names I couldn't find a comma after."
              )
          ]
      )
      []

  ty ← Core.tryJunkTill $ Core.label "Type" pType

  when (isNothing ty) do
    Core.reportError
      "NoForallType"
      "Universal quantifiers must wrap a type."
      ( reverse . catMaybes $
          [ Just (Base.spanOf tForall, DG.This "I encountered the issue while parsing this universal quantifier.")
          , comma <&> \c →
              ( Base.spanOf c
              , DG.Where "I was expecting a type after this comma."
              )
          ]
      )
      []

  pure $
    Forall
      { tForall = tForall
      , names = names
      , comma = comma
      , ty = ty
      }

tyVar ∷ Core.Parser Var
tyVar = do
  name ← Core.name
  pure $ Var{name = name}

tyParens ∷ Core.Parser (Base.Delimited (Maybe Type'))
tyParens =
  Core.delimited
    (Core.string "(")
    (Core.string ")")
    (Core.label "Type" pType)
