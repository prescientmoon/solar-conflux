module Nihil.Parser.Expr (pPattern, pPatternAtom, pExpr) where

import Relude

import Error.Diagnose qualified as DG
import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Expr
  ( App (..)
  , Expr (..)
  , Lambda (..)
  , Match (..)
  , MatchBranch (..)
  , MatchKind (Coinductive, Inductive)
  , Pattern (..)
  , PatternProj (..)
  , Var (..)
  )
import Nihil.Parser.Combinators qualified as Core
import Nihil.Parser.Core qualified as Core
import Nihil.Parser.Symbols qualified as Symbols
import Optics qualified as O
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

-- {{{ Basic expressions
pExpr ∷ Core.Parser Expr
pExpr = pExprApp

pLambda ∷ Core.Parser Expr
pLambda = do
  -- TODO: errors
  lam ← Core.token Symbols.lambda

  (patterns, arrow) ←
    Core.manyTill
      (Core.label "pattern" pPatternAtom)
      (Core.label "arrow" $ Core.token Symbols.arrow)

  body ← Core.tryJunkTill $ Core.label "expression" pExpr

  pure $
    ELambda $
      Lambda
        { lam = lam
        , arrow = arrow
        , patterns = patterns
        , body = body
        }

pExprApp ∷ Core.Parser Expr
pExprApp = do
  base ← pExprAtom
  args ← Core.many' $ Core.tryJunkTill $ Core.label "Expression atom" pExprAtom
  pure $ foldl' (\f a → EApp $ App f a) base args

pExprAtom ∷ Core.Parser Expr
pExprAtom = pLambda <|> pMatch <|> pExprVar <|> pExprParens

pExprVar ∷ Core.Parser Expr
pExprVar = EVar . Var <$> Core.name

pExprParens ∷ Core.Parser Expr
pExprParens =
  EParens
    <$> Core.delimited
      (Core.string "(")
      (Core.string ")")
      (Core.label "Expr" pExpr)

-- }}}
-- {{{ Match blocks
pMatch ∷ Core.Parser Expr
pMatch = do
  -- TODO: errors
  kind ←
    Core.token . M.choice $
      [ Inductive <$ MC.string "case"
      , Coinductive <$ MC.string "intro"
      ]

  let closer = case O.view #value kind of
        Inductive → Core.string "of"
        Coinductive → Core.string "where"

  exprs ←
    Core.alsoStopOnPre closer $
      Core.separated
        True
        (Core.string ",")
        (Core.label "expression" pExpr)

  where' ← Core.tryJunkTill closer

  mbBlock ← Core.tryMkBlock
  branches ← case mbBlock of
    Nothing → pure mempty
    Just block →
      Core.many'
        . Core.blockLike block
        . Core.tryJunkTill
        . Core.label "branch"
        $ pBranch
        $ Base.spanOf (kind, exprs, where')

  pure $
    EMatch $
      Match
        { kind = kind
        , exprs = exprs
        , where' = where'
        , branches = branches
        }

pBranch ∷ Base.Span → Core.Parser MatchBranch
pBranch s = do
  let parentHint =
        ( s
        , DG.Where "While parsing this (co)inducitve match block."
        )

  start ← M.getSourcePos
  patterns ←
    Core.alsoStopOnPre (Core.label "arrow" Symbols.arrow) $
      Core.separated
        True
        (Core.string ",")
        (Core.label "pattern" pPattern)
  let hasPatterns = not . null . O.view #elements $ patterns
  when (not hasPatterns) do
    Core.reportError
      "NoBranchPatterns"
      ("No pattern list found for branch.")
      [
        ( Base.mkMegaparsecSpan' start
        , DG.This "I was expecting a list of patterns here."
        )
      , parentHint
      ]
      []

  arrow ← Core.tryJunkTill . Core.label "arrow" $ Core.token Symbols.arrow
  when (hasPatterns && isNothing arrow) do
    Core.reportError
      "NoBranchArrow"
      ("No arrow found for branch.")
      [
        ( Base.spanOf patterns
        , DG.This "I was expecting an arrow after these patterns."
        )
      , parentHint
      ]
      []

  body ← Core.tryJunkTill $ Core.label "expression" pExpr
  when (isJust arrow && isNothing body) do
    Core.reportError
      "NoBranchBody"
      ("No body found for branch.")
      [
        ( Base.spanOf arrow
        , DG.This "I was expecting an expression after this arrow."
        )
      , parentHint
      ]
      []

  pure $
    MatchBranch
      { start = start
      , patterns = patterns
      , arrow = arrow
      , body = body
      }

-- }}}
-- {{{ Patterns
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

-- }}}
