module Nihil.Parser.Module (pModule) where

import Relude

import Error.Diagnose qualified as DG
import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Module
import Nihil.Parser.Combinators qualified as Core
import Nihil.Parser.Core qualified as Core
import Nihil.Parser.Expr (pPatternAtom)
import Nihil.Parser.Notation qualified as Core
import Nihil.Parser.Type (pType)
import Optics qualified as O
import Prettyprinter qualified as PP
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

pModule ∷ Core.Parser Module
pModule = do
  pos ← M.getSourcePos
  mbModule ← Core.unindented $ M.optional $ snd $ Core.string "module"
  (name, exports, where') ← case mbModule of
    Nothing → do
      Core.reportError
        "NoModuleHeader"
        "No module header found."
        [(Base.mkMegaparsecSpan' pos, DG.This "This is where I thought the module header might be.")]
        [] -- TODO: suggest fix based on file name
      pure (Nothing, Nothing, Nothing)
    Just _ → Core.tighten Core.do
      name ← Core.step $ Core.label "module name" Core.name
      exports' ←
        Core.optStep
          $ Core.label "export list"
          $ Core.delimitedList
            (Core.string "(")
            (Core.string ")")
            (Core.string ",")
          $ Core.label "export name" Core.name
      where' ← Core.pre $ Core.step $ Core.string "where"
      Core.pure (name, exports', where')

  (decls, mbEof) ←
    Core.anyIndentation $
      Core.manyTill
        (Core.label "declaration" . Core.unindented $ pDecl)
        (Core.label "end of file" . Core.anyIndentation $ Core.token M.eof)

  eof ← case mbEof of
    Just e → pure e
    Nothing →
      Core.anyIndentation
        . Core.junkTill
        . Core.label "end of file"
        $ Core.token M.eof

  pure $
    Module
      { module' = mbModule
      , name = name
      , exports = exports
      , where' = where'
      , decls = decls
      , eof = eof
      }

pDecl ∷ Core.Parser Declaration
pDecl =
  M.choice
    [ pForeign
    , pTypeAlias
    , pIndLike
    , pValue
    , pValueEquation
    ]

-- | Helper that makes a parser stop when it hits "toplevel"-like things.
notToplevel ∷ ∀ a. Core.Parser a → Core.Parser a
notToplevel =
  Core.alsoStopOnPre (Core.string "foreign")
    . Core.alsoStopOnPre (Core.string "type")
    . Core.alsoStopOnPre (Core.string "struct")
    . Core.alsoStopOnPre (Core.string "inductive")
    . Core.alsoStopOnPre (Core.string "coinductive")
    . Core.alsoStopOnPre (Core.string "trait")
    . Core.alsoStopOnPre (Core.string "alias")

pValue ∷ Core.Parser Declaration
pValue = do
  -- This will reject this branch if there's no colon after the name. A smarter
  -- heuristic could be implemented in the future.
  (name, colon) ← M.try do
    n ← Core.name
    colon ← snd $ Core.string ":"
    pure (n, colon)

  notToplevel do
    ty ← Core.tryJunkTill $ Core.label "Type" pType
    pure . DeclValueTypeAnn $
      ValueTypeAnnotation
        { name = name
        , colon = Just colon
        , ty = ty
        }

pValueEquation ∷ Core.Parser Declaration
pValueEquation = do
  name ← Core.name

  notToplevel do
    (patterns, eq) ←
      Core.manyTill
        (Core.label "pattern" pPatternAtom)
        (Core.string "=")

    pure . DeclValueEquation $
      ValueEquation
        { name = name
        , args = patterns
        , eq = eq
        , expr = Nothing
        }

pForeign ∷ Core.Parser Declaration
pForeign = do
  foreign' ← snd $ Core.string "foreign"
  next ←
    Core.tryJunkTill
      . Core.label "name"
      . M.choice
      $ [ Left <$> snd (Core.string "type")
        , Right <$> Core.name
        ]

  notToplevel case next of
    Nothing →
      pure . DeclForeignValue $
        ForeignValue
          { foreign' = foreign'
          , name = Nothing
          , colon = Nothing
          , ty = Nothing
          }
    Just (Right name) → do
      colon ← Core.tryJunkTill $ Core.string ":"
      ty ← Core.tryJunkTill $ Core.label "Type" pType
      pure . DeclForeignValue $
        ForeignValue
          { foreign' = foreign'
          , name = Just name
          , colon = colon
          , ty = ty
          }
    Just (Left tTy) → do
      name ← Core.tryJunkTill $ Core.label "type name" Core.name
      args ← Core.many' $ Core.tryJunkTill $ Core.label "argument name" Core.name
      pure . DeclForeignType $
        ForeignType
          { foreign' = foreign'
          , ty = tTy
          , name = name
          , args = args
          }

pTypeAlias ∷ Core.Parser Declaration
pTypeAlias = do
  tTy ← snd $ Core.string "type"
  notToplevel do
    name ← Core.tryJunkTill $ Core.label "type name" Core.name
    (args, eq) ←
      Core.manyTill
        (Core.label "argument name" Core.name)
        (Core.string "=")
    body ← Core.tryJunkTill $ Core.label "type" pType

    pure . DeclTypeAlias $
      TypeAlias
        { name = name
        , ty = tTy
        , args = args
        , eq = eq
        , body = body
        }

pIndLike ∷ Core.Parser Declaration
pIndLike = do
  kind ← pKind
  (name, args, where') ← Core.tighten Core.do
    name ← Core.step $ Core.label "name" Core.name
    rest ←
      Core.step . Core.label "where" $
        Core.manyTill
          (Core.label "argument name" Core.name)
          (Core.string "where")
    Core.pure
      ( name
      , rest <&> fst & fromMaybe mempty
      , rest >>= snd
      )

  let res =
        IndLike
          { kind = kind
          , name = name
          , args = args
          , where' = where'
          , fields = mempty
          }

  mbBlock ← Core.tryMkBlock

  let fieldLabel = case O.view #value kind of
        Inductive → "constructor"
        Coinductive → "field"
        Trait → "method"

  fields ← case mbBlock of
    Nothing → pure mempty
    Just block →
      Core.many'
        . Core.blockLike block
        . Core.tryJunkTill
        $ Core.label fieldLabel pField

  let label = case O.view #value kind of
        Inductive → "inductive type" ∷ Text
        Coinductive → "coinductive type"
        Trait → "trait"

  when (null fields) do
    Core.reportError
      "NoIndLikeFields"
      (PP.hsep ["Empty", PP.pretty label, "declaration found."])
      [
        ( Base.spanOf res
        , DG.This $
            PP.hsep
              [ "I was expecting at least one"
              , PP.pretty fieldLabel
              , "associated with this"
              , PP.pretty label <> "."
              ]
        )
      ]
      []

  pure . DeclIndLike $ O.set #fields fields res
 where
  pKind = Core.token (trait <|> inductive <|> coinductive)
  trait = Trait <$ MC.string "trait"
  inductive = Inductive <$ MC.string "inductive"
  coinductive = Coinductive <$ (MC.string "coinductive" <|> MC.string "struct")

pField ∷ Core.Parser Field
pField = M.try do
  start ← M.getSourcePos
  name ← optional Core.name
  colon ← optional $ snd $ Core.string ":"
  ty ←
    pType
      & if isJust name || isJust colon
        then Core.tryJunkTill . Core.label "type"
        else optional

  when (isNothing name && isNothing colon && isNothing ty) do
    fail "no field ;-;"

  pure $
    Field
      { start = start
      , name = name
      , colon = colon
      , ty = ty
      }
