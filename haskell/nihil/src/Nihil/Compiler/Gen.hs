-- | This module takes care of the CST -> AST transformation.
module Nihil.Compiler.Gen (moduleAst) where

import Data.HashMap.Strict qualified as HashMap
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Traversable (for)
import Error.Diagnose qualified as DG
import Nihil.Compiler.Ast
  ( Definition (Definition)
  , Expr (..)
  , ExprDefinition (EDeclaration, EForeign, EParam)
  , IsNode (nodeId)
  , Name (Unresolved)
  , NodeId
  , Pattern (..)
  , Scope
  , ScopeId
  , Type' (..)
  , TypeDefinition (TAlias, TForeign, TParam)
  , atExprInScope
  , atTypeInScope
  , forceMaybeName
  , forceMaybeUsed
  , nameToText
  )
import Nihil.Compiler.Monad
  ( CompilerState (..)
  , MonadCompile
  , genNodeId
  , genScopeId
  , getSpan
  , getSpanConfident
  , keepNodeAt
  , makeChildScope
  , makeCstNode
  , reportError
  )
import Nihil.Cst.Base qualified as Cst
import Nihil.Cst.Expr qualified as Cst.Expr
import Nihil.Cst.Module qualified as Cst.Mod
import Nihil.Cst.Type qualified as Cst.Type
import Nihil.Error qualified as Error
import Nihil.Utils qualified as Utils
import Optics ((%))
import Optics qualified as O
import Prettyprinter qualified as PP
import Relude

-- {{{ Types
typeMaybeAst
  ∷ ∀ m
   . (MonadCompile m)
  ⇒ ScopeId
  → Error.Span
  → Maybe Cst.Type.Type'
  → m Type'
typeMaybeAst scope _ (Just t) = typeAst scope t
typeMaybeAst _ s Nothing = do
  i ← genNodeId
  keepNodeAt i s
  pure $ TyUnknown i

keepType ∷ ∀ m cst. (MonadCompile m, Cst.HasTrivia cst) ⇒ cst → Type' → m Type'
keepType c t = keepNodeAt (nodeId t) (Cst.spanOf c) $> t

typeAst ∷ ∀ m. (MonadCompile m) ⇒ ScopeId → Cst.Type.Type' → m Type'
typeAst _ cst@(Cst.Type.TyVar (Cst.Type.Var{..})) = do
  i ← makeCstNode cst
  ni ← makeCstNode name
  pure $ TyVar i $ Unresolved ni $ O.view #value name
typeAst scope cst@(Cst.Type.TyApp (Cst.Type.App{..})) = do
  i ← makeCstNode cst
  f' ← typeAst scope f
  a' ← typeAst scope a
  pure $ TyApp i f' a'
typeAst scope cst@(Cst.Type.TyArrow (Cst.Type.Arrow{..})) = do
  let s = Cst.spanOf cst
  i ← makeCstNode cst
  from' ← typeMaybeAst scope s from
  to' ← typeMaybeAst scope s to
  pure $ TyArrow i (O.view #value kind) from' to'
typeAst scope cst@(Cst.Type.TyForall (Cst.Type.Forall{..})) = do
  child ← makeChildScope scope
  inner ← typeMaybeAst child (Cst.spanOf cst) ty

  let go inside name = do
        let textName = O.view #value name

        ni ← makeCstNode name
        addToScope atTypeInScope child textName name
          . Definition (pure ni)
          $ TParam ni

        i ← makeCstNode cst
        pure $ TyForall i child (Unresolved ni textName) inside
  foldlM go inner (Seq.reverse names)
typeAst scope cst@(Cst.Type.TyParens (Cst.Delimited{inner})) = do
  typeMaybeAst scope (Cst.spanOf cst) inner

-- }}}
-- {{{ Expressions
keepExpr ∷ ∀ m. (MonadCompile m) ⇒ Cst.Expr.Expr → Expr → m Expr
keepExpr c e = keepNodeAt (nodeId e) (Cst.spanOf c) $> e

exprMaybeAst
  ∷ ∀ m
   . (MonadCompile m)
  ⇒ ScopeId
  → Error.Span
  → Maybe Cst.Expr.Expr
  → m Expr
exprMaybeAst scope _ (Just e) = exprAst scope e
exprMaybeAst _ s Nothing = do
  i ← genNodeId
  keepNodeAt i s
  pure $ EUnknown i

-- | Adds the binders found in a list of patterns to a given scope.
genPatternBinders
  ∷ ∀ m
   . (MonadCompile m)
  ⇒ ScopeId
  → Seq Pattern
  → m ()
genPatternBinders scope patterns = go patterns
 where
  go Seq.Empty = pure ()
  go (PWildcard _ :<| rest) = go rest
  go (PProj _ _ args :<| rest) = go (args <> rest)
  go (PName i name :<| rest) = do
    s ← getSpanConfident i
    addToScope atExprInScope scope (nameToText name) s
      . Definition (pure i)
      $ EParam i
    go rest

exprAst ∷ ∀ m. (MonadCompile m) ⇒ ScopeId → Cst.Expr.Expr → m Expr
exprAst _ cst@(Cst.Expr.EVar (Cst.Expr.Var{..})) = do
  i ← makeCstNode cst
  ni ← makeCstNode name
  pure $ EVar i $ Unresolved ni name.value
exprAst scope cst@(Cst.Expr.EApp (Cst.Expr.App{..})) = do
  i ← genNodeId
  f' ← exprAst scope f
  a' ← exprAst scope a
  keepExpr cst $ EApp i f' a'
exprAst scope cst@(Cst.Expr.ELambda (Cst.Expr.Lambda{..})) = do
  i ← genNodeId
  patterns' ← traverse patternAst patterns

  child ← makeChildScope scope
  body' ← exprMaybeAst child (Cst.spanOf cst) body
  genPatternBinders child patterns'

  keepExpr cst $ EMatch i mempty $ pure (patterns', child, body')
exprAst scope cst@(Cst.Expr.EMatch (Cst.Expr.Match{..})) = do
  i ← genNodeId
  exprs' ←
    traverse (exprAst scope)
      . Seq.fromList
      $ O.view (O.partsOf O.traversed) exprs
  branches' ← for branches \(Cst.Expr.MatchBranch{..}) → do
    patterns' ←
      traverse patternAst . Seq.fromList $
        O.view (O.partsOf O.traversed) patterns

    child ← makeChildScope scope
    body' ← exprMaybeAst child (Cst.spanOf cst) body

    genPatternBinders child patterns'
    pure $ (patterns', child, body')
  keepExpr cst $ EMatch i exprs' branches'
exprAst scope cst@(Cst.Expr.EParens (Cst.Delimited{inner})) =
  exprMaybeAst
    scope
    (Cst.spanOf cst)
    inner
exprAst _ e = error $ "Unimplemented `exprAst` for " <> Utils.textPretty e

-- }}}
-- {{{ Patterns
patternAst ∷ ∀ m. (MonadCompile m) ⇒ Cst.Expr.Pattern → m Pattern
patternAst cst@(Cst.Expr.PParens (Cst.Delimited{inner = Nothing})) = do
  i ← makeCstNode cst
  pure $ PWildcard i
patternAst (Cst.Expr.PParens (Cst.Delimited{inner = Just p})) = patternAst p
patternAst cst@(Cst.Expr.PWildcard _) = do
  i ← makeCstNode cst
  pure $ PWildcard i
patternAst cst@(Cst.Expr.PName name) = do
  i ← makeCstNode cst
  pure $ PName i $ Unresolved i name.value
patternAst cst@(Cst.Expr.PProj (Cst.Expr.PatternProj{head = head', args})) = do
  i ← makeCstNode cst
  args' ← traverse patternAst args
  let fallback = forceMaybeName i head'
  ni ← case head' of
    Just h → makeCstNode h
    Nothing → pure i -- Does this ever come up?
  pure $ PProj i (Unresolved ni fallback) args'

-- }}}
-- {{{ Modules
addToScope
  ∷ ∀ m cst a k is
   . ( MonadCompile m
     , IsNode a
     , Cst.HasTrivia cst
     , O.Is k O.A_Lens
     )
  ⇒ (Text → O.Optic' k is Scope (Maybe a))
  → ScopeId
  → Text
  → cst
  → a
  → m ()
addToScope at scope name cst val = do
  let at' = (O.castOptic @O.A_Lens . at)
  unused ← ensureUnusedName at' scope name cst
  let name' = forceMaybeUsed (nodeId val) unused name
  O.assign (#scopes % O.ix scope % at' name') $ Just val

ensureUnusedName
  ∷ ∀ m cst a k is
   . ( MonadCompile m
     , IsNode a
     , Cst.HasTrivia cst
     , O.Is k O.An_AffineFold
     )
  ⇒ (Text → O.Optic' k is Scope (Maybe a))
  → ScopeId
  → Text
  → cst
  → m Bool
ensureUnusedName at scope name cst = do
  let at' = O.castOptic @O.An_AffineFold $ at name
  mbExisting ← gets $ O.preview (#scopes % O.ix scope % at' % O._Just)
  case mbExisting of
    Nothing → pure True
    Just existing → do
      existingSpan ← getSpan existing
      reportError
        "DuplicateDeclaration"
        "Duplicate declaration."
        ( catMaybes
            [ Just
                ( Cst.spanOf cst
                , DG.This $
                    fold
                      [ "Cannot declare `"
                      , PP.pretty name
                      , "`, as the name is already taken."
                      ]
                )
            , existingSpan
                <&> (,DG.Where "The name is already defined here.")
            ]
        )
        []
      pure False

-- Temporary types used for the implementation of `moduleAst`.
type ModuleAcc = HashMap Text ModuleAccElem
data ModuleAccElem = ModuleAccElem
  { span ∷ Maybe Error.Span
  , names ∷ Seq NodeId
  , types ∷ Seq (NodeId, Type')
  , branches ∷ Seq (Seq Pattern, ScopeId, Expr)
  }
  deriving (Generic, Show)

instance Semigroup ModuleAccElem where
  a <> b =
    ModuleAccElem
      (a.span <> b.span)
      (a.names <> b.names)
      (a.types <> b.types)
      (a.branches <> b.branches)

instance Monoid ModuleAccElem where
  mempty = ModuleAccElem mempty mempty mempty mempty

moduleAst ∷ ∀ m. (MonadCompile m) ⇒ Cst.Mod.Module → m ScopeId
moduleAst (Cst.Mod.Module{..}) = do
  scope ← genScopeId
  acc ← foldlM (go scope) mempty decls
  for_ (HashMap.toList acc) \(name', elem') → do
    ty ∷ Type' ← case toList elem'.types of
      [] → do
        i ← genNodeId
        pure $ TyUnknown i
      [(_, ty)] → pure ty
      (idF, f) : more → do
        getSpan idF >>= traverse_ \firstSpan →
          for_ more \(idOther, _) →
            getSpan idOther >>= traverse_ \secondSpan →
              reportError
                "DuplicateTypeAnnotation"
                ("Multiple type annotations found for `" <> PP.pretty name' <> "`.")
                [ (firstSpan, DG.Where $ "Name `" <> PP.pretty name' <> "` is already defined here.")
                , (secondSpan, DG.This $ "Cannot re-declare value `" <> PP.pretty name' <> "`.")
                ]
                []
        pure f

    let s = fromMaybe (error "impossible") elem'.span
    i ← genNodeId
    keepNodeAt i s

    let nameNodes =
          fromMaybe (error "impossible")
            . nonEmpty
            $ toList elem'.names

    addToScope atExprInScope scope name' s
      . Definition nameNodes
      . EDeclaration i ty
      $ EMatch i mempty elem'.branches

  pure scope
 where
  go ∷ ScopeId → ModuleAcc → Cst.Mod.Declaration → m ModuleAcc
  go scope acc = \case
    Cst.Mod.DeclTypeAlias cst → do
      i ← makeCstNode cst

      child ← makeChildScope scope
      body ← typeMaybeAst child (Cst.spanOf cst) cst.body

      let addArg inside name' = do
            let textName = O.view #value name'

            ni ← makeCstNode name'
            addToScope atTypeInScope child textName name
              . Definition (pure ni)
              $ TParam ni

            keepType cst $ TyLambda i child (Unresolved ni textName) inside
      body'' ← foldlM addArg body (Seq.reverse cst.args)

      let textName = forceMaybeName i cst.name
      ni ← maybe (pure i) makeCstNode cst.name
      addToScope atTypeInScope scope textName cst
        . Definition (pure ni)
        $ TAlias i body''
      pure acc
    Cst.Mod.DeclForeignType cst → do
      i ← makeCstNode cst

      ty ← typeMaybeAst scope (Cst.spanOf cst) cst.ty
      let textName = forceMaybeName i cst.name

      ni ← maybe (pure i) makeCstNode cst.name
      addToScope atTypeInScope scope textName cst
        . Definition (pure ni)
        $ TForeign i ty
      pure acc
    Cst.Mod.DeclForeignValue cst → do
      i ← makeCstNode cst

      ty ← typeMaybeAst scope (Cst.spanOf cst) cst.ty
      let name' = forceMaybeName i cst.name

      ni ← maybe (pure i) makeCstNode cst.name
      addToScope atExprInScope scope name' cst
        . Definition (pure ni)
        $ EForeign i ty
      pure acc
    Cst.Mod.DeclValueTypeAnn cst → do
      i ← makeCstNode cst
      ty ← typeMaybeAst scope (Cst.spanOf cst) cst.ty

      let name' = cst.name.value
      ni ← makeCstNode cst.name
      let piece =
            ModuleAccElem
              { span = Just $ Cst.spanOf cst
              , names = pure ni
              , types = pure (i, ty)
              , branches = mempty
              }

      pure
        . HashMap.update (Just . (<> piece)) name'
        $ if HashMap.member name' acc
          then acc
          else HashMap.insert name' mempty acc
    Cst.Mod.DeclValueEquation cst → do
      patterns ← traverse patternAst cst.args

      child ← makeChildScope scope
      body ← exprMaybeAst child (Cst.spanOf cst) cst.expr
      genPatternBinders child patterns

      let name' = cst.name.value
      ni ← makeCstNode cst.name
      let piece =
            ModuleAccElem
              { span = Just $ Cst.spanOf cst
              , names = pure ni
              , types = mempty
              , branches = pure (patterns, child, body)
              }

      pure
        . HashMap.update (Just . (<> piece)) name'
        $ if HashMap.member name' acc
          then acc
          else HashMap.insert name' mempty acc
    _ → error "Unimplemented"

-- }}}
