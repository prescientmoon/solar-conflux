-- | This module resolves all the names inside the AST structure.
module Nihil.Compiler.Resolve
  ( resolveNames
  , getScopedName
  , getUnscopedName
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Seq
import Data.Traversable (for)
import Error.Diagnose qualified as DG
import Nihil.Compiler.Ast
  ( Definition (..)
  , Expr (..)
  , ExprDefinition (..)
  , IsNode
  , Name (..)
  , Pattern (..)
  , Scope (..)
  , ScopeId
  , Type' (..)
  , TypeDefinition (..)
  , atExprInScope
  , atTypeInScope
  , nameToText
  )
import Nihil.Compiler.Monad (CompilerState (..), MonadCompile, getSpan, reportError)
import Optics ((%))
import Optics qualified as O
import Prettyprinter qualified as PP
import Relude

-- {{{ Name resolution
getUnscopedName
  ∷ ∀ m a k is
   . ( O.Is k O.An_AffineFold
     , MonadCompile m
     )
  ⇒ (Text → O.Optic' k is Scope (Maybe a))
  → Name
  → m (Maybe a)
getUnscopedName at resolved@(Resolved _ scope _) = do
  res ← getScopedName at scope resolved
  pure $ snd <$> res
getUnscopedName _ _ = pure Nothing

getScopedName
  ∷ ∀ m a k is
   . ( O.Is k O.An_AffineFold
     , MonadCompile m
     )
  ⇒ (Text → O.Optic' k is Scope (Maybe a))
  → ScopeId
  → Name
  → m (Maybe (Name, a))
getScopedName at _ resolved@(Resolved _ scope name) = do
  let at' = O.castOptic @O.An_AffineFold $ at name
  v ← gets $ O.preview (#scopes % O.ix scope % at' % O._Just)
  pure $ (resolved,) <$> v
getScopedName at si (Unresolved ni name) = do
  let at' = O.castOptic @O.An_AffineFold $ at name
  mbScope ← gets $ O.preview (#scopes % O.ix si)
  case mbScope of
    Nothing → pure Nothing
    Just scope → case O.preview (at' % O._Just) scope of
      Just thing → pure $ Just (Resolved ni si name, thing)
      Nothing → go scope.inherits
       where
        go Seq.Empty = pure Nothing
        go (rest :|> last') = do
          attempt ← getScopedName at last' $ Unresolved ni name
          case attempt of
            Nothing → go rest
            Just res → pure $ Just res

-- }}}

-- Resolve the names inside a...
-- {{{ Variable
resolveName
  ∷ ∀ m a k is node
   . ( O.Is k O.An_AffineFold
     , MonadCompile m
     , IsNode node
     )
  ⇒ (Text → O.Optic' k is Scope (Maybe a))
  → ScopeId
  → node
  → Name
  → m Name
resolveName at scope node name =
  getScopedName at scope name
    >>= \case
      Just (n', _) → pure n'
      Nothing → do
        let name' = nameToText name
        getSpan node >>= traverse \span' →
          reportError
            "MissingName"
            "Name not in scope."
            [(span', DG.This $ "Name `" <> PP.pretty name' <> "` not in scope.")]
            []
        pure name

-- }}}
-- {{{ Expression
exprResolveNames ∷ ∀ m. (MonadCompile m) ⇒ ScopeId → Expr → m Expr
exprResolveNames scope (EVar i n) =
  resolveName atExprInScope scope i n <&> \case
    r@(Resolved _ _ _) → EVar i r
    (Unresolved i' _) → EUnknown i'
exprResolveNames scope (EApp i f a) = do
  f' ← exprResolveNames scope f
  a' ← exprResolveNames scope a
  pure $ EApp i f' a'
exprResolveNames scope (EMatch i exprs branches) = do
  exprs' ← traverse (exprResolveNames scope) exprs
  branches' ← for branches \(pats, scope', e) → do
    pats' ← traverse (patternResolveNames scope') pats
    e' ← exprResolveNames scope' e
    pure (pats', scope', e')
  pure $ EMatch i exprs' branches'
exprResolveNames _ e@(EUnknown _) = pure e
exprResolveNames _ _ = error "Unimplemented"

-- }}}
-- {{{ Patterns
patternResolveNames ∷ ∀ m. (MonadCompile m) ⇒ ScopeId → Pattern → m Pattern
patternResolveNames _ w@(PWildcard _) = pure w
patternResolveNames scope (PName i name) =
  resolveName atExprInScope scope i name <&> PName i
patternResolveNames scope (PProj i name args) =
  -- NOTE: we delay the resolution of constructor names
  PProj i name
    <$> traverse (patternResolveNames scope) args

-- }}}
-- {{{ Types
typeResolveNames ∷ ∀ m. (MonadCompile m) ⇒ ScopeId → Type' → m Type'
typeResolveNames scope (TyVar i n) =
  resolveName atTypeInScope scope i n <&> \case
    r@(Resolved _ _ _) → TyVar i r
    (Unresolved i' _) → TyUnknown i'
typeResolveNames scope (TyApp i f a) = do
  f' ← typeResolveNames scope f
  a' ← typeResolveNames scope a
  pure $ TyApp i f' a'
typeResolveNames scope (TyArrow i kind from to) = do
  from' ← typeResolveNames scope from
  to' ← typeResolveNames scope to
  pure $ TyArrow i kind from' to'
typeResolveNames _ (TyForall i scope name ty) = do
  name' ← resolveName atTypeInScope scope i name
  ty' ← typeResolveNames scope ty
  pure $ TyForall i scope name' ty'
typeResolveNames _ (TyLambda i scope name ty) = do
  name' ← resolveName atTypeInScope scope i name
  ty' ← typeResolveNames scope ty
  pure $ TyLambda i scope name' ty'
typeResolveNames _ t@(TyUnknown _) = pure t

-- }}}
-- {{{ Scopes
exprDefinitionResolveNames
  ∷ ∀ m
   . (MonadCompile m)
  ⇒ ScopeId
  → ExprDefinition
  → m ExprDefinition
exprDefinitionResolveNames scope (EForeign i ty) = do
  ty' ← typeResolveNames scope ty
  pure $ EForeign i ty'
exprDefinitionResolveNames _ e@(EParam _) = pure e
exprDefinitionResolveNames scope (EDeclaration i ty e) = do
  ty' ← typeResolveNames scope ty
  e' ← exprResolveNames scope e
  pure $ EDeclaration i ty' e'

typeDefinitionResolveNames
  ∷ ∀ m
   . (MonadCompile m)
  ⇒ ScopeId
  → TypeDefinition
  → m TypeDefinition
typeDefinitionResolveNames scope (TForeign i ty) = do
  ty' ← typeResolveNames scope ty
  pure $ TForeign i ty'
typeDefinitionResolveNames _ e@(TParam _) = pure e
typeDefinitionResolveNames scope (TAlias i ty) = do
  ty' ← typeResolveNames scope ty
  pure $ TAlias i ty'

scopeResolveNames ∷ ∀ m. (MonadCompile m) ⇒ ScopeId → m ()
scopeResolveNames i = do
  mbScope ← gets $ O.preview (#scopes % O.ix i)
  for_ mbScope \scope → do
    for_ (HashMap.toList scope.types) \(k, (Definition ni v)) → do
      v' ← typeDefinitionResolveNames i v
      O.assign (#scopes % O.ix i % #types % O.ix k) $ Definition ni v'
    for_ (HashMap.toList scope.exprs) \(k, (Definition ni v)) → do
      v' ← exprDefinitionResolveNames i v
      O.assign (#scopes % O.ix i % #exprs % O.ix k) $ Definition ni v'

resolveNames ∷ ∀ m. (MonadCompile m) ⇒ m ()
resolveNames = do
  scopes ← gets $ O.view #scopes
  for_ (HashMap.keys scopes) \scope → do
    scopeResolveNames scope

-- }}}
