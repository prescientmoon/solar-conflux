module Nihil.Ast.State
  ( Name (..)
  , Type' (..)
  , Expr (..)
  , Pattern (..)
  , AstState (..)
  , ScopeId (..)
  , NodeId (..)
  , FileData (..)
  , ExprDefinition (..)
  , TypeDefinition (..)
  , Scope (..)
  , genTest
  , nodeId
  , exprAst
  , typeAst
  , moduleAst
  , getSpan
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Sequence (Seq ((:<|), (:|>)), (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Traversable (for)
import Error.Diagnose qualified as DG
import Language.LSP.Protocol.Types qualified as LSP
import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Base qualified as Cst
import Nihil.Cst.Expr qualified as Cst.Expr
import Nihil.Cst.Module qualified as Cst.Mod
import Nihil.Cst.Type qualified as Cst.Type
import Nihil.Error (Report, Span)
import Nihil.Error qualified as Error
import Nihil.Parser.Core qualified as Parser
import Nihil.Parser.Module qualified as Parser
import Nihil.Utils (textPretty)
import Optics ((%))
import Optics qualified as O
import Prettyprinter qualified as PP
import Relude

-- {{{ Scopes
data Name
  = Unresolved Text
  | Resolved ScopeId Text
  deriving (Show, Generic)

type Binder = Name

-- | The parser often returns @Maybe Cst.Name@, which this function forcefully
-- turns into a piece of text.
forceMaybeName ∷ NodeId → Maybe Cst.Name → Text
forceMaybeName (NodeId i) Nothing = "__unnammed_" <> show i
forceMaybeName _ (Just t) = t.value

-- | Some names are already used, but we still want to keep track of the declarations.
--
-- Thus, we invent a new name if that's the case
forceMaybeUsed ∷ NodeId → Bool → Text → Text
forceMaybeUsed _ True x = x
forceMaybeUsed (NodeId i) False x = "__overload" <> show i <> x

newtype ScopeId = ScopeId Natural
  deriving (Generic, Show, Eq)
  deriving newtype (Hashable, Num, PP.Pretty)

data TypeDefinition
  = TForeign NodeId Type' -- Kind
  | TAlias NodeId Type'
  | TParam NodeId
  deriving (Show, Generic)

instance IsNode TypeDefinition where
  nodeId (TForeign i _) = i
  nodeId (TAlias i _) = i
  nodeId (TParam i) = i

data ExprDefinition
  = EForeign NodeId Type'
  | EDeclaration NodeId Type' Expr
  | EParam NodeId
  deriving (Show, Generic)

instance IsNode ExprDefinition where
  nodeId (EForeign i _) = i
  nodeId (EDeclaration i _ _) = i
  nodeId (EParam i) = i

data Scope = Scope
  { inherits ∷ Seq ScopeId
  , types ∷ HashMap Text TypeDefinition
  , exprs ∷ HashMap Text ExprDefinition
  , scopes ∷ HashMap Text ScopeId
  , reports ∷ Seq Report
  }
  deriving (Generic)

instance Semigroup Scope where
  a <> b =
    Scope
      { inherits = a.inherits <> b.inherits
      , types = a.types <> b.types
      , exprs = a.exprs <> b.exprs
      , scopes = a.scopes <> b.scopes
      , reports = a.reports <> b.reports
      }

instance Monoid Scope where
  mempty = Scope mempty mempty mempty mempty mempty

genScopeId ∷ ∀ m. (MonadState AstState m) ⇒ m ScopeId
genScopeId = do
  res ← O.use #nextScopeId
  O.modifying #nextScopeId (+ 1)
  O.assign (#scopes % O.at res) $ Just mempty

  curr ← O.use #currentFile
  O.modifying (#files % O.ix curr % #scopes) (|> res)

  pure res

scopeInherits ∷ ∀ m. (MonadState AstState m) ⇒ ScopeId → ScopeId → m ()
scopeInherits parent child = do
  O.modifying (#scopes % O.ix child % #inherits) $ (|> parent)

makeChildScope ∷ ∀ m. (MonadState AstState m) ⇒ ScopeId → m ScopeId
makeChildScope parent = do
  child ← genScopeId
  scopeInherits parent child
  pure child

atExprInScope ∷ Text → O.Lens' Scope (Maybe ExprDefinition)
atExprInScope name = #exprs % O.at name

atTypeInScope ∷ Text → O.Lens' Scope (Maybe TypeDefinition)
atTypeInScope name = #types % O.at name

-- }}}
-- {{{ AST gen state
newtype NodeId = NodeId Natural
  deriving (Generic, Show, Eq)
  deriving newtype (Hashable, Num, PP.Pretty)

class IsNode a where
  nodeId ∷ a → NodeId

instance IsNode NodeId where
  nodeId = id

data FileData = FileData
  { scopes ∷ Seq ScopeId
  }
  deriving (Generic)

instance Semigroup FileData where
  a <> b = FileData (a.scopes <> b.scopes)

instance Monoid FileData where
  mempty = FileData mempty

data AstState = AstState
  { spans ∷ HashMap NodeId Cst.Span
  , scopes ∷ HashMap ScopeId Scope
  , files ∷ HashMap Error.Path FileData
  , currentFile ∷ Error.Path
  -- ^ New scopes automatically get added here
  , nextNodeId ∷ NodeId
  , nextScopeId ∷ ScopeId
  }
  deriving (Generic)

genNodeId ∷ ∀ m. (MonadState AstState m) ⇒ m NodeId
genNodeId = do
  res ← O.use #nextNodeId
  O.modifying #nextNodeId (+ 1)
  pure res

keepNodeAt ∷ ∀ m. (MonadState AstState m) ⇒ NodeId → Cst.Span → m ()
keepNodeAt i s = O.modifying #spans (HashMap.insert i s)

makeCstNode ∷ ∀ m c. (MonadState AstState m, Cst.HasTrivia c) ⇒ c → m NodeId
makeCstNode c = do
  i ← genNodeId
  keepNodeAt i (Cst.spanOf c)
  pure i

getSpan ∷ ∀ m n. (MonadState AstState m, IsNode n) ⇒ n → m (Maybe Error.Span)
getSpan i = do
  s ← get
  pure $ O.preview (#spans % O.at (nodeId i) % O._Just) s

-- | Like @`getSpan`, but errors out on failure.
getSpanConfident ∷ ∀ m n. (MonadState AstState m, IsNode n, PP.Pretty n) ⇒ n → m Error.Span
getSpanConfident a =
  getSpan a <&> \case
    Just b → b
    Nothing → error $ "No span found for " <> textPretty a

-- }}}
-- {{{ Error handling

-- | Create a custom error. The offset is there for sorting purposes.
reportError
  ∷ ∀ m
   . (MonadState AstState m)
  ⇒ ScopeId
  → Text
  → Error.Doc
  → [(Error.Span, DG.Marker Error.Doc)]
  → [DG.Note Error.Doc]
  → m ()
reportError scope code desc markers hints = do
  O.modifying (#scopes % O.at scope % O._Just % #reports) $
    (|> DG.Err (Just . PP.pretty $ "Ast" <> code) desc markers hints)

-- }}}

-- AST generation
-- {{{ Types
data Type'
  = TyForall NodeId ScopeId Binder Type'
  | TyArrow NodeId Cst.Type.ArrowKind Type' Type'
  | TyVar NodeId Name
  | TyApp NodeId Type' Type'
  | TyLambda NodeId ScopeId Binder Type'
  | TyUnknown NodeId
  deriving (Generic, Show)

instance IsNode Type' where
  nodeId (TyForall i _ _ _) = i
  nodeId (TyArrow i _ _ _) = i
  nodeId (TyApp i _ _) = i
  nodeId (TyVar i _) = i
  nodeId (TyLambda i _ _ _) = i
  nodeId (TyUnknown i) = i

-- | The type of types
tyType ∷ ∀ m. (MonadState AstState m) ⇒ m Type'
tyType = do
  i ← genNodeId
  pure $ TyVar i $ Unresolved "Type"

typeMaybeAst ∷ ∀ m. (MonadState AstState m) ⇒ ScopeId → Span → Maybe Cst.Type.Type' → m Type'
typeMaybeAst scope _ (Just t) = typeAst scope t
typeMaybeAst _ s Nothing = do
  i ← genNodeId
  keepNodeAt i s
  pure $ TyUnknown i

keepType ∷ ∀ m cst. (MonadState AstState m, Cst.HasTrivia cst) ⇒ cst → Type' → m Type'
keepType c t = keepNodeAt (nodeId t) (Cst.spanOf c) $> t

typeAst ∷ ∀ m. (MonadState AstState m) ⇒ ScopeId → Cst.Type.Type' → m Type'
typeAst _ cst@(Cst.Type.TyVar (Cst.Type.Var{..})) = do
  i ← genNodeId
  keepType cst $ TyVar i $ Unresolved $ O.view #value name
typeAst scope cst@(Cst.Type.TyApp (Cst.Type.App{..})) = do
  i ← genNodeId
  f' ← typeAst scope f
  a' ← typeAst scope a
  keepType cst $ TyApp i f' a'
typeAst scope cst@(Cst.Type.TyArrow (Cst.Type.Arrow{..})) = do
  let s = Cst.spanOf cst
  i ← genNodeId
  from' ← typeMaybeAst scope s from
  to' ← typeMaybeAst scope s to
  keepType cst $ TyArrow i (O.view #value kind) from' to'
typeAst scope cst@(Cst.Type.TyForall (Cst.Type.Forall{..})) = do
  child ← makeChildScope scope
  inner ← typeMaybeAst child (Cst.spanOf cst) ty

  let go inside name = do
        let textName = O.view #value name

        pId ← genNodeId
        addToScope atTypeInScope child textName name $ TParam pId

        i ← genNodeId
        keepType cst $ TyForall i child (Unresolved textName) inside
  foldlM go inner (Seq.reverse names)
typeAst scope cst@(Cst.Type.TyParens (Cst.Delimited{inner})) = do
  typeMaybeAst scope (Cst.spanOf cst) inner

-- }}}
-- {{{ Expressions
data Expr
  = EVar NodeId Name
  | EApp NodeId Expr Expr
  | EMatch NodeId (Seq Expr) (Seq (Seq Pattern, Expr))
  | EScope NodeId ScopeId Expr
  | EUnknown NodeId
  deriving (Generic, Show)

instance IsNode Expr where
  nodeId (EVar i _) = i
  nodeId (EApp i _ _) = i
  nodeId (EMatch i _ _) = i
  nodeId (EScope i _ _) = i
  nodeId (EUnknown i) = i

data Pattern
  = PName NodeId Text
  | PProj NodeId Name (Seq Pattern) -- (Thing a b) → ...
  | PWildcard NodeId -- _ → ...
  deriving (Generic, Show)

instance IsNode Pattern where
  nodeId (PName i _) = i
  nodeId (PProj i _ _) = i
  nodeId (PWildcard i) = i

keepExpr ∷ ∀ m. (MonadState AstState m) ⇒ Cst.Expr.Expr → Expr → m Expr
keepExpr c e = keepNodeAt (nodeId e) (Cst.spanOf c) $> e

exprMaybeAst
  ∷ ∀ m
   . (MonadState AstState m)
  ⇒ ScopeId
  → Span
  → Maybe Cst.Expr.Expr
  → m Expr
exprMaybeAst scope _ (Just e) = exprAst scope e
exprMaybeAst _ s Nothing = do
  i ← genNodeId
  keepNodeAt i s
  pure $ EUnknown i

exprScopeBranch
  ∷ ∀ m
   . (MonadState AstState m)
  ⇒ ScopeId
  → Seq Pattern
  → Expr
  → m Expr
exprScopeBranch scope patterns e = do
  go patterns
  pure $ EScope (nodeId e) scope e
 where
  go Seq.Empty = pure ()
  go (PWildcard _ :<| rest) = go rest
  go (PProj _ _ args :<| rest) = go (args <> rest)
  go (PName i name :<| rest) = do
    s ← getSpanConfident i
    addToScope atExprInScope scope name s $ EParam i
    go rest

exprAst ∷ ∀ m. (MonadState AstState m) ⇒ ScopeId → Cst.Expr.Expr → m Expr
exprAst _ cst@(Cst.Expr.EVar (Cst.Expr.Var{..})) = do
  i ← genNodeId
  keepExpr cst $ EVar i $ Unresolved name.value
exprAst scope cst@(Cst.Expr.EApp (Cst.Expr.App{..})) = do
  i ← genNodeId
  f' ← exprAst scope f
  a' ← exprAst scope a
  keepExpr cst $ EApp i f' a'
exprAst scope cst@(Cst.Expr.ELambda (Cst.Expr.Lambda{..})) = do
  i ← genNodeId
  patterns' ← traverse exprPattern patterns

  child ← makeChildScope scope
  body' ← exprMaybeAst child (Cst.spanOf cst) body
  body'' ← exprScopeBranch child patterns' body'

  keepExpr cst $ EMatch i mempty $ pure (patterns', body'')
exprAst scope cst@(Cst.Expr.EMatch (Cst.Expr.Match{..})) = do
  i ← genNodeId
  exprs' ←
    traverse (exprAst scope)
      . Seq.fromList
      $ O.view (O.partsOf O.traversed) exprs
  branches' ← for branches \(Cst.Expr.MatchBranch{..}) → do
    patterns' ←
      traverse exprPattern . Seq.fromList $
        O.view (O.partsOf O.traversed) patterns

    child ← makeChildScope scope
    body' ← exprMaybeAst child (Cst.spanOf cst) body
    body'' ← exprScopeBranch child patterns' body'

    pure $ (patterns', body'')
  keepExpr cst $ EMatch i exprs' branches'
exprAst _ _ = error "Unimplemented"

exprPattern ∷ ∀ m. (MonadState AstState m) ⇒ Cst.Expr.Pattern → m Pattern
exprPattern cst@(Cst.Expr.PParens (Cst.Delimited{inner = Nothing})) = do
  i ← makeCstNode cst
  pure $ PWildcard i
exprPattern (Cst.Expr.PParens (Cst.Delimited{inner = Just p})) = exprPattern p
exprPattern cst@(Cst.Expr.PWildcard _) = do
  i ← makeCstNode cst
  pure $ PWildcard i
exprPattern cst@(Cst.Expr.PName name) = do
  i ← makeCstNode cst
  pure $ PName i name.value
exprPattern cst@(Cst.Expr.PProj (Cst.Expr.PatternProj{head = head', args})) = do
  i ← makeCstNode cst
  args' ← traverse exprPattern args
  let fallback = Unresolved $ "__no_name" <> show i
  pure $ PProj i (maybe fallback (Unresolved . O.view #value) head') args'

-- }}}
-- {{{ Modules
addToScope
  ∷ ∀ m cst a k is
   . ( MonadState AstState m
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
   . ( MonadState AstState m
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
        scope
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
  { span ∷ Maybe Span
  , types ∷ Seq (NodeId, Type')
  , branches ∷ Seq (NodeId, Seq Pattern, Expr)
  }
  deriving (Generic, Show)

moduleAst ∷ ∀ m. (MonadState AstState m) ⇒ Cst.Mod.Module → m ScopeId
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
                scope
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

    let branches = elem'.branches <&> \(_, patterns, body) → (patterns, body)
    addToScope atExprInScope scope name' s
      . EDeclaration i ty
      $ EMatch i mempty branches

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

            pId ← genNodeId
            addToScope atTypeInScope child textName name $ TParam pId

            i' ← genNodeId
            keepType cst $ TyLambda i' child (Unresolved textName) inside
      body'' ← foldlM addArg body (Seq.reverse cst.args)

      let textName = forceMaybeName i cst.name
      addToScope atTypeInScope scope textName cst $ TAlias i body''
      pure acc
    Cst.Mod.DeclForeignType cst → do
      i ← makeCstNode cst

      t ← tyType
      let textName = forceMaybeName i cst.name

      addToScope atTypeInScope scope textName cst $ TForeign i t
      pure acc
    Cst.Mod.DeclForeignValue cst → do
      i ← makeCstNode cst

      ty ← typeMaybeAst scope (Cst.spanOf cst) cst.ty
      let name' = forceMaybeName i cst.name

      addToScope atExprInScope scope name' cst $ EForeign i ty
      pure acc
    Cst.Mod.DeclValueTypeAnn cst → do
      i ← makeCstNode cst

      ty ← typeMaybeAst scope (Cst.spanOf cst) cst.ty
      let name' = cst.name.value

      pure
        . HashMap.update (Just . O.over #span ((<>) . Just $ Cst.spanOf cst)) name'
        . HashMap.update (Just . O.over #types (|> (i, ty))) name'
        $ if HashMap.member name' acc
          then acc
          else HashMap.insert name' (ModuleAccElem mempty mempty mempty) acc
    Cst.Mod.DeclValueEquation cst → do
      i ← makeCstNode cst

      patterns ← traverse exprPattern cst.args

      child ← makeChildScope scope
      body ← exprMaybeAst child (Cst.spanOf cst) cst.expr
      body' ← exprScopeBranch child patterns body

      let name' = cst.name.value

      pure
        . HashMap.update (Just . O.over #span ((<>) . Just $ Cst.spanOf cst)) name'
        . HashMap.update (Just . O.over #branches (|> (i, patterns, body'))) name'
        $ if HashMap.member name' acc
          then acc
          else HashMap.insert name' (ModuleAccElem mempty mempty mempty) acc
    _ → error "Unimplemented"

-- }}}

-- {{{ Name resolution
getScopedName
  ∷ ∀ m a k is
   . ( O.Is k O.An_AffineFold
     , MonadState AstState m
     )
  ⇒ (Text → O.Optic' k is Scope (Maybe a))
  → ScopeId
  → Name
  → m (Maybe (Name, a))
getScopedName at _ resolved@(Resolved scope name) = do
  let at' = O.castOptic @O.An_AffineFold $ at name
  v ← gets $ O.preview (#scopes % O.ix scope % at' % O._Just)
  pure $ (resolved,) <$> v
getScopedName at i (Unresolved name) = do
  let at' = O.castOptic @O.An_AffineFold $ at name
  mbScope ← gets $ O.preview (#scopes % O.ix i)
  case mbScope of
    Nothing → pure Nothing
    Just scope → case O.preview (at' % O._Just) scope of
      Just thing → pure $ Just (Resolved i name, thing)
      Nothing → go scope.inherits
       where
        go Seq.Empty = pure Nothing
        go (rest :|> last') = do
          attempt ← getScopedName at last' $ Unresolved name
          case attempt of
            Nothing → go rest
            Just res → pure $ Just res

resolveName
  ∷ ∀ m a k is node
   . ( O.Is k O.An_AffineFold
     , MonadState AstState m
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
        getSpan node >>= traverse \span' →
          reportError
            scope
            "MissingName"
            "Name not in scope."
            [(span', DG.This $ "Name `" <> PP.pretty name <> "` not in scope.")]
            []
        pure name

exprResolveNames ∷ ∀ m. (MonadState AstState m) ⇒ ScopeId → Expr → m Expr
exprResolveNames scope (EVar i n) =
  resolveName atExprInScope scope i n <&> EVar i
exprResolveNames scope (EApp i f a) = do
  f' ← exprResolveNames scope f
  a' ← exprResolveNames scope a
  pure $ EApp i f' a'
exprResolveNames scope (EMatch i exprs branches) = do
  exprs' ← traverse (exprResolveNames scope) exprs
  branches' ← for branches \(pats, e) → do
    e' ← exprResolveNames scope e
    pure (pats, e')
  pure $ EMatch i exprs' branches'
exprResolveNames _ (EScope i scope e) = do
  e' ← exprResolveNames scope e
  pure $ EScope i scope e'
exprResolveNames _ e@(EUnknown _) = pure e

typeResolveNames ∷ ∀ m. (MonadState AstState m) ⇒ ScopeId → Type' → m Type'
typeResolveNames scope (TyVar i n) =
  resolveName atTypeInScope scope i n <&> TyVar i
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

exprDefinitionResolveNames
  ∷ ∀ m
   . (MonadState AstState m)
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
   . (MonadState AstState m)
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

scopeResolveNames ∷ ∀ m. (MonadState AstState m) ⇒ ScopeId → m ()
scopeResolveNames i = do
  mbScope ← gets $ O.preview (#scopes % O.ix i)
  for_ mbScope \scope → do
    for_ (HashMap.toList scope.types) \(k, v) → do
      v' ← typeDefinitionResolveNames i v
      O.assign (#scopes % O.ix i % #types % O.ix k) v'
    for_ (HashMap.toList scope.exprs) \(k, v) → do
      v' ← exprDefinitionResolveNames i v
      O.assign (#scopes % O.ix i % #exprs % O.ix k) v'

resolveNames ∷ ∀ m. (MonadState AstState m) ⇒ m ()
resolveNames = do
  scopes ← gets $ O.view #scopes
  for_ (HashMap.keys scopes) \scope → do
    scopeResolveNames scope

-- }}}

-- Debug tooling
-- {{{ REPL testing
genTest ∷ Text → IO ()
genTest source = do
  parsed ← Parser.parseTest Parser.pModule source
  case parsed of
    Nothing → pure ()
    Just m → do
      let astState = execState (moduleAst m) initialState
      putTextLn "========= AST gen"
      putTextLn $ textPretty astState
      printErrors astState
      putTextLn "========= Name resolution"
      -- let astState' = execState resolveNames $ clearErrors astState
      let astState' = execState resolveNames astState
      putTextLn $ textPretty astState'
      printErrors astState'
 where
  printErrors astState =
    for_ astState.scopes \s → do
      Error.printDiagnostic $
        DG.addFile
          (Error.addReports $ toList s.reports)
          "<test>"
          (Text.unpack source)

  initialState =
    AstState
      { spans = mempty
      , scopes = mempty
      , nextNodeId = 0
      , nextScopeId = 0
      , currentFile = initialFile
      , files = HashMap.singleton initialFile mempty
      }

  initialFile = LSP.toNormalizedUri $ LSP.Uri "<test>"

clearErrors ∷ AstState → AstState
clearErrors = O.set (#scopes % O.traversed % #reports) mempty

-- }}}
-- {{{ Pretty instances
instance PP.Pretty Name where
  pretty (Unresolved t) = "[?]." <> PP.pretty t
  pretty (Resolved s t) = "[" <> PP.pretty s <> "]." <> PP.pretty t

instance PP.Pretty TypeDefinition where
  pretty (TForeign i ty) = PP.hsep ["[" <> PP.pretty i <> "]foreign", "::", PP.pretty ty]
  pretty (TAlias i ty) = PP.hsep ["[" <> PP.pretty i <> "]alias", PP.pretty ty]
  pretty (TParam i) = "[" <> PP.pretty i <> "]Param"

instance PP.Pretty Scope where
  pretty (Scope{..}) =
    Cst.prettyTree
      "Scope"
      $ catMaybes
        [ guard (not $ null inherits) $> Base.prettyTree "inherits" do
            PP.pretty <$> toList inherits
        , guard (not $ null types) $> Base.prettyTree "types" do
            (k, v) ← HashMap.toList types
            pure $ fold ["[", PP.pretty k, "]", PP.pretty v]
        , guard (not $ null exprs) $> Base.prettyTree "exprs" do
            (k, v) ← HashMap.toList exprs
            pure $ fold ["[", PP.pretty k, "]", PP.pretty v]
        ]

instance PP.Pretty AstState where
  pretty (AstState{..}) =
    Cst.prettyTree
      "AstState"
      $ catMaybes
        [ guard (not $ null scopes) $> Base.prettyTree "scopes" do
            (k, v) ← HashMap.toList scopes
            pure $ fold ["[", PP.pretty k, "]", PP.pretty v]
        ]

instance PP.Pretty ExprDefinition where
  pretty (EParam i) = "[" <> PP.pretty i <> "]Param"
  pretty (EForeign i ty) =
    PP.hsep
      [ "[" <> PP.pretty i <> "]Foreign"
      , "::"
      , PP.pretty ty
      ]
  pretty (EDeclaration i ty e) =
    Cst.prettyTree
      ("[" <> PP.pretty i <> "]Declaration")
      [ PP.pretty ty
      , PP.pretty e
      ]

instance PP.Pretty Type' where
  pretty (TyForall i scope name ty) =
    fold
      [ "["
      , PP.pretty i
      , "]∀"
      , PP.pretty name
      , ",["
      , PP.pretty scope
      , "] "
      , PP.pretty ty
      ]
  pretty (TyLambda i scope name ty) =
    fold
      [ "["
      , PP.pretty i
      , "]λ"
      , PP.pretty name
      , ",["
      , PP.pretty scope
      , "] "
      , PP.pretty ty
      ]
  pretty (TyArrow i kind from to) =
    fold
      [ "["
      , PP.pretty i
      , "]("
      , PP.pretty from
      , PP.pretty kind
      , PP.pretty to
      , ")"
      ]
  pretty (TyApp i f a) =
    fold
      [ "["
      , PP.pretty i
      , "]("
      , PP.pretty f
      , " "
      , PP.pretty a
      , ")"
      ]
  pretty (TyUnknown i) =
    fold
      [ "["
      , PP.pretty i
      , "]???"
      ]
  pretty (TyVar i name) =
    fold
      [ "["
      , PP.pretty i
      , "]"
      , PP.pretty name
      ]

instance PP.Pretty Expr where
  pretty (EUnknown i) = "[" <> PP.pretty i <> "]???"
  pretty (EVar i name) = "[" <> PP.pretty i <> "]" <> PP.pretty name
  pretty (EScope i scope expr) =
    Cst.prettyTree
      ("[" <> PP.pretty i <> "]Scoped[" <> PP.pretty scope <> "]")
      [PP.pretty expr]
  pretty (EMatch i exprs branches) =
    Cst.prettyTree
      ("[" <> PP.pretty i <> "]Match")
      $ fold
        [ guard (not $ null exprs) $> Base.prettyTree "exprs" do
            PP.pretty <$> toList exprs
        , guard (not $ null branches) *> do
            (patterns, expr) ← toList branches
            pure . Cst.prettyTree "Branch" . catMaybes $
              [ guard (not $ null patterns) $> Base.prettyTree "patterns" do
                  PP.pretty <$> toList patterns
              , pure $ PP.pretty expr
              ]
        ]
  pretty (EApp i f a) =
    Cst.prettyTree
      ("[" <> PP.pretty i <> "]App")
      [ PP.pretty f
      , PP.pretty a
      ]

instance PP.Pretty Pattern where
  pretty (PWildcard i) = "[" <> PP.pretty i <> "]_"
  pretty (PName i n) = "[" <> PP.pretty i <> "]" <> PP.pretty n
  pretty (PProj i n args)
    | null args = "[" <> PP.pretty i <> "]." <> PP.pretty n
    | otherwise =
        fold
          [ "(["
          , PP.pretty i
          , "]."
          , PP.pretty n
          , " "
          , fold $ intersperse " " $ PP.pretty <$> toList args
          , ")"
          ]

-- }}}
