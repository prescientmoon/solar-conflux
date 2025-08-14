module Nihil.Ast.State
  ( Name (..)
  , Binder
  , Type' (..)
  , Expr (..)
  , Pattern (..)
  , CompilerState (..)
  , ScopeId (..)
  , NodeId (..)
  , FileData (..)
  , Definition (..)
  , ExprDefinition (..)
  , TypeDefinition (..)
  , Scope (..)
  , MonadCompile
  , IsNode
  , genTest
  , nodeId
  , exprAst
  , typeAst
  , moduleAst
  , getSpan
  , resolveNames
  , initialCompilerState
  , getScopedName
  , getUnscopedName
  , atTypeInScope
  , atExprInScope
  , collectDiagnostics
  , collectReports
  , genNodeId
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
import Nihil.Cst.Module qualified as Cst
import Nihil.Cst.Module qualified as Cst.Mod
import Nihil.Cst.Type qualified as Cst.Type
import Nihil.Error (Report, Span)
import Nihil.Error qualified as Error
import Nihil.Parser.Core qualified as Parser
import Nihil.Parser.Module qualified as Parser
import Nihil.Utils (Icit, textPretty)
import Optics ((%))
import Optics qualified as O
import Prettyprinter qualified as PP
import Relude

-- {{{ Names & binders
data Name
  = Unresolved NodeId Text
  | Resolved NodeId ScopeId Text
  deriving (Show, Generic, Eq, Hashable)

type Binder = Name

nameToText ∷ Name → Text
nameToText = \case
  Unresolved _ n → n
  Resolved _ s n → "[:" <> show s <> "]." <> n

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

-- }}}
-- {{{ Scopes
newtype ScopeId = ScopeId Natural
  deriving (Generic, Show, Eq)
  deriving newtype (Hashable, Num, PP.Pretty)

data TypeDefinition
  = TForeign NodeId Type' -- Kind
  | TAlias NodeId Type'
  | TParam NodeId
  deriving (Show, Generic)

data ExprDefinition
  = EForeign NodeId Type'
  | EDeclaration NodeId Type' Expr
  | EParam NodeId
  deriving (Show, Generic)

data Definition a
  = Definition
  { names ∷ NonEmpty NodeId
  -- ^ Can be used to get the span of the name at the definition site.
  , value ∷ a
  }
  deriving (Show, Generic)

data Scope = Scope
  { inherits ∷ Seq ScopeId
  , types ∷ HashMap Text (Definition TypeDefinition)
  , exprs ∷ HashMap Text (Definition ExprDefinition)
  , scopes ∷ HashMap Text ScopeId
  }
  deriving (Generic)

instance Semigroup Scope where
  a <> b =
    Scope
      { inherits = a.inherits <> b.inherits
      , types = a.types <> b.types
      , exprs = a.exprs <> b.exprs
      , scopes = a.scopes <> b.scopes
      }

instance Monoid Scope where
  mempty = Scope mempty mempty mempty mempty

genScopeId ∷ ∀ m. (MonadState CompilerState m) ⇒ m ScopeId
genScopeId = do
  res ← O.use #nextScopeId
  O.modifying #nextScopeId (+ 1)
  O.assign (#scopes % O.at res) $ Just mempty

  pure res

scopeInherits ∷ ∀ m. (MonadState CompilerState m) ⇒ ScopeId → ScopeId → m ()
scopeInherits parent child = do
  O.modifying (#scopes % O.ix child % #inherits) $ (|> parent)

makeChildScope ∷ ∀ m. (MonadState CompilerState m) ⇒ ScopeId → m ScopeId
makeChildScope parent = do
  child ← genScopeId
  scopeInherits parent child
  pure child

atExprInScope ∷ Text → O.Lens' Scope (Maybe (Definition ExprDefinition))
atExprInScope name = #exprs % O.at name

atTypeInScope ∷ Text → O.Lens' Scope (Maybe (Definition TypeDefinition))
atTypeInScope name = #types % O.at name

-- }}}
-- {{{ Nodes
newtype NodeId = NodeId Natural
  deriving (Generic, Show, Eq)
  deriving newtype (Hashable, Num, PP.Pretty)

class IsNode a where
  nodeId ∷ a → NodeId

instance IsNode NodeId where
  nodeId = id

instance IsNode Name where
  nodeId (Unresolved i _) = i
  nodeId (Resolved i _ _) = i

instance IsNode Type' where
  nodeId (TyForall i _ _ _) = i
  nodeId (TyArrow i _ _ _) = i
  nodeId (TyApp i _ _) = i
  nodeId (TyVar i _) = i
  nodeId (TyLambda i _ _ _) = i
  nodeId (TyUnknown i) = i

instance IsNode Expr where
  nodeId (EVar i _) = i
  nodeId (EApp i _ _) = i
  nodeId (EMatch i _ _) = i
  nodeId (EUnknown i) = i
  nodeId (EPi i _ _ _ _ _) = i
  nodeId (Eannotation i _ _) = i
  nodeId (EHole i) = i

instance IsNode Pattern where
  nodeId (PName i _) = i
  nodeId (PProj i _ _) = i
  nodeId (PWildcard i) = i

instance IsNode TypeDefinition where
  nodeId (TForeign i _) = i
  nodeId (TAlias i _) = i
  nodeId (TParam i) = i

instance IsNode ExprDefinition where
  nodeId (EForeign i _) = i
  nodeId (EDeclaration i _ _) = i
  nodeId (EParam i) = i

instance IsNode (Definition a) where
  nodeId (Definition i _) = head i

-- }}}
-- {{{ AST gen state
data FileData = FileData
  { source ∷ Maybe Text
  , parsed ∷ Maybe Cst.Module
  , parsingReports ∷ Seq Report
  -- ^ These get invalidated when the file is reparsed.
  , mainScope ∷ Maybe ScopeId
  }
  deriving (Generic)

instance Semigroup FileData where
  a <> b =
    FileData
      { source = a.source <|> b.source
      , parsed = a.parsed <|> b.parsed
      , parsingReports = a.parsingReports <> b.parsingReports
      , mainScope = a.mainScope <|> b.mainScope
      }

instance Monoid FileData where
  mempty = FileData Nothing Nothing mempty Nothing

data CompilerState = CompilerState
  { spans ∷ HashMap NodeId Cst.Span
  , scopes ∷ HashMap ScopeId Scope
  , files ∷ HashMap Error.Path FileData
  -- ^ New scopes automatically get added here
  , reports ∷ Seq Report
  -- ^ These get invalidated whenever *any* file changes.
  -- This is not optimal, but it makes reasoning about changes
  -- much easier.
  , nextNodeId ∷ NodeId
  , nextScopeId ∷ ScopeId
  }
  deriving (Generic)

initialCompilerState ∷ CompilerState
initialCompilerState =
  CompilerState
    { spans = mempty
    , scopes = mempty
    , nextNodeId = 0
    , nextScopeId = 0
    , files = mempty
    , reports = mempty
    }

type MonadCompile m = (MonadState CompilerState m)

genNodeId ∷ ∀ m. (MonadState CompilerState m) ⇒ m NodeId
genNodeId = do
  res ← O.use #nextNodeId
  O.modifying #nextNodeId (+ 1)
  pure res

keepNodeAt ∷ ∀ m. (MonadState CompilerState m) ⇒ NodeId → Cst.Span → m ()
keepNodeAt i s = O.modifying #spans (HashMap.insert i s)

makeCstNode ∷ ∀ m c. (MonadState CompilerState m, Cst.HasTrivia c) ⇒ c → m NodeId
makeCstNode c = do
  i ← genNodeId
  keepNodeAt i (Cst.spanOf c)
  pure i

getSpan ∷ ∀ m n. (MonadState CompilerState m, IsNode n) ⇒ n → m (Maybe Error.Span)
getSpan i = do
  s ← get
  pure $ O.preview (#spans % O.at (nodeId i) % O._Just) s

-- | Like @`getSpan`, but errors out on failure.
getSpanConfident ∷ ∀ m n. (MonadState CompilerState m, IsNode n, PP.Pretty n) ⇒ n → m Error.Span
getSpanConfident a =
  getSpan a <&> \case
    Just b → b
    Nothing → error $ "No span found for " <> textPretty a

-- }}}
-- {{{ Error handling

-- | Create a custom error. The offset is there for sorting purposes.
reportError
  ∷ ∀ m
   . (MonadState CompilerState m)
  ⇒ Text
  → Error.Doc
  → [(Error.Span, DG.Marker Error.Doc)]
  → [DG.Note Error.Doc]
  → m ()
reportError code desc markers hints = do
  O.modifying #reports $
    (|> DG.Err (Just . PP.pretty $ "Ast" <> code) desc markers hints)

collectReports ∷ CompilerState → Seq Error.Report
collectReports st =
  fold $
    [ st.reports
    , O.foldOf (#files % O.folded % #parsingReports) st
    ]

collectDiagnostics ∷ CompilerState → Error.Diagnostics
collectDiagnostics st = do
  let reports = Error.addReports . toList $ collectReports st

  foldl'
    ( \d (k, file) → case file.source of
        Nothing → d
        Just source →
          DG.addFile
            d
            (Error.pathToString k)
            (Text.unpack source)
    )
    reports
    (HashMap.toList st.files)

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

-- | The type of types
-- tyType ∷ ∀ m. (MonadState CompilerState m) ⇒ m Type'
-- tyType = do
--   i ← genNodeId
--   pure $ TyVar i $ Unresolved "Type"
typeMaybeAst ∷ ∀ m. (MonadState CompilerState m) ⇒ ScopeId → Span → Maybe Cst.Type.Type' → m Type'
typeMaybeAst scope _ (Just t) = typeAst scope t
typeMaybeAst _ s Nothing = do
  i ← genNodeId
  keepNodeAt i s
  pure $ TyUnknown i

keepType ∷ ∀ m cst. (MonadState CompilerState m, Cst.HasTrivia cst) ⇒ cst → Type' → m Type'
keepType c t = keepNodeAt (nodeId t) (Cst.spanOf c) $> t

typeAst ∷ ∀ m. (MonadState CompilerState m) ⇒ ScopeId → Cst.Type.Type' → m Type'
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
data Expr
  = EVar NodeId Name
  | EApp NodeId Expr Expr
  | EMatch NodeId (Seq Expr) (Seq (Seq Pattern, ScopeId, Expr))
  | EUnknown NodeId
  | EPi NodeId ScopeId Icit Binder Expr Expr
  | Eannotation NodeId Expr Expr
  | EHole NodeId
  deriving (Generic, Show)

data Pattern
  = PName NodeId Binder
  | PProj NodeId Name (Seq Pattern) -- (Thing a b) → ...
  | PWildcard NodeId -- _ → ...
  deriving (Generic, Show)

keepExpr ∷ ∀ m. (MonadState CompilerState m) ⇒ Cst.Expr.Expr → Expr → m Expr
keepExpr c e = keepNodeAt (nodeId e) (Cst.spanOf c) $> e

exprMaybeAst
  ∷ ∀ m
   . (MonadState CompilerState m)
  ⇒ ScopeId
  → Span
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
   . (MonadState CompilerState m)
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

exprAst ∷ ∀ m. (MonadState CompilerState m) ⇒ ScopeId → Cst.Expr.Expr → m Expr
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
  patterns' ← traverse exprPattern patterns

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
      traverse exprPattern . Seq.fromList $
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
exprAst _ e = error $ "Unimplemented `exprAst` for " <> textPretty e

exprPattern ∷ ∀ m. (MonadState CompilerState m) ⇒ Cst.Expr.Pattern → m Pattern
exprPattern cst@(Cst.Expr.PParens (Cst.Delimited{inner = Nothing})) = do
  i ← makeCstNode cst
  pure $ PWildcard i
exprPattern (Cst.Expr.PParens (Cst.Delimited{inner = Just p})) = exprPattern p
exprPattern cst@(Cst.Expr.PWildcard _) = do
  i ← makeCstNode cst
  pure $ PWildcard i
exprPattern cst@(Cst.Expr.PName name) = do
  i ← makeCstNode cst
  pure $ PName i $ Unresolved i name.value
exprPattern cst@(Cst.Expr.PProj (Cst.Expr.PatternProj{head = head', args})) = do
  i ← makeCstNode cst
  args' ← traverse exprPattern args
  let fallback = forceMaybeName i head'
  ni ← case head' of
    Just h → makeCstNode h
    Nothing → pure i -- Does this ever come up?
  pure $ PProj i (Unresolved ni fallback) args'

-- }}}
-- {{{ Modules
addToScope
  ∷ ∀ m cst a k is
   . ( MonadState CompilerState m
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
   . ( MonadState CompilerState m
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
  { span ∷ Maybe Span
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

moduleAst ∷ ∀ m. (MonadState CompilerState m) ⇒ Cst.Mod.Module → m ScopeId
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
      patterns ← traverse exprPattern cst.args

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
     , MonadState CompilerState m
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

resolveName
  ∷ ∀ m a k is node
   . ( O.Is k O.An_AffineFold
     , MonadState CompilerState m
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

exprResolveNames ∷ ∀ m. (MonadState CompilerState m) ⇒ ScopeId → Expr → m Expr
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

patternResolveNames ∷ ∀ m. (MonadState CompilerState m) ⇒ ScopeId → Pattern → m Pattern
patternResolveNames _ w@(PWildcard _) = pure w
patternResolveNames scope (PName i name) =
  resolveName atExprInScope scope i name <&> PName i
patternResolveNames scope (PProj i name args) =
  -- NOTE: we delay the resolution of constructor names
  PProj i name
    <$> traverse (patternResolveNames scope) args

typeResolveNames ∷ ∀ m. (MonadState CompilerState m) ⇒ ScopeId → Type' → m Type'
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

exprDefinitionResolveNames
  ∷ ∀ m
   . (MonadState CompilerState m)
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
   . (MonadState CompilerState m)
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

scopeResolveNames ∷ ∀ m. (MonadState CompilerState m) ⇒ ScopeId → m ()
scopeResolveNames i = do
  mbScope ← gets $ O.preview (#scopes % O.ix i)
  for_ mbScope \scope → do
    for_ (HashMap.toList scope.types) \(k, (Definition ni v)) → do
      v' ← typeDefinitionResolveNames i v
      O.assign (#scopes % O.ix i % #types % O.ix k) $ Definition ni v'
    for_ (HashMap.toList scope.exprs) \(k, (Definition ni v)) → do
      v' ← exprDefinitionResolveNames i v
      O.assign (#scopes % O.ix i % #exprs % O.ix k) $ Definition ni v'

resolveNames ∷ ∀ m. (MonadState CompilerState m) ⇒ m ()
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
      let (_, astState) = runState (moduleAst m) initialState
      putTextLn "========= AST gen"
      putTextLn $ textPretty astState
      putTextLn "========= Name resolution"
      -- let astState' = execState resolveNames $ clearErrors astState
      let astState' = execState resolveNames astState
      putTextLn $ textPretty astState'
      printErrors astState'
 where
  printErrors = Error.printDiagnostic . collectDiagnostics

  initialState =
    initialCompilerState
      { files = HashMap.singleton initialFile mempty
      }

  initialFile = LSP.toNormalizedUri $ LSP.Uri "ethereal://test.waow"

-- }}}
-- {{{ Pretty instances
instance PP.Pretty Name where
  pretty (Unresolved i t) = "[" <> PP.pretty i <> "][?]." <> PP.pretty t
  pretty (Resolved i s t) =
    fold
      [ "["
      , PP.pretty i
      , "][:"
      , PP.pretty s
      , "]."
      , PP.pretty t
      ]

instance PP.Pretty TypeDefinition where
  pretty (TForeign i ty) =
    PP.hsep
      [ "[" <> PP.pretty i <> "]foreign"
      , "::"
      , PP.pretty ty
      ]
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

instance PP.Pretty CompilerState where
  pretty (CompilerState{..}) =
    Cst.prettyTree
      "CompilerState"
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
      , ",[:"
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
      , ",[:"
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
  pretty (EMatch i exprs branches) =
    Cst.prettyTree
      ("[" <> PP.pretty i <> "]Match")
      $ fold
        [ guard (not $ null exprs) $> Base.prettyTree "exprs" do
            PP.pretty <$> toList exprs
        , guard (not $ null branches) *> do
            (patterns, scope, expr) ← toList branches
            pure . Cst.prettyTree ("Branch[:" <> PP.pretty scope <> "]") . catMaybes $
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
  pretty _ = error "Unimplemented"

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
instance (PP.Pretty a) ⇒ PP.Pretty (Definition a) where
  pretty (Definition i inner) =
    fold
      [ "["
      , PP.pretty i
      , "]"
      , PP.pretty inner
      ]

-- }}}
