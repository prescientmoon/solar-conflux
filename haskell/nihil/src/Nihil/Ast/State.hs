module Nihil.Ast.State
  ( Type' (..)
  , Expr (..)
  , Pattern (..)
  , AstState (..)
  , ScopeId (..)
  , NodeId (..)
  , genTest
  , nodeId
  , exprAst
  , typeAst
  , moduleAst
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Traversable (for)
import Error.Diagnose qualified as DG
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
  | NoName
  deriving (Show, Generic)

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
  deriving (Show, Generic)

instance IsNode TypeDefinition where
  nodeId (TForeign i _) = i
  nodeId (TAlias i _) = i

data ExprDefinition
  = EForeign NodeId Type'
  | EDeclaration NodeId Type' Expr
  deriving (Show, Generic)

instance IsNode ExprDefinition where
  nodeId (EForeign i _) = i
  nodeId (EDeclaration i _ _) = i

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
  pure res

getScopedName
  ∷ ∀ m a
   . ( MonadState AstState m
     )
  ⇒ (Text → O.AffineFold Scope (Maybe a))
  → ScopeId
  → Name
  → m (Maybe (Name, a))
getScopedName _ _ NoName = pure $ Nothing
getScopedName at _ resolved@(Resolved scope name) = do
  v ← gets $ O.preview $ O.castOptic @O.An_AffineFold (#scopes % O.at scope % O._Just % at name % O._Just)
  pure $ (resolved,) <$> v

typeInScope ∷ ScopeId → Text → O.AffineTraversal' AstState (Maybe TypeDefinition)
typeInScope scope name = (#scopes % O.at scope % O._Just % #types % O.at name)

addTypeToScope ∷ ∀ m. (MonadState AstState m) ⇒ ScopeId → Text → TypeDefinition → m ()
addTypeToScope scope name ty = O.assign (typeInScope scope name) $ Just ty

exprInScope ∷ ScopeId → Text → O.AffineTraversal' AstState (Maybe ExprDefinition)
exprInScope scope name = (#scopes % O.at scope % O._Just % #exprs % O.at name)

addExprToScope ∷ ∀ m. (MonadState AstState m) ⇒ ScopeId → Text → ExprDefinition → m ()
addExprToScope scope name expr = O.assign (exprInScope scope name) $ Just expr

-- }}}
-- {{{ AST gen state
newtype NodeId = NodeId Natural
  deriving (Generic, Show, Eq)
  deriving newtype (Hashable, Num, PP.Pretty)

class IsNode a where
  nodeId ∷ a → NodeId

instance IsNode NodeId where
  nodeId = id

data AstState = AstState
  { spans ∷ HashMap NodeId Cst.Span
  , scopes ∷ HashMap ScopeId Scope
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

-- keepCstAt ∷ ∀ m c a. (MonadState AstState m, Cst.HasTrivia c, IsNode a) ⇒ c → a → m a
-- keepCstAt c t = keepNodeAt (nodeId t) (Cst.spanOf c) $> t

makeCstNode ∷ ∀ m c. (MonadState AstState m, Cst.HasTrivia c) ⇒ c → m NodeId
makeCstNode c = do
  i ← genNodeId
  keepNodeAt i (Cst.spanOf c)
  pure i

getSpan ∷ ∀ m n. (MonadState AstState m, IsNode n) ⇒ n → m (Maybe Error.Span)
getSpan i = do
  s ← get
  pure $ O.preview (#spans % O.at (nodeId i) % O._Just) s

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

-- {{{ Types
data Type'
  = TyForall NodeId Name Type'
  | TyArrow NodeId Cst.Type.ArrowKind Type' Type'
  | TyApp NodeId Type' Type'
  | TyVar NodeId Name
  | TyUnknown NodeId
  deriving (Generic, Show)

instance IsNode Type' where
  nodeId (TyForall i _ _) = i
  nodeId (TyArrow i _ _ _) = i
  nodeId (TyApp i _ _) = i
  nodeId (TyVar i _) = i
  nodeId (TyUnknown i) = i

-- | The type of types
tyType ∷ ∀ m. (MonadState AstState m) ⇒ m Type'
tyType = do
  i ← genNodeId
  pure $ TyVar i $ Unresolved "Type"

typeMaybeAst ∷ ∀ m. (MonadState AstState m) ⇒ Span → Maybe Cst.Type.Type' → m Type'
typeMaybeAst _ (Just t) = typeAst t
typeMaybeAst s Nothing = do
  i ← genNodeId
  keepNodeAt i s
  pure $ TyUnknown i

keepType ∷ ∀ m. (MonadState AstState m) ⇒ Cst.Type.Type' → Type' → m Type'
keepType c t = keepNodeAt (nodeId t) (Cst.spanOf c) $> t

typeAst ∷ ∀ m. (MonadState AstState m) ⇒ Cst.Type.Type' → m Type'
typeAst cst@(Cst.Type.TyVar (Cst.Type.Var{..})) = do
  i ← genNodeId
  keepType cst $ TyVar i $ Unresolved $ O.view #value name
typeAst cst@(Cst.Type.TyApp (Cst.Type.App{..})) = do
  i ← genNodeId
  f' ← typeAst f
  a' ← typeAst a
  keepType cst $ TyApp i f' a'
typeAst cst@(Cst.Type.TyArrow (Cst.Type.Arrow{..})) = do
  let s = Cst.spanOf cst
  i ← genNodeId
  from' ← typeMaybeAst s from
  to' ← typeMaybeAst s to
  keepType cst $ TyArrow i (O.view #value kind) from' to'
typeAst cst@(Cst.Type.TyForall (Cst.Type.Forall{..})) = do
  inner ← typeMaybeAst (Cst.spanOf cst) ty
  let go inside name = do
        i ← genNodeId
        keepType cst $ TyForall i (Unresolved $ O.view #value name) inside
  foldlM go inner (Seq.reverse names)
typeAst cst@(Cst.Type.TyParens (Cst.Delimited{inner})) = do
  typeMaybeAst (Cst.spanOf cst) inner

-- }}}
-- {{{ Expressions
data Expr
  = EVar NodeId Name
  | EApp NodeId Expr Expr
  | EMatch NodeId (Seq Expr) (Seq (Seq Pattern, Expr))
  | EUnknown NodeId
  deriving (Generic, Show)

instance IsNode Expr where
  nodeId (EVar i _) = i
  nodeId (EApp i _ _) = i
  nodeId (EMatch i _ _) = i
  nodeId (EUnknown i) = i

data Pattern
  = PName Name
  | PProj Name (Seq Pattern) -- (Thing a b) → ...
  | PWildcard -- _ → ...
  deriving (Generic, Show)

keepExpr ∷ ∀ m. (MonadState AstState m) ⇒ Cst.Expr.Expr → Expr → m Expr
keepExpr c e = keepNodeAt (nodeId e) (Cst.spanOf c) $> e

exprMaybeAst ∷ ∀ m. (MonadState AstState m) ⇒ Span → Maybe Cst.Expr.Expr → m Expr
exprMaybeAst _ (Just e) = exprAst e
exprMaybeAst s Nothing = do
  i ← genNodeId
  keepNodeAt i s
  pure $ EUnknown i

exprAst ∷ ∀ m. (MonadState AstState m) ⇒ Cst.Expr.Expr → m Expr
exprAst cst@(Cst.Expr.EVar (Cst.Expr.Var{..})) = do
  i ← genNodeId
  keepExpr cst $ EVar i $ Unresolved name.value
exprAst cst@(Cst.Expr.EApp (Cst.Expr.App{..})) = do
  i ← genNodeId
  f' ← exprAst f
  a' ← exprAst a
  keepExpr cst $ EApp i f' a'
exprAst cst@(Cst.Expr.ELambda (Cst.Expr.Lambda{..})) = do
  i ← genNodeId
  body' ← exprMaybeAst (Cst.spanOf cst) body
  patterns' ← traverse exprPattern patterns
  keepExpr cst $ EMatch i mempty $ pure (patterns', body')
exprAst cst@(Cst.Expr.EMatch (Cst.Expr.Match{..})) = do
  i ← genNodeId
  exprs' ←
    traverse exprAst
      . Seq.fromList
      $ O.view (O.partsOf O.traversed) exprs
  branches' ← for branches \(Cst.Expr.MatchBranch{..}) → do
    patterns' ←
      traverse exprPattern . Seq.fromList $
        O.view (O.partsOf O.traversed) patterns
    body' ← exprMaybeAst (Cst.spanOf cst) body
    pure $ (patterns', body')
  keepExpr cst $ EMatch i exprs' branches'
exprAst _ = error "Unimplemented"

exprPattern ∷ ∀ m. (MonadState AstState m) ⇒ Cst.Expr.Pattern → m Pattern
exprPattern (Cst.Expr.PParens (Cst.Delimited{inner = Nothing})) = pure PWildcard
exprPattern (Cst.Expr.PParens (Cst.Delimited{inner = Just p})) = exprPattern p
exprPattern (Cst.Expr.PWildcard _) = pure PWildcard
exprPattern (Cst.Expr.PName name) = pure $ PName $ Unresolved name.value
exprPattern (Cst.Expr.PProj (Cst.Expr.PatternProj{head = head', args})) = do
  args' ← traverse exprPattern args
  pure $ PProj (maybe NoName (Unresolved . O.view #value) head') args'

-- }}}
-- {{{ Modules
ensureUnusedValueName
  ∷ ∀ m cst
   . (MonadState AstState m, Cst.HasTrivia cst)
  ⇒ ScopeId
  → Text
  → cst
  → m Bool
ensureUnusedValueName scope name cst = do
  mbExisting ← gets $ O.preview (exprInScope scope name % O._Just)
  case mbExisting of
    Nothing → pure True
    Just existing → do
      existingSpan ← getSpan existing
      reportError
        scope
        "DuplicateValueDeclaration"
        "Duplicate value declaration."
        ( catMaybes
            [ Just
                ( Cst.spanOf cst
                , DG.This $
                    fold
                      [ "Cannot declare value `"
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

ensureUnusedTypeName
  ∷ ∀ m cst
   . (MonadState AstState m, Cst.HasTrivia cst)
  ⇒ ScopeId
  → Text
  → cst
  → m Bool
ensureUnusedTypeName scope name cst = do
  mbExisting ← gets $ O.preview (typeInScope scope name % O._Just)
  case mbExisting of
    Nothing → pure True
    Just existing → do
      existingSpan ← getSpan existing
      reportError
        scope
        "DuplicateValueDeclaration"
        "Duplicate value declaration."
        ( catMaybes
            [ Just
                ( Cst.spanOf cst
                , DG.This $
                    fold
                      [ "Cannot declare value `"
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
  deriving (Generic)

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

    unused ← ensureUnusedValueName scope name' s
    addExprToScope scope (forceMaybeUsed i unused name')
      . EDeclaration i ty
      . EMatch i mempty
      $ (\(_, p, e) → (p, e)) <$> elem'.branches

  pure scope
 where
  go ∷ ScopeId → ModuleAcc → Cst.Mod.Declaration → m ModuleAcc
  go scope acc = \case
    Cst.Mod.DeclTypeAlias cst → do
      i ← makeCstNode cst

      body' ← typeMaybeAst (Cst.spanOf cst) cst.body
      let textName = forceMaybeName i cst.name

      addTypeToScope scope textName $ TAlias i body'
      pure acc
    Cst.Mod.DeclForeignType cst → do
      i ← makeCstNode cst

      t ← tyType
      let textName = forceMaybeName i cst.name

      addTypeToScope scope textName $ TForeign i t
      pure acc
    Cst.Mod.DeclForeignValue cst → do
      i ← makeCstNode cst

      ty ← typeMaybeAst (Cst.spanOf cst) cst.ty
      let name' = forceMaybeName i cst.name

      unused ← ensureUnusedValueName scope name' cst
      addExprToScope scope (forceMaybeUsed i unused name') $ EForeign i ty
      pure acc
    Cst.Mod.DeclValueTypeAnn cst → do
      i ← makeCstNode cst

      ty ← typeMaybeAst (Cst.spanOf cst) cst.ty
      let name' = cst.name.value

      pure
        . HashMap.update (Just . O.over #span ((<>) . Just $ Cst.spanOf cst)) name'
        . HashMap.update (Just . O.over #types (|> (i, ty))) name'
        $ if HashMap.member name' acc
          then acc
          else HashMap.insert name' (ModuleAccElem mempty mempty mempty) acc
    Cst.Mod.DeclValueEquation cst → do
      i ← makeCstNode cst

      expr ← exprMaybeAst (Cst.spanOf cst) cst.expr
      patterns ← traverse exprPattern cst.args
      let name' = cst.name.value

      pure
        . HashMap.update (Just . O.over #span ((<>) . Just $ Cst.spanOf cst)) name'
        . HashMap.update (Just . O.over #branches (|> (i, patterns, expr))) name'
        $ if HashMap.member name' acc
          then acc
          else HashMap.insert name' (ModuleAccElem mempty mempty mempty) acc
    _ → error "Unimplemented"

-- }}}

genTest ∷ Text → IO ()
genTest source = do
  parsed ← Parser.parseTest Parser.pModule source
  case parsed of
    Nothing → pure ()
    Just m → do
      let astState = execState (moduleAst m) initialState
      putTextLn $ textPretty astState
      for_ astState.scopes \s → do
        Error.printDiagnostic $
          DG.addFile
            (Error.addReports $ toList s.reports)
            "<test>"
            (Text.unpack source)
 where
  initialState =
    AstState
      { spans = mempty
      , scopes = mempty
      , nextNodeId = 0
      , nextScopeId = 0
      }

-- {{{ Pretty instances
instance PP.Pretty Name where
  pretty (Unresolved t) = PP.pretty t <> "?"
  pretty NoName = "???"

instance PP.Pretty TypeDefinition where
  pretty (TForeign i ty) = PP.hsep ["[" <> PP.pretty i <> "]foreign", "::", PP.pretty ty]
  pretty (TAlias i ty) = PP.hsep ["[" <> PP.pretty i <> "]alias", PP.pretty ty]

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
  pretty (EForeign i ty) =
    PP.hsep
      [ "[" <> PP.pretty i <> "]foreign"
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
  pretty (TyForall i name ty) =
    fold
      [ "["
      , PP.pretty i
      , "]∀"
      , PP.pretty name
      , "."
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
      ("Match[" <> PP.pretty i <> "]")
      $ catMaybes
        [ guard (not $ null exprs) $> Base.prettyTree "exprs" do
            PP.pretty <$> toList exprs
        , guard (not $ null exprs) $> Base.prettyTree "branches" do
            (patterns, expr) ← toList branches
            pure . Cst.prettyTree "Branch" . catMaybes $
              [ guard (not $ null patterns) $> Base.prettyTree "patterns" do
                  PP.pretty <$> toList patterns
              , pure $ PP.pretty expr
              ]
        ]
  pretty (EApp i f a) =
    Cst.prettyTree
      ("App[" <> PP.pretty i <> "]")
      [ PP.pretty f
      , PP.pretty a
      ]

instance PP.Pretty Pattern where
  pretty PWildcard = "_"
  pretty (PName n) = PP.pretty n
  pretty (PProj n args)
    | null args = "." <> PP.pretty n
    | otherwise =
        fold
          [ "(."
          , PP.pretty n
          , " "
          , fold $ intersperse " " $ PP.pretty <$> toList args
          , ")"
          ]

-- }}}
--
-- case a, b of
--   A, B, C -> ...
--   _, b' -> ...
--
-- case a of
--   A -> case b of
--     B -> intro where
--       C -> ...
--   _ -> case b of
--
-- something Nothing x = x
-- something .Just y x = x
--     b' -> ...
