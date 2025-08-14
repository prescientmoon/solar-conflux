module Nihil.Compiler.Monad where

import Data.HashMap.Strict qualified as HashMap
import Data.Sequence ((|>))
import Data.Text qualified as Text
import Error.Diagnose qualified as DG
import Language.LSP.Protocol.Types qualified as LSP
import Nihil.Compiler.Ast
  ( Expr
  , IsNode (nodeId)
  , Name
  , NodeId
  , Scope (..)
  , ScopeId
  )
import Nihil.Compiler.Term
  ( CheckVar
  , Env
  , Lvl
  , MetaVar
  , Pruning
  , Term
  , Type'
  , VType
  , Value
  )
import Nihil.Cst.Base qualified as Cst
import Nihil.Cst.Module qualified as Cst
import Nihil.Error qualified as Error
import Nihil.Utils qualified as Utils
import Optics ((%))
import Optics qualified as O
import Prettyprinter qualified as PP
import Relude

-- {{{ Monad type
data CompilerState = CompilerState
  { spans ∷ HashMap NodeId Cst.Span
  -- ^ Source spans for the AST
  , scopes ∷ HashMap ScopeId Scope
  -- ^ Name resolution info
  , files ∷ HashMap Error.Path FileData
  -- ^ New scopes automatically get added here
  , reports ∷ Seq Error.Report
  -- ^ These get invalidated whenever *any* file changes.
  -- This is not optimal, but it makes reasoning about changes
  -- much easier.
  , --------- ID generation
    nextNodeId ∷ NodeId
  , nextScopeId ∷ ScopeId
  , ---------- Elaboration state
    checkVars ∷ Seq CheckEntry
  , metaVars ∷ Seq MetaEntry
  }
  deriving (Generic)

type MonadCompile m =
  ( MonadReader CompilerContext m
  , MonadState CompilerState m
  )

-- }}}
-- {{{ Running the monad
initialCompilerState ∷ CompilerState
initialCompilerState =
  CompilerState
    { spans = mempty
    , scopes = mempty
    , nextNodeId = 0
    , nextScopeId = 0
    , files = mempty
    , reports = mempty
    , checkVars = mempty
    , metaVars = mempty
    }

-- }}}
-- {{{ File data
data FileData = FileData
  { source ∷ Maybe Text
  , parsed ∷ Maybe Cst.Module
  , parsingReports ∷ Seq Error.Report
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

-- }}}
-- {{{ Span / node id handling

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
getSpanConfident
  ∷ ∀ m n
   . (MonadState CompilerState m, IsNode n, PP.Pretty n)
  ⇒ n
  → m Error.Span
getSpanConfident a =
  getSpan a <&> \case
    Just b → b
    Nothing → error $ "No span found for " <> Utils.textPretty a

-- }}}
-- {{{ Scope manipulation
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

instance PP.Pretty CompilerState where
  pretty (CompilerState{..}) =
    Cst.prettyTree
      "CompilerState"
      $ catMaybes
        [ guard (not $ null scopes) $> Cst.prettyTree "scopes" do
            (k, v) ← HashMap.toList scopes
            pure $ fold ["[", PP.pretty k, "]", PP.pretty v]
        ]

-- }}}
-- {{{ Error reporting

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
    (|> DG.Err (Just . PP.pretty $ "Compiler" <> code) desc markers hints)

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
            (fromMaybe "ethereal.waow" $ LSP.uriToFilePath $ LSP.fromNormalizedUri k)
            (Text.unpack source)
    )
    reports
    (HashMap.toList st.files)

-- }}}
-- {{{ Elaboration state & context
data CheckEntry
  = Checked Term
  | -- | We got stuck solving Γ ⊢ t : A, thus elaborated to a meta m for now.
    Unchecked CompilerContext Expr VType MetaVar
  deriving (Show, Generic)

data MetaEntry
  = Solved Value VType
  | -- | An unsolved meta that blocks a numb of checking problems.
    Unsolved (HashSet CheckVar) VType
  deriving (Show, Generic)

data VarEntry
  = Bind (Maybe Name) Type'
  | Define Name Term Type'
  deriving (Show, Generic)

data CompilerContext = CompilerContext
  { env ∷ Env
  -- ^ Holds concrete values to be used for evaluation
  , srcNames ∷ HashMap (ScopeId, Text) (Lvl, VType)
  -- ^ Holds the types for the various names in context.
  , rigid ∷ Pruning
  -- ^ Which variables in scope represent rigid variables
  -- (i.e. things not bound by a `let-in` and the like).
  , variables ∷ Seq VarEntry
  }
  deriving (Show, Generic)

defaultCompilerContext ∷ CompilerContext
defaultCompilerContext =
  CompilerContext
    { env = mempty
    , srcNames = mempty
    , rigid = mempty
    , variables = mempty
    }

-- }}}
