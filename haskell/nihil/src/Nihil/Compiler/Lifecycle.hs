-- | Handle file changes in regards to the compiler state.
module Nihil.Compiler.Lifecycle (onChange) where

import Data.HashMap.Strict qualified as HashMap
import Nihil.Compiler.Gen qualified as Compiler
import Nihil.Compiler.Monad (CompilerState (..), FileData (..), MonadCompile)
import Nihil.Compiler.Resolve qualified as Compiler
import Nihil.Error qualified as Error
import Nihil.Parser.Core qualified as Parser
import Nihil.Parser.Module qualified as Parser
import Optics ((%))
import Optics qualified as O
import Relude

-- clearScope ∷ ∀ m. (MonadCompile m) ⇒ Ast.ScopeId → m ()
-- clearScope scope = O.assign (#scopes % O.at scope) Nothing
--
-- clearNode ∷ ∀ m. (MonadCompile m) ⇒ Ast.NodeId → m ()
-- clearNode i = O.assign (#spans % O.at i) Nothing

onChange ∷ ∀ m. (MonadCompile m) ⇒ Error.Path → Text → m ()
onChange p content = do
  -- Nuke the derived state
  O.assign #scopes mempty
  O.assign #spans mempty
  O.assign #reports mempty
  O.assign (#files % O.at p) $ Just mempty

  -- Clear stale toplevel scopes associated with each file.
  files ← O.use #files
  for_ (HashMap.keys files) \(key) → do
    O.assign (#files % O.ix key % #mainScope) Nothing

  -- Re-parse the data
  let (parsingReports, parsingResult) =
        Parser.runParser Parser.pModule p content
  let mbModule = fst <$> parsingResult

  -- Save parsed data
  O.assign (#files % O.ix p % #parsingReports) parsingReports
  O.assign (#files % O.ix p % #parsed) mbModule

  -- Re-generate ASTs
  files' ← O.use #files
  for_ (HashMap.toList files') \(key, file) → do
    for_ file.parsed \module' → do
      scope ← Compiler.moduleAst module'
      O.assign (#files % O.ix key % #mainScope) $ Just scope

  Compiler.resolveNames
