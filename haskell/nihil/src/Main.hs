module Main where

import Data.HashMap.Strict qualified as HashMap
import Language.LSP.Protocol.Types qualified as LSP
import Nihil.Compiler.Ast
  ( Definition (Definition)
  , ExprDefinition (EDeclaration)
  , Scope (..)
  )
import Nihil.Compiler.Elab qualified as Compiler
import Nihil.Compiler.Gen qualified as Ast
import Nihil.Compiler.Monad
  ( CompilerState (..)
  , FileData (..)
  , collectDiagnostics
  , defaultCompilerContext
  , initialCompilerState
  )
import Nihil.Compiler.Resolve as Compiler
import Nihil.Error qualified as Error
import Nihil.Parser.Core qualified as Parser
import Nihil.Parser.Module qualified as Parser
import Nihil.Server qualified as Server
import Nihil.Utils qualified as Utils
import Optics ((%))
import Optics qualified as O
import Prettyprinter qualified as PP
import Relude

main ∷ IO Int
main = Server.run

-- {{{ REPL testing
replTest ∷ FilePath → IO ()
replTest filePath = do
  sourceBS ← readFileBS filePath
  let source = decodeUtf8 sourceBS
  let initialFile = LSP.toNormalizedUri $ LSP.filePathToUri filePath
  let initialState =
        initialCompilerState
          { files =
              HashMap.singleton initialFile $
                mempty
                  { source = Just source
                  }
          }
  parsed ← Parser.parseTest Parser.pModule initialFile source
  case parsed of
    Nothing → pure ()
    Just m →
      flip evalStateT initialState $
        flip runReaderT defaultCompilerContext do
          sid ← Ast.moduleAst m

          putTextLn "========= AST gen"
          s ← get
          putTextLn $ Utils.textPretty s

          putTextLn "========= Name resolution"
          Compiler.resolveNames
          s' ← get
          putTextLn $ Utils.textPretty s'

          putTextLn "========= Elaboration"
          for_ (O.preview (#scopes % O.ix sid) s') \scope → do
            for_ (HashMap.toList scope.exprs) \case
              (n, Definition _ (EDeclaration i _ e)) → do
                (term, vty) ← Compiler.infer e
                Compiler.checkEverything i
                term' ← Compiler.normalForm term
                ty ← Compiler.quote vty
                putTextLn . Utils.textPretty' . PP.align . PP.vsep $
                  [ "Declaration"
                  , PP.indent 2 $ PP.pretty n
                  , "has type:"
                  , PP.indent 2 $ PP.pretty ty
                  , "and value"
                  , PP.indent 2 $ PP.pretty term'
                  ]
              _ → pure ()

          s'' ← get
          printErrors s''

          pure ()
 where
  printErrors ∷ ∀ m. (MonadIO m) ⇒ CompilerState → m ()
  printErrors = Error.printDiagnostic . collectDiagnostics

-- }}}
