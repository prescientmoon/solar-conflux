module Sky.Main
  ( _metaContext
  , main
  , runPipeline
  ) where

import Prelude

import Control.Plus (empty)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Dodo (print, twoSpaces)
import Dodo.Ansi (ansiGraphics)
import Effect (Effect)
import Effect.Class.Console as Console
import Run (Run)
import Run as Run
import Run.Except as Except
import Run.Reader as Reader
import Run.State as State
import Run.Supply (SUPPLY)
import Run.Supply as Supply
import Run.Writer as Writer
import Sky.Language.Cst (Cst, toplevelScopeToAst)
import Sky.Language.Effects (ELABORATION_CONTEXT, EVALUATION_ENV, PRINT_CONTEXT, PrintContext(..), QUOTATION_ENV, SKY_LOGGER, SkyLog, _printContext, _skyLogger, runElaborationContext, runEvaluationEnv, runPrintM, runQuotationEnv)
import Sky.Language.Elaboration (infer)
import Sky.Language.Error (SKY_ERROR, SkyError, _skyError)
import Sky.Language.Eval (eval, quote)
import Sky.Language.Log (prettyPrintLogArray)
import Sky.Language.MetaVar (META_CONTEXT)
import Sky.Language.Pretty (prettyPrintTerm)
import Sky.Language.Source (WithSpan, SourceMap)
import Sky.Language.Term (MetaContext)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

runPipeline
  :: forall a o
   . String
  -> SourceMap a
  -> Run
       ( META_CONTEXT a
           + EVALUATION_ENV a
           + QUOTATION_ENV
           + ELABORATION_CONTEXT a
           + SKY_ERROR a
           + PRINT_CONTEXT a
           + SKY_LOGGER
           + SUPPLY Int ()
       )
       o
  -> Array SkyLog /\ Either (SkyError a) (MetaContext a /\ o)
runPipeline originalText sourceMap =
  runElaborationContext
    >>> runEvaluationEnv
    >>> runQuotationEnv
    >>> Supply.runSupply ((+) 1) 0
    >>> State.runStateAt _metaContext empty
    >>> Reader.runReaderAt _printContext
      ( PrintContext
          { originalText
          , scope: mempty
          , sourceMap
          }
      )
    >>> Except.runExceptAt _skyError
    >>> Writer.runWriterAt _skyLogger
    >>> Run.extract

-- invertMap :: forall a b. Hashable a => Hashable b => HashMap a b -> HashMap b a
-- invertMap = HM.toArrayBy Tuple >>> map swap >>> HM.fromArray

main :: String -> Array ({ name :: WithSpan String, value :: Cst }) -> Effect Unit
main originalText cstDeclarations = case NonEmptyArray.fromArray cstDeclarations of
  Nothing -> pure unit
  Just cstDeclarations -> do
    Console.log "Logs:"
    Console.log $ consolePrinter $ prettyPrintLogArray logs
    case pipelineResult of
      Left err -> do
        Console.log "An error occured"
        Console.log $ show err
      Right (metaContext /\ (term /\ type_)) -> do
        Console.log "Original ast"
        Console.log $ show ast
        Console.log "Normal form for term:"
        Console.log $ printTerm term
        Console.log "Inferred type:"
        Console.log $ printTerm type_
        where

        printTerm t = consolePrinter
          $ runPrintM metaContext
              ( PrintContext
                  { scope: mempty
                  , sourceMap
                  , originalText
                  }
              )
          $ prettyPrintTerm t
    where
    consolePrinter = print ansiGraphics (twoSpaces { pageWidth = 80 })

    pipeline ast = do
      ast /\ inferred <- infer ast
      inferred <- quote inferred
      ast <- eval ast
      ast <- quote ast
      pure (ast /\ inferred)

    ast /\ sourceMap = toplevelScopeToAst cstDeclarations
    logs /\ pipelineResult = runPipeline originalText sourceMap (pipeline ast)

---------- Proxies
_metaContext :: Proxy "metaContext"
_metaContext = Proxy