module Sky.Main where

import Prelude

import Control.Plus (empty)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), swap)
import Data.Tuple.Nested ((/\), type (/\))
import Dodo (print, twoSpaces)
import Dodo.Ansi (ansiGraphics)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Run (Run)
import Run as Run
import Run.Except as Except
import Run.State as State
import Run.Supply (SUPPLY)
import Run.Supply as Supply
import Safe.Coerce (coerce)
import Sky.Debug (showPretty)
import Sky.Language.Ast (ELABORATION_CONTEXT, infer, runElaborationContext)
import Sky.Language.Cst (Cst, WithSpan, toplevelScopeToAst)
import Sky.Language.Error (SKY_ERROR, SkyError, _skyError)
import Sky.Language.Eval (EVALUATION_ENV, QUOTATION_ENV, eval, quote, runEvaluationEnv, runQuotationEnv)
import Sky.Language.MetaVar (META_CONTEXT)
import Sky.Language.Pretty (PrintContext(..), prettyPrintTerm, runPrintM)
import Sky.Language.Term (MetaContext(..), Name(..))
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

runPipeline
  :: forall a o
   . Run
       ( META_CONTEXT a
           + EVALUATION_ENV a
           + QUOTATION_ENV
           + ELABORATION_CONTEXT a
           + SKY_ERROR a
           + SUPPLY Int ()
       )
       o
  -> Either (SkyError a) (MetaContext a /\ o)
runPipeline =
  runElaborationContext
    >>> runEvaluationEnv
    >>> runQuotationEnv
    >>> Supply.runSupply ((+) 1) 0
    >>> State.runStateAt _metaContext empty
    >>> Except.runExceptAt _skyError
    >>> Run.extract

invertMap :: forall a b. Hashable a => Hashable b => HashMap a b -> HashMap b a
invertMap = HM.toArrayBy Tuple >>> map swap >>> HM.fromArray

main :: String -> Array ({ name :: WithSpan String, value :: Cst }) -> Effect Unit
main originalText cstDeclarations = case NonEmptyArray.fromArray cstDeclarations of
  Nothing -> pure unit
  Just cstDeclarations -> case runPipeline (pipeline ast) of
    Left err -> do
      Console.log "An error occured"
      Console.log $ show err
    Right ((MetaContext metaContext) /\ (term /\ type_)) -> do
      Console.log "Original ast"
      Console.log $ show ast
      Console.log "Normal form for term:"
      Console.log $ printTerm term
      Console.log "Inferred type:"
      Console.log $ printTerm type_
      -- Console.log "Metas: "
      -- Console.log $ showPretty $ lmap printTerm $  metaContext.metaNames
      where
      printTerm t = print ansiGraphics (twoSpaces { pageWidth = 80 })
        $ runPrintM
            ( PrintContext
                { metaNames: coerce $ invertMap metaContext.metaNames
                , scope: []
                , sourceMap
                , originalText
                }
            )
        $ prettyPrintTerm t
    where

    pipeline ast = do
      let a = unsafePerformEffect $ log $ showPretty ast
      ast /\ inferred <- infer ast
      inferred <- quote inferred
      ast <- eval ast
      ast <- quote ast
      pure (ast /\ inferred)

    ast /\ sourceMap = toplevelScopeToAst cstDeclarations

---------- Proxies
_metaContext :: Proxy "metaContext"
_metaContext = Proxy