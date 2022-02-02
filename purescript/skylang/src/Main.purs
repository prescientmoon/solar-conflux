module Sky.Main where

import Prelude

import Control.Plus (empty)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Run (Run)
import Run as Run
import Run.Except as Except
import Run.State as State
import Run.Supply (SUPPLY)
import Run.Supply as Supply
import Sky.Language.Ast (ELABORATION_CONTEXT, infer, runElaborationContext)
import Sky.Language.Cst (Cst, WithSpan, toplevelScopeToAst)
import Sky.Language.Error (SKY_ERROR, SkyError, _skyError)
import Sky.Language.Eval (EVALUATION_ENV, QUOTATION_ENV, eval, runEvaluationEnv, runQuotationEnv)
import Sky.Language.MetaVar (META_CONTEXT)
import Sky.Language.Term (MetaContext)
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

main :: Array ({ name :: WithSpan String, value :: Cst }) -> Effect Unit
main cstDeclarations = case NonEmptyArray.fromArray cstDeclarations of
  Nothing -> pure unit
  Just cstDeclarations -> case runPipeline (pipeline ast) of
    Left err -> do
      Console.log "An error occured"
      Console.log $ show err
    Right (metaContext /\ (term /\ type_)) -> do
      Console.log "Normal form for term:"
      Console.log $ show term
      Console.log "Inferred type:"
      Console.log $ show type_
    where
    pipeline ast = do
      ast /\ inferred <- infer ast
      ast <- eval ast
      pure (ast /\ inferred)

    ast /\ sourceMap = toplevelScopeToAst cstDeclarations

---------- Proxies
_metaContext :: Proxy "metaContext"
_metaContext = Proxy