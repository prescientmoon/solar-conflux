module Sky.Language.Eval where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Run (Run)
import Run.Reader (runReaderAt)
import Run.Reader as Reader
import Safe.Coerce (coerce)
import Sky.Language.Effects (EVALUATION_ENV, EvalM, QUOTATION_ENV, QuoteEnv(..), SkyM, _evaluationEnv, _quotationEnv, augumentEnv, environment, extendPrintScope, getDepth, getScope, increaseDepth, lookupSource, setPrintScope)
import Sky.Language.Error (class SourceSpot, EvaluationError(..), SKY_ERROR, lambdaArgument, piArgument, throwEvaluationError)
import Sky.Language.Log (evaluating)
import Sky.Language.MetaVar (lookupMeta)
import Sky.Language.Term (Closure(..), Env(..), Index(..), Level(..), Mask(..), MetaVar, NameEnv(..), Spine(..), Term(..), Value(..), extendEnv, extendSpine)
import Type.Row (type (+))

---------- Helpers
applyClosure
  :: forall r a
   . SourceSpot a
  => Closure a
  -> Value a
  -> SkyM a r (Value a)
applyClosure (Closure { env, names, term }) argument =
  evalWith names (extendEnv argument env) term

vApply
  :: forall a r
   . SourceSpot a
  => Value a
  -> Value a
  -> SkyM a r (Value a)
vApply f a = case f of
  VLambda _ body -> applyClosure body a
  VMetaApplication source meta spine ->
    pure $ VMetaApplication source meta (extendSpine spine a)
  VVariableApplication source var spine ->
    pure $ VVariableApplication source var (extendSpine spine a)
  VAssumptionApplication source type_ spine ->
    pure $ VAssumptionApplication source type_ (extendSpine spine a)
  VPi source domain codomain ->
    throwEvaluationError $ PiNotCallable
      { piDomain: domain, piCodomain: codomain, piSource: source, argument: a }
  VStar source -> throwEvaluationError
    $ StarNotCallable { starSource: source, argument: a }
  VSourceAnnotation source inner ->
    vApply inner a
      <#> VSourceAnnotation source

vApplySpine
  :: forall r a
   . SourceSpot a
  => Value a
  -> Spine a
  -> SkyM a r (Value a)
vApplySpine to spine@(Spine arguments) = case to of
  -- These two paths are here for performance reasons 
  VMetaApplication source meta originalSpine ->
    pure $ VMetaApplication source meta (originalSpine <> spine)
  VVariableApplication source var originalSpine ->
    pure $ VVariableApplication source var (originalSpine <> spine)
  VAssumptionApplication source ty originalSpine ->
    pure $ VAssumptionApplication source ty (originalSpine <> spine)
  VSourceAnnotation source inner ->
    vApplySpine inner spine
      <#> VSourceAnnotation source
  other -> Array.foldM vApply other arguments

vApplyMaskedeEnvironment
  :: forall a r
   . SourceSpot a
  => Env a
  -> Value a
  -> Mask
  -> SkyM a r
       (Value a)
vApplyMaskedeEnvironment (Env { scope }) to (Mask mask) =
  Array.zip (Array.reverse scope) mask
    # Array.mapMaybe onlyAccepted
    # Spine
    # vApplySpine to
  where
  onlyAccepted (argument /\ keep) = if keep then Just argument else Nothing

-- | Lookup the value a var has in the current environment
lookupVar :: forall r a. a -> Index -> Run (SKY_ERROR a + EVALUATION_ENV a r) (Value a)
lookupVar source indexObject@(Index index) = do
  (Env { scope }) <- environment
  case Array.index scope index of
    Just value -> pure value
    Nothing -> throwEvaluationError
      $ VarNotInScope
          { source, index: indexObject }

-- | Create a closure holding the current environment
makeClosure :: forall r a. SourceSpot a => a -> Term a -> EvalM a r (Closure a)
makeClosure source term = do
  name <- lookupSource source
  names <- getScope
  environment
    <#>
      { term
      , names: NameEnv (Array.cons name $ coerce names)
      , env: _
      }
    <#> Closure

-- | Evaluate a term under a given environment
evalWith
  :: forall r a
   . SourceSpot a
  => NameEnv
  -> Env a
  -> Term a
  -> SkyM a r (Value a)
evalWith names env = eval
  >>> runReaderAt _evaluationEnv env
  >>> setPrintScope names

-- | Evaluate a term
eval
  :: forall r a
   . SourceSpot a
  => Term a
  -> EvalM a r (Value a)
eval t = evaluating t case t of
  Star source -> pure $ VStar source
  SourceAnnotation source inner -> eval inner
    <#> VSourceAnnotation source
  Var source index -> ado
    result <- lookupVar source index
    env <- environment
    in VSourceAnnotation source result
  Application source lhs rhs -> do
    lhs <- eval lhs
    rhs <- eval rhs
    vApply lhs rhs
  Lambda source body -> ado
    closure <- makeClosure (lambdaArgument source) body
    in VLambda source closure
  -- TODO: cases like this could be run in paralle, to accumulate more errors
  Pi source domain codomain -> ado
    domain <- eval domain
    codomain <- makeClosure (piArgument source) codomain
    in VPi source domain codomain
  Let source definition body -> do
    definition <- eval definition
    name <- lookupSource source
    extendPrintScope [ name ] $ augumentEnv (extendEnv definition) $ eval body
  Meta source meta -> evalMeta source meta
  InsertedMeta source meta mask -> do
    env <- environment
    meta <- evalMeta source meta
    vApplyMaskedeEnvironment env meta mask
  Assumption source type_ -> ado
    type_ <- eval type_
    in VAssumptionApplication source type_ mempty

evalMeta :: forall a r. a -> MetaVar -> EvalM a r (Value a)
evalMeta source meta = lookupMeta source meta <#> case _ of
  Just v -> v
  Nothing -> VMetaApplication source meta mempty

-- | Attempt to resume execution blocked by an unsolved meta variable
force :: forall a r. SourceSpot a => Value a -> SkyM a r (Value a)
force value = case value of
  VMetaApplication source meta arguments ->
    lookupMeta source meta >>= case _ of
      Nothing -> pure value
      Just meta -> do
        applied <- vApplySpine meta arguments
        force applied
  VSourceAnnotation source inner ->
    force inner <#> VSourceAnnotation source
  other -> pure other

---------- Quotation (aka Value -> Term conversion)
-- | Convert a spine application to a term
quoteSpine :: forall a r. SourceSpot a => a -> Term a -> Spine a -> SkyM a r (Term a)
quoteSpine source term (Spine spine) = Array.foldM go term spine
  where
  go previous argument = ado
    argument <- quote argument
    -- | TODO: make sure using the source here is fine
    in Application source previous argument

-- | When encountering a variable while quoting,
-- | , the variable will hold the level it came from as it's payload*
-- | To reconstruct the correct de brujin index, 
-- | we first see how far the var was bound (level - index),
-- | and then substract 1 to account for the fact the 
-- | payload of the var refers to the lambda of origin, not the inside of the lambda
-- |
-- | * as long as the value is well formed
quoteIndex :: Level -> Level -> Index
quoteIndex (Level level) (Level index) = Index $ level - index - 1

-- | Convert a value to a term
quote :: forall a r. SourceSpot a => Value a -> SkyM a r (Term a)
quote = force >=> case _ of
  VStar source -> pure $ Star source
  VMetaApplication source meta spine -> do
    quoteSpine source (Meta source meta) spine
  VVariableApplication source var spine -> do
    level <- getDepth
    quoteSpine source (Var source $ quoteIndex level var) spine
  VAssumptionApplication source type_ spine -> do
    type_ <- quote type_
    quoteSpine source type_ spine
  VPi source domain codomain -> do
    marker <- getDepthMarker (piArgument source)
    domain <- quote domain
    codomain <- applyClosure codomain marker
    name <- lookupSource (piArgument source)
    codomain <- extendPrintScope [ name ] $ increaseDepth $ quote codomain
    pure $ Pi source domain codomain
  VLambda source body -> do
    marker <- getDepthMarker (lambdaArgument source)
    body <- applyClosure body marker
    name <- lookupSource (lambdaArgument source)
    body <- extendPrintScope [ name ] $ increaseDepth $ quote body
    pure $ Lambda source body
  VSourceAnnotation source inner ->
    quote inner <#> SourceAnnotation source

-- | Normalize a term
normalForm
  :: forall a r
   . SourceSpot a
  => Term a
  -> EvalM a r (Term a)
normalForm term = do
  (Env { scope }) <- environment
  let quotationEnv = QuoteEnv { depth: Level (Array.length scope) }
  eval term
    >>= quote
    # Reader.runReaderAt _quotationEnv quotationEnv

-- | Create a variable carrying the depth it came from as it's payload
getDepthMarker :: forall a r. a -> Run (QUOTATION_ENV r) (Value a)
getDepthMarker source = getDepth <#> \depth -> VVariableApplication source depth mempty
