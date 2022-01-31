module Sky.Language.Eval where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Run (Run)
import Run.Reader (Reader, runReaderAt)
import Run.Reader as Reader
import Safe.Coerce (coerce)
import Sky.Language.Error (EvaluationError(..), SKY_ERROR, throwEvaluationError)
import Sky.Language.MetaVar (META_CONTEXT, lookupMeta)
import Sky.Language.Term (Closure(..), Env(..), Index(..), Level(..), Mask(..), MetaVar, Spine(..), Term(..), Value(..), extendEnv, extendSpine)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

---------- Types
newtype QuoteEnv = QuoteEnv { depth :: Level }

---------- Effect-related stuff

type EVALUATION_ENV a r = (evaluationEnv :: Reader (Env a) | r)
type QUOTATION_ENV r = (quotationEnv :: Reader QuoteEnv | r)

-- | Base monad with everything required for evaluation to take place
type EvalM a r = Run (EVALUATION_ENV a + META_CONTEXT a + SKY_ERROR a r)
-- | Base monad with everything required for quotation to take place
type QuoteM a r = Run (QUOTATION_ENV + META_CONTEXT a + SKY_ERROR a r)

-- | Expose the current evaluation environment
environment :: forall a r. Run (EVALUATION_ENV a r) (Env a)
environment = Reader.askAt _evaluationEnv

-- | Expose the current depth from the context
getDepth :: forall r. Run (QUOTATION_ENV r) Level
getDepth = Reader.askAt _quotationEnv
  <#> \(QuoteEnv { depth }) -> depth

-- | Run a computation in a context deeper by 1 than the current one
increaseDepth :: forall r. Run (QUOTATION_ENV r) ~> Run (QUOTATION_ENV r)
increaseDepth = Reader.localAt _quotationEnv
  \(QuoteEnv { depth }) -> QuoteEnv { depth: coerce ((+) 1) depth }

-- | Run a computation in a modified environment
augumentEnv :: forall a r. (Env a -> Env a) -> Run (EVALUATION_ENV a r) ~> Run (EVALUATION_ENV a r)
augumentEnv = Reader.localAt _evaluationEnv

---------- Helpers
applyClosure
  :: forall r a
   . Closure a
  -> Value a
  -> Run (SKY_ERROR a + META_CONTEXT a r) (Value a)
applyClosure (Closure { env, term }) argument =
  evalWith (extendEnv argument env) term

vApply
  :: forall a r
   . Value a
  -> Value a
  -> Run (SKY_ERROR a + META_CONTEXT a r) (Value a)
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
  :: forall r
   . forall a
   . Value a
  -> Spine a
  -> Run (SKY_ERROR a + META_CONTEXT a r) (Value a)
vApplySpine to spine@(Spine arguments) = case to of
  -- These two paths are here for performance reasons 
  VMetaApplication source meta originalSpine ->
    pure $ VMetaApplication source meta (originalSpine <> spine)
  VVariableApplication source var originalSpine ->
    pure $ VVariableApplication source var (originalSpine <> spine)
  VSourceAnnotation source inner ->
    vApplySpine inner spine
      <#> VSourceAnnotation source
  other -> Array.foldM vApply other arguments

vApplyMaskedeEnvironment
  :: forall a r
   . Env a
  -> Value a
  -> Mask
  -> Run (SKY_ERROR a + META_CONTEXT a r)
       (Value a)
vApplyMaskedeEnvironment (Env { scope }) to (Mask mask) =
  Array.zip scope mask
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
makeClosure :: forall r a. Term a -> Run (EVALUATION_ENV a r) (Closure a)
makeClosure term = environment <#> { term, env: _ } <#> Closure

-- | Evaluate a term under a given environment
evalWith
  :: forall r a
   . Env a
  -> Term a
  -> Run (SKY_ERROR a + META_CONTEXT a r) (Value a)
evalWith env = eval >>> runReaderAt _evaluationEnv env

-- | Evaluate a term
eval
  :: forall r a
   . Term a
  -> EvalM a r (Value a)
eval = case _ of
  Star source -> pure $ VStar source
  SourceAnnotation source inner -> eval inner
    <#> VSourceAnnotation source
  Var source index -> ado
    result <- lookupVar source index
    in VSourceAnnotation source result
  Application source lhs rhs -> do
    lhs <- eval lhs
    rhs <- eval rhs
    vApply lhs rhs
  Lambda source body -> ado
    closure <- makeClosure body
    in VLambda source closure
  -- TODO: cases like this could be run in paralle, to accumulate more errors
  Pi source domain codomain -> ado
    domain <- eval domain
    codomain <- makeClosure codomain
    in VPi source domain codomain
  Let source definition body -> do
    definition <- eval definition
    augumentEnv (extendEnv definition) $ eval body
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
force :: forall a r. Value a -> Run (SKY_ERROR a + META_CONTEXT a r) (Value a)
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
quoteSpine :: forall a r. a -> Term a -> Spine a -> QuoteM a r (Term a)
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
quote :: forall a r. Value a -> QuoteM a r (Term a)
quote = force >=> case _ of
  VStar source -> pure $ Star source
  VMetaApplication source meta spine -> quoteSpine source (Meta source meta) spine
  VVariableApplication source var spine -> do
    level <- getDepth
    quoteSpine source (Var source $ quoteIndex level var) spine
  VAssumptionApplication source type_ spine -> do
    type_ <- quote type_
    quoteSpine source type_ spine
  VPi source domain codomain -> do
    marker <- getDepthMarker source
    domain <- quote domain
    -- | TODO: mark the source as the source of the variable, not the source of the whole thing
    codomain <- applyClosure codomain marker
    codomain <- increaseDepth $ quote codomain
    pure $ Pi source domain codomain
  VLambda source body -> do
    marker <- getDepthMarker source
    body <- applyClosure body marker
    body <- increaseDepth $ quote body
    pure $ Lambda source body
  VSourceAnnotation source inner ->
    quote inner <#> SourceAnnotation source

-- | Normalize a term
normalForm
  :: forall a r
   . Term a
  -> Run (QUOTATION_ENV + META_CONTEXT a + SKY_ERROR a + EVALUATION_ENV a r) (Term a)
normalForm term = do
  (Env { scope }) <- environment
  let quotationEnv = QuoteEnv { depth: Level (Array.length scope) }
  eval term
    >>= quote
    # Reader.runReaderAt _quotationEnv quotationEnv

-- | Create a variable carrying the depth it came from as it's payload
getDepthMarker :: forall a r. a -> Run (QUOTATION_ENV r) (Value a)
getDepthMarker source = getDepth <#> \depth -> VVariableApplication source depth mempty

---------- Proxies
_evaluationEnv :: Proxy "evaluationEnv"
_evaluationEnv = Proxy

_quotationEnv :: Proxy "quotationEnv"
_quotationEnv = Proxy