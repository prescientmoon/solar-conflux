module Sky.Language.Elaboration where

import Prelude

import Data.Foldable (for_)
import Data.HashMap as HM
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Run.Supply (generate)
import Safe.Coerce (coerce)
import Sky.Language.Ast (Ast(..))
import Sky.Language.Effects (ElabM, ElaborationContext(..), augumentElaborationContext, augumentEnv, environment, extendPrintScope, generateVar, getDepth, getElaborationContext, increaseDepth)
import Sky.Language.Error (class SourceSpot, ElaborationError(..), lambdaArgument, nameOfLet, nowhere, piArgument, throwElaborationError)
import Sky.Language.Eval (applyClosure, eval, force, getDepthMarker, makeClosure, quote, quoteIndex)
import Sky.Language.Log (checking, inferring)
import Sky.Language.MetaVar (freshMeta, nameMeta)
import Sky.Language.PatternUnification (unify)
import Sky.Language.Term (Closure, Name(..), Term(..), VType, Value(..), extendEnv, extendMask)

---------- Helpers
-- | Extend the scope with a new bound variable
bindVariable :: forall a r. a -> Name -> Value a -> ElabM a r ~> ElabM a r
bindVariable source name _type compute = do
  depth <- getDepth
  marker <- getDepthMarker source
  compute
    # augumentElaborationContext (modifyElaborationContext depth)
    # augumentEnv (extendEnv marker)
    # extendPrintScope [ coerce name ]
    # increaseDepth
  where
  modifyElaborationContext depth (ElaborationContext context) =
    ElaborationContext
      { types: HM.insert name (depth /\ _type) context.types
      , mask: extendMask context.mask true
      }

-- | Extend the context with a definition (aka a variable we know the value of)
defineVariable
  :: forall a r
   . a
  -> Name
  -> Value a
  -> VType a
  -> ElabM a r ~> ElabM a r
defineVariable suorce name value _type compute = do
  depth <- getDepth
  compute
    # augumentElaborationContext (modifyElaborationContext depth)
    # augumentEnv (extendEnv value)
    # extendPrintScope [ coerce name ]
    # increaseDepth
  where
  modifyElaborationContext depth (ElaborationContext context) =
    ElaborationContext
      { types: HM.insert name (depth /\ _type) context.types
      , mask: extendMask context.mask false
      }

-- | Create a closure from a value
valueToClosure :: forall a r. SourceSpot a => a -> Value a -> ElabM a r (Closure a)
valueToClosure source value = do
  name <- generateVar
  term <- extendPrintScope [ name ] $ increaseDepth $ quote value
  makeClosure source term

-- | Create a meta and apply the current mask to it
freshMetaTerm :: forall a r. a -> Maybe Name -> ElabM a r (Term a)
freshMetaTerm source name = do
  meta <- freshMeta
  (ElaborationContext { mask }) <- getElaborationContext
  -- Remembers hole name, as long as the name is not Nothing
  for_ name \name -> nameMeta name meta
  pure $ InsertedMeta source meta mask

---------- Implementation
-- | Check an expression has a given type
check :: forall a r. SourceSpot a => Ast a -> Value a -> ElabM a r (Term a)
check expression _type = checking expression _type do
  _type <- force _type
  check' expression _type

-- | Same as check, but assumes the type has been forced.
check' :: forall a r. SourceSpot a => Ast a -> Value a -> ElabM a r (Term a)
check' = case _, _ of
  -- Here not to pollute the logs:
  EStar source, VStar _ -> pure $ Star source
  EHole source name, other -> do
    freshMetaTerm source name
  ELambda source name body, VPi piSource domain codomain -> do
    marker <- getDepthMarker $ piArgument piSource
    codomain <- applyClosure codomain marker
    body <- bindVariable (lambdaArgument source) name domain $ check body codomain
    pure $ Lambda source body
  ELet source name value body, type_ -> do
    value /\ typeofValue <- infer value
    vValue <- eval value
    body <- defineVariable (nameOfLet source) name vValue typeofValue $ check body type_
    pure $ Let source value body
  value, expected -> do

    value' /\ inferred <- infer value
    env <- environment
    expected <- quote expected
    expected <- eval expected
    unify expected inferred
    pure value'

infer :: forall a r. SourceSpot a => Ast a -> ElabM a r (Term a /\ VType a)
infer a = inferring a case a of
  EStar source -> pure $ (Star source /\ VStar source)
  ELambda source name body -> do
    insertedMeta <- freshMetaTerm (lambdaArgument source) Nothing
    vMeta <- eval insertedMeta
    body /\ inferred <- bindVariable (lambdaArgument source) name vMeta
      $ infer body
    codomain <- valueToClosure (lambdaArgument source) inferred
    let lambda = Lambda source body
    let inferred = VPi source vMeta codomain
    pure $ lambda /\ inferred
  EPi source name domain codomain -> do
    domain <- check domain (VStar nowhere)
    vDomain <- eval domain
    codomain <- bindVariable (piArgument source) name vDomain
      $ check codomain (VStar nowhere)
    pure $ (Pi source domain codomain) /\ VStar source
  EVar source name -> do
    (ElaborationContext { types }) <- getElaborationContext
    case HM.lookup name types of
      Just (origin /\ type_) -> do
        depth <- getDepth
        let index = quoteIndex depth origin
        pure (Var source index /\ type_)
      Nothing -> throwElaborationError $ ElabVarNotInScope
        { name: coerce name, source }
  EAnnotation source value type_ -> do
    type_ <- check type_ (VStar nowhere)
    type_ <- eval type_
    value <- check value type_
    pure $ (value /\ type_)
  ELet source name value body -> do
    value /\ typeofValue <- infer value
    vValue <- eval value
    (body /\ inferred) <- defineVariable (nameOfLet source) name vValue typeofValue $ infer body
    let let_ = Let source value body
    pure (let_ /\ inferred)
  EHole source name -> do
    typeofMeta <- freshMetaTerm source Nothing >>= eval
    meta <- freshMetaTerm source name
    pure (meta /\ typeofMeta)
  EApplication source function argument -> do
    function /\ typeofFunction <- infer function

    -- Ensure the argument has type PI
    domain /\ codomain <- case typeofFunction of
      VPi source domain codomain -> pure (domain /\ codomain)
      other -> do
        uniqueId <- generate
        let generatedName = Name ("?" <> show uniqueId)
        -- TODO: fix sources
        -- TODO: consider reporting the type of the domain
        domain <- freshMetaTerm nowhere Nothing >>= eval
        codomain <- bindVariable nowhere generatedName domain
          -- TODO: the closure here will have the wrong source
          (freshMetaTerm source Nothing >>= makeClosure source)
        unify typeofFunction (VPi source domain codomain)
        pure (domain /\ codomain)
    argument <- check argument domain
    vArgument <- eval argument
    inferred <- applyClosure codomain vArgument
    pure (Application source function argument /\ inferred)
  EAssumption source type_ -> do
    type_ <- check type_ (VStar nowhere)
    vType_ <- eval type_
    pure $ (Assumption source type_) /\ vType_
