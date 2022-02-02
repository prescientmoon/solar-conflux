module Sky.Language.Ast where

import Prelude

import Data.Foldable (for_)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Run (Run)
import Run.Reader (Reader)
import Run.Reader as Reader
import Run.Supply (SUPPLY, generate)
import Safe.Coerce (coerce)
import Sky.Language.Error (class SourceSpot, ElaborationError(..), SKY_ERROR, lambdaArgument, nameOfLet, nowhere, piArgument, throwElaborationError)
import Sky.Language.Eval (EVALUATION_ENV, QUOTATION_ENV, applyClosure, augumentEnv, eval, force, getDepth, getDepthMarker, increaseDepth, makeClosure, quote, quoteIndex)
import Sky.Language.MetaVar (META_CONTEXT, freshMeta, nameMeta)
import Sky.Language.PatternUnification (unify)
import Sky.Language.Term (Closure, Level, Mask, Name(..), Term(..), VType, Value(..), extendEnv, extendMask)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

---------- Types
-- | Context required by elaboration
newtype ElaborationContext a = ElaborationContext
  { types :: HashMap Name (Level /\ Value a)
  , mask :: Mask
  }

-- | Types of terms before elaboration
data Ast a
  = EVar a Name
  | ELambda a Name (Ast a)
  | EPi a Name (Ast a) (Ast a)
  | EApplication a (Ast a) (Ast a)
  | ELet a Name (Ast a) (Ast a)
  | EAnnotation a (Ast a) (Ast a)
  | EStar a
  | EHole a (Maybe Name)
  | EAssumption a (Ast a)

---------- Effects
-- | Context required for elaboration to take place in
type ELABORATION_CONTEXT a r =
  ( elaborationContext :: Reader (ElaborationContext a)
  | r
  )

-- | Base monad containing all the effects required for evaluation to compute
type ElabM a r = Run
  ( ELABORATION_CONTEXT a + META_CONTEXT a
      + SKY_ERROR a
      + QUOTATION_ENV
      + EVALUATION_ENV a
      + SUPPLY Int r
  )

-- | Run a computation which makes use of an elaboration context. 
runElaborationContext :: forall a r. Run (ELABORATION_CONTEXT a r) ~> Run r
runElaborationContext = Reader.runReaderAt _elaborationContext $ ElaborationContext
  { mask: mempty
  , types: HM.empty
  }

-- | Expose the current elaboration context
getElaborationContext :: forall a r. Run (ELABORATION_CONTEXT a r) (ElaborationContext a)
getElaborationContext = Reader.askAt _elaborationContext

-- | Run a computation in a modified version of the current context
augumentElaborationContext
  :: forall a r
   . (ElaborationContext a -> ElaborationContext a)
  -> Run (ELABORATION_CONTEXT a r) ~> Run (ELABORATION_CONTEXT a r)
augumentElaborationContext = Reader.localAt _elaborationContext

---------- Helpers

-- | Extend the scope with a new bound variable
bindVariable :: forall a r. a -> Name -> Value a -> ElabM a r ~> ElabM a r
bindVariable source name _type compute = do
  depth <- getDepth
  marker <- getDepthMarker source
  compute
    # augumentElaborationContext (modifyElaborationContext depth)
    # augumentEnv (extendEnv marker)
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
    # increaseDepth
  where
  modifyElaborationContext depth (ElaborationContext context) =
    ElaborationContext
      { types: HM.insert name (depth /\ _type) context.types
      , mask: extendMask context.mask false
      }

-- | Create a closure from a value
valueToClosure :: forall a r. SourceSpot a => Value a -> ElabM a r (Closure a)
valueToClosure value = do
  term <- increaseDepth $ quote value
  makeClosure term

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
check expression _type = do
  _type <- force _type
  check' expression _type

-- | Same as check, but assumes the type has been forced.
check' :: forall a r. SourceSpot a => Ast a -> Value a -> ElabM a r (Term a)
check' = case _, _ of
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
    value /\ inferred <- infer value
    unify expected inferred
    pure value

infer :: forall a r. SourceSpot a => Ast a -> ElabM a r (Term a /\ VType a)
infer = case _ of
  EStar source -> pure $ (Star source /\ VStar source)
  ELambda source name body -> do
    insertedMeta <- freshMetaTerm (lambdaArgument source) Nothing
    vMeta <- eval insertedMeta
    body /\ inferred <- bindVariable (lambdaArgument source) name vMeta
      $ infer body
    codomain <- valueToClosure inferred
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
          (freshMetaTerm source Nothing >>= makeClosure)
        pure (domain /\ codomain)

    argument <- check argument domain
    vArgument <- eval argument
    inferred <- applyClosure codomain vArgument
    pure (Application source function argument /\ inferred)
  EAssumption source type_ -> do
    type_ <- check type_ (VStar nowhere)
    vType_ <- eval type_
    pure $ (Assumption source type_) /\ vType_

---------- Proxies
_elaborationContext :: Proxy "elaborationContext"
_elaborationContext = Proxy