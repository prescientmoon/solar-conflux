module Sky.Language.Eval where

import Prelude

import Data.Array as Array
import Run (Run)
import Sky.Language.Error (EVALUATION_ERROR, EvaluationError(..), throwEvaluationError)
import Sky.Language.Term (Closure(..), Env, Spine(..), Term, Value(..), extendEnv, extendSpine)
import Unsafe.Coerce (unsafeCoerce)

---------- Effect-related stuff

---------- Helpers
applyClosure
  :: forall r a
   . Closure a
  -> Value a
  -> Run (EVALUATION_ERROR a r) (Value a)
applyClosure (Closure { env, term }) argument =
  eval (extendEnv env argument) term

vApply
  :: forall r
   . forall a
   . Value a
  -> Value a
  -> Run (EVALUATION_ERROR a r) (Value a)
vApply f a = case f of
  VLambda _ body -> applyClosure body a
  VMetaApplication source meta spine ->
    pure $ VMetaApplication source meta (extendSpine spine a)
  VVariableApplication source var spine ->
    pure $ VVariableApplication source var (extendSpine spine a)
  VPi source domain codomain ->
    throwEvaluationError $ PiNotCallable
      { piDomain: domain, piCodomain: codomain, piSource: source, argument: a }
  VStar source -> throwEvaluationError
    $ StarNotCallable { starSource: source, argument: a }

vApplySpine
  :: forall r
   . forall a
   . Value a
  -> Spine a
  -> Run (EVALUATION_ERROR a r) (Value a)
vApplySpine to spine@(Spine arguments) = case to of
  -- These two paths are here for performance reasons 
  VMetaApplication source meta originalSpine ->
    pure $ VMetaApplication source meta (originalSpine <> spine)
  VVariableApplication source var originalSpine ->
    pure $ VVariableApplication source var (originalSpine <> spine)
  other -> Array.foldM
    vApply
    other
    arguments

eval
  :: forall r a
   . Env a
  -> Term a
  -> Run (EVALUATION_ERROR a r) (Value a)
eval = unsafeCoerce