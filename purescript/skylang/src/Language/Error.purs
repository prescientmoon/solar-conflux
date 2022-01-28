module Sky.Language.Error where

import Prelude
import Run (Run)
import Run.Except (Except, throwAt)
import Sky.Language.Term (Closure, MetaVar, Value)
import Type.Proxy (Proxy(..))

---------- Error-related types
data EvaluationError a
  = PiNotCallable
      { piSource :: a
      , piDomain :: Value a
      , piCodomain :: Closure a
      , argument :: Value a
      }
  | StarNotCallable
      { starSource :: a
      , argument :: Value a
      }

-- | Errors related to meta variables
data MetaError a = MetaNotInContext
  { meta :: MetaVar
  , source :: a
  }

data SkyError a
  = EvaluationError (EvaluationError a)
  | MetaError (MetaError a)

---------- Effect related stuff
type SKY_ERROR :: forall k. Type -> Row (k -> Type) -> Row (k -> Type)
type SKY_ERROR a r =
  ( skyError :: Except (SkyError a)
  | r
  )

throwMetaError
  :: forall a r o
   . MetaError a
  -> Run (SKY_ERROR a r) o
throwMetaError = MetaError >>> throwAt _skyError

throwEvaluationError
  :: forall a r o
   . EvaluationError a
  -> Run (SKY_ERROR a r) o
throwEvaluationError = EvaluationError >>> throwAt _skyError

---------- Proxies
_skyError :: Proxy "skyError"
_skyError = Proxy