module Sky.Language.Error where

import Prelude

import Run (Run)
import Run.Except (Except, throwAt)
import Sky.Language.Term (Closure, Index, Level, MetaVar, Spine, Value)
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
  | VarNotInScope
      { source :: a
      , index :: Index
      }

-- | Errors related to meta variables
data MetaError a
  = MetaNotInContext
      { meta :: MetaVar
      , source :: a
      }
  | NonLinearPattern
      { varSource :: a
      , spine :: Spine a
      }
  | PatternArgumentIsNotVar
      { argument :: Value a
      , spine :: Spine a
      }

-- | Errors which can occur during unification
data UnificationError a
  = FailedOccursCheck
      { meta :: MetaVar
      , occursAt :: a
      , occursIn :: Value a
      }
  | EscapingVariable
      { variable :: Level
      -- | TODO: handle source annotations
      , source :: a
      , whileRenaming :: Value a
      }

  | CannotUnify
      { lhs :: Value a
      , rhs :: Value a
      }

data SkyError a
  = EvaluationError (EvaluationError a)
  | MetaError (MetaError a)
  | UnificationError (UnificationError a)

---------- Effect related stuff
type SKY_ERROR :: forall k. Type -> Row (k -> Type) -> Row (k -> Type)
type SKY_ERROR a r =
  ( skyError :: Except (SkyError a)
  | r
  )

---------- Helpers
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

throwUnificationError
  :: forall a r o
   . UnificationError a
  -> Run (SKY_ERROR a r) o
throwUnificationError = UnificationError >>> throwAt _skyError

---------- Proxies
_skyError :: Proxy "skyError"
_skyError = Proxy