module Sky.Language.Error where

import Prelude

import Data.Debug (class Debug)
import Data.Generic.Rep (class Generic)
import Data.HashMap as HM
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Run (Run)
import Run.Except (Except, throwAt)
import Sky.Debug (showPretty)
import Sky.Language.Source (SourceMap, SourcePosition(..), SourceSpan(..))
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

-- | Type of all errors which can occur during elaboration
data ElaborationError a = ElabVarNotInScope
  { name :: String
  , source :: a
  }

-- | The type of all possible sky errors
data SkyError a
  = EvaluationError (EvaluationError a)
  | MetaError (MetaError a)
  | UnificationError (UnificationError a)
  | ElaborationError (ElaborationError a)

---------- Effect related stuff
type SKY_ERROR :: forall k. Type -> Row (k -> Type) -> Row (k -> Type)
type SKY_ERROR a r =
  ( skyError :: Except (SkyError a)
  | r
  )

---------- Source span data type
class Hashable a <= SourceSpot a where
  nowhere :: a
  nameOfLet :: a -> a
  piArgument :: a -> a
  lambdaArgument :: a -> a

indexOriginalBuffer :: forall a. Hashable a => String -> SourceMap a -> a -> Maybe String
indexOriginalBuffer originalText map key = HM.lookup key map
  >>= \(SourceSpan { start: SourcePosition from, end: SourcePosition to }) ->
    String.slice from.index (to.index - from.index) originalText

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

throwElaborationError
  :: forall a r o
   . ElaborationError a
  -> Run (SKY_ERROR a r) o
throwElaborationError = ElaborationError >>> throwAt _skyError

---------- Typeclass instances
derive instance Generic (ElaborationError a) _
derive instance Generic (MetaError a) _
derive instance Generic (UnificationError a) _
derive instance Generic (EvaluationError a) _
derive instance Generic (SkyError a) _

instance Show a => Show (ElaborationError a) where
  show = genericShow

instance Show a => Show (MetaError a) where
  show = genericShow

instance (Show a, Debug a) => Show (UnificationError a) where
  show (CannotUnify { lhs, rhs }) = "Cannot unify " <> showPretty lhs <> " with type " <> showPretty rhs
  show a = genericShow a

instance Show a => Show (EvaluationError a) where
  show = genericShow

instance (Debug a, Show a) => Show (SkyError a) where
  show = genericShow

---------- Proxies
_skyError :: Proxy "skyError"
_skyError = Proxy