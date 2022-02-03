module Sky.Language.Term where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Plus)
import Data.Array as Array
import Data.Debug (class Debug, debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

---------- Term-related types
-- | Variable name
newtype Name = Name String

-- | De brujin index
newtype Index = Index Int

-- | Unique meta id
newtype MetaVar = MetaVar Int

-- | Type describing all terms in the lang
data Term a
  = Pi a (Term a) (Term a) -- (a: A) -> B
  | Lambda a (Term a) -- \a -> b
  | Application a (Term a) (Term a) -- a b
  | Let a (Term a) (Term a) -- let a = A in B
  | Var a Index -- a
  | Star a -- *
  | Meta a MetaVar -- alpha
  | InsertedMeta a MetaVar Mask -- alpha a b c d...
  | SourceAnnotation a (Term a) -- internal branch used by evaluation to carry extra source data
  | Assumption a (Term a) -- assume type

-- | Alias for making certain type definition less ambiguours to the prgorammer
type SkyType = Term

---------- Evaluation-related types
newtype Spine a = Spine (Array (Value a))
-- | De-brujin level
newtype Level = Level Int
newtype Env a = Env
  { scope :: Array (Value a)
  }

newtype Mask = Mask (Array Boolean)

newtype Closure a = Closure
  { env :: Env a
  , term :: Term a
  }

newtype MetaContext a = MetaContext
  { metas :: HashMap MetaVar (Maybe (Value a))
  , metaNames :: HashMap MetaVar Name
  }

-- | Type representing the result of evaluating a Term
data Value a
  = VStar a
  -- TODO: might have to add more source data here
  | VMetaApplication a MetaVar (Spine a)
  | VVariableApplication a Level (Spine a)
  | VAssumptionApplication a (Value a) (Spine a)
  | VPi a (Value a) (Closure a)
  | VLambda a (Closure a)
  | VSourceAnnotation a (Value a)

-- | Type alias to make some type annotations less ambiguous to the programmer
type VType = Value

---------- Helpers
-- | Append a value to a context
extendEnv :: forall a. Value a -> Env a -> Env a
extendEnv extra (Env { scope }) = Env
  { scope: Array.cons extra scope
  }

-- | Append a value to a spine
extendSpine :: forall a. Spine a -> Value a -> Spine a
extendSpine (Spine arguments) extra = Spine
  (Array.snoc arguments extra)

-- | Append a value to a mask
extendMask :: Mask -> Boolean -> Mask
extendMask (Mask arguments) extra = Mask
  (Array.snoc arguments extra)

---------- Typeclass instances
derive newtype instance Semigroup (Spine a)
derive newtype instance Monoid (Spine a)

derive newtype instance Semigroup Mask
derive newtype instance Monoid Mask

derive newtype instance Eq MetaVar
derive newtype instance Hashable MetaVar
derive newtype instance Show MetaVar
derive newtype instance Debug MetaVar

derive newtype instance Eq Level
derive newtype instance Hashable Level
derive newtype instance Show Level
derive newtype instance Debug Level

derive newtype instance Semigroup (Env a)
derive newtype instance Monoid (Env a)

derive newtype instance Eq Name
derive newtype instance Hashable Name
derive newtype instance Show Name
derive newtype instance Debug Name

derive instance Newtype (MetaContext a) _

derive newtype instance Show Index
derive newtype instance Debug Index

derive newtype instance Show Mask
derive newtype instance Debug Mask

derive newtype instance Show a => Show (Env a)
derive newtype instance Show a => Show (Spine a)
derive newtype instance Show a => Show (Closure a)

derive newtype instance Debug a => Debug (Env a)
derive newtype instance Debug a => Debug (Spine a)

instance Debug a => Debug (Closure a) where
  debug a = genericDebug a

derive instance Generic (Value a) _
derive instance Generic (Term a) _
derive instance Generic (Closure a) _

instance Show a => Show (Value a) where
  show a = genericShow a

instance Show a => Show (Term a) where
  show a = genericShow a

instance Debug a => Debug (Value a) where
  debug (VSourceAnnotation _ inner) = debug inner
  debug a = genericDebug a

instance Debug a => Debug (Term a) where
  debug a = genericDebug a

derive instance Functor Term
derive instance Functor Env
derive instance Functor Closure
derive instance Functor Spine
derive instance Functor Value
derive instance Functor MetaContext

instance Alt MetaContext where
  alt (MetaContext a) (MetaContext b) = MetaContext
    { metas: HM.union a.metas b.metas
    , metaNames: HM.union a.metaNames b.metaNames
    }

instance Plus MetaContext where
  empty = MetaContext
    { metas: HM.empty
    , metaNames: HM.empty
    }