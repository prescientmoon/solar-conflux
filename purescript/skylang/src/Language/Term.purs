module Sky.Language.Term where

import Prelude

import Data.Array as Array
import Data.HashMap (HashMap)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

---------- Term-related types
newtype Index = Index Int
newtype MetaVar = MetaVar Int

data Term a
  = Pi a (Term a) (Term a) -- (a: A) -> B
  | Lambda a (Term a) -- \a -> b
  | Application a (Term a) (Term a) -- a b
  | Let a (Term a) (Term a) -- let a = A in B
  | Annotation a { term :: Term a, type :: Term a } -- a :: A
  | Var a Index -- a
  | Star a -- *
  | Meta a MetaVar -- alpha
  | InsertedMeta a MetaVar Mask -- alpha a b c d...
  | SourceAnnotation a (Term a) -- internal branch used by evaluation to carry extra source data

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
  }

data Value a
  = VStar a
  -- TODO: might have to add more source data here
  | VMetaApplication a MetaVar (Spine a)
  | VVariableApplication a Level (Spine a)
  | VPi a (Value a) (Closure a)
  | VLambda a (Closure a)
  | VSourceAnnotation a (Value a)

---------- Helpers
-- | Append a value to a context
extendEnv :: forall a. Value a -> Env a -> Env a
extendEnv extra (Env { scope }) = Env
  { scope: Array.snoc scope extra
  }

-- | Append a value to a spine
extendSpine :: forall a. Spine a -> Value a -> Spine a
extendSpine (Spine arguments) extra = Spine
  (Array.snoc arguments extra)

---------- Effect-related things

---------- Typeclass instances
derive newtype instance Semigroup (Spine a)
derive newtype instance Monoid (Spine a)

derive newtype instance Eq MetaVar
derive newtype instance Hashable MetaVar

derive newtype instance Eq Level
derive newtype instance Hashable Level

derive newtype instance Semigroup (Env a)
derive newtype instance Monoid (Env a)

derive instance Newtype (MetaContext a) _