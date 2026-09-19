module Sky.Language.Ast where

import Prelude

import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Sky.Language.Term (Name)

---------- Types

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

---------- Typeclass instances
derive instance Generic (Ast a) _
instance Show a => Show (Ast a) where
  show a = genericShow a

instance Debug a => Debug (Ast a) where
  debug a = genericDebug a
