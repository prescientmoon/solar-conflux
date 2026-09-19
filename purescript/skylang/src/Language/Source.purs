module Sky.Language.Source where

import Prelude

import Data.Function (on)
import Data.HashMap (HashMap)
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype

---------- Base type
newtype SourcePosition = SourcePosition { line :: Int, column :: Int, index :: Int }
newtype SourceSpan = SourceSpan { start :: SourcePosition, end :: SourcePosition }
type WithSpan a =
  { value :: a
  , span :: SourceSpan
  }

type SourceMap a = HashMap a SourceSpan

---------- Typeclass instances
derive instance Newtype SourceSpan _
derive instance Newtype SourcePosition _
derive instance Eq SourceSpan
derive instance Eq SourcePosition

instance Ord SourcePosition where
  compare = on compare (Newtype.unwrap >>> _.index)

instance Semigroup SourceSpan where
  append (SourceSpan a) (SourceSpan b) = SourceSpan
    { start: min a.start b.start
    , end: max a.end b.end
    }