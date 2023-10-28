module Slice where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

newtype Slice a 
  = Slice 
    { array :: Array a
    , at :: Int
    , length :: Int }

uncons :: forall a. Slice a -> Maybe { head :: a, tail :: Slice a }
uncons (Slice { length, at, array }) 
    = if length == 0 
      then Nothing
      else Just 
        { head: unsafePartial $ Array.unsafeIndex array at
        , tail: Slice { length: length - 1, at: at + 1, array } }

uncons' :: forall a r. Slice a -> { nil :: r, cons :: a -> Slice a -> r } -> r
uncons' (Slice { length, at, array }) cases
    = if length == 0
        then cases.nil
        else cases.cons 
            (unsafePartial $ Array.unsafeIndex array at)
            (Slice { length: length - 1, at: at + 1, array })

fromArray :: forall a. Array a -> Slice a
fromArray array = Slice { array, at: 0, length: Array.length array }