module Moontorio.Ord.Extra (Side, left, right, OrderedArray, binarySearch) where

import Prelude

import Data.Array (length, unsafeIndex)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

type OrderedArray = Array
newtype Side = Side Boolean

left :: Side
left = Side false

right :: Side
right = Side true

binarySearch :: forall a. (Int -> a -> Side) -> OrderedArray a -> Maybe Int
binarySearch f arr = unsafePartial $ findImpl 0 (length arr)
    where
    findImpl :: Partial => _
    findImpl start length | length == 0 = Nothing
                          | length == 1 = Just start
                          | otherwise   = do
        let middle = start + length / 2
        let element = unsafeIndex arr middle
        if f middle element == left then 
            findImpl start (middle - start)
        else 
            findImpl middle (length + start - middle)

---------- Typeclass instances 
derive instance eqSide :: Eq Side