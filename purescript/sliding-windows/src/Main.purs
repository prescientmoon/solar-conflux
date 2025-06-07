module Main where

import Prelude
import Data.List (List(..))
import Data.List as List
import Effect (Effect)
import Effect.Console (logShow)

-- Computes sliding windows in linear complexity
sliding :: forall a. Monoid a => Int -> List a -> List a
sliding k l
  | List.length l < k = Nil
  | otherwise =
      List.zipWith (<>)
        (List.scanr (<>) mempty $ List.take (k - 1) l)
        (List.scanl (<>) mempty $ List.slice (k - 1) (2 * k - 1) l)
        <> sliding k (List.drop (k - 1) l)

main :: Effect Unit
main = do
  logShow (sliding 3 $ List.fromFoldable [ "a", "b", "c", "d", "e", "f", "g", "h", "i" ])
  logShow (sliding 2 $ List.fromFoldable [ "a", "b" ])

