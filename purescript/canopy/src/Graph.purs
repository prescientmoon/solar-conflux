module Canopy.Graph where

import Prelude

import Data.Array as Array
import Data.List (List(..))

newtype Node v = Node
  { adjacentTo :: Array Index
  , value :: v
  }

type Graph v = List (Node v)
type Index = Int

adjacentTo :: forall v. Node v -> Array Index
adjacentTo (Node {adjacentTo}) = adjacentTo

areAdjacent :: forall v. Index -> Index -> Graph v -> Boolean
areAdjacent first second Nil = false
areAdjacent first second whole@(Cons node graph)
  | first == second = false
  | first > second = areAdjacent second first whole
  | first == 0 = Array.elem second (adjacentTo node)
  | otherwise = areAdjacent (first - 1) (second - 1) graph
