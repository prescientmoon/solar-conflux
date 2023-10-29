-- | Vector2 utilies I keep using in a lot of my projects.
-- | I need to publish this as it's own package soon.
module Kombinator.Vector
  ( Vec2
  , Axis(..)
  , x
  , y
  , toTuple
  , fromTuple
  , other
  , indexByAxis
  , mapAxis
  , lmapAxis
  , rmapAxis
  , bimapAxis
  , buildFromAxis
  , greaterThan
  , smallerThan
  , origin
  , _insideVector
  , _x
  , _y
  , _axis
  , _otherAxis
  ) where

import Prelude

import Data.Lens (Lens', lens, over)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Lt, class Nat, D2, d0, d1)
import Data.Vec (Vec, vec2, (!!))
import Data.Vec as Vec

-- | Sized array with 2 elements
type Vec2 = Vec D2

-- | The origin of the coordinate system, with both elements at 0.
origin :: forall a. Semiring a => Vec2 a
origin = zero

-- | Get the first element of a vector
-- |
-- | Ex:
-- | ```purs
-- | x (vec2 0 1) == 0
-- | ```
x :: forall a. Vec2 a -> a
x = (_ !! d0)

-- | Get the second element of a vector
-- |
-- | Ex:
-- | ```purs
-- | y (vec2 0 1) == 1
-- | ```
y :: forall a. Vec2 a -> a
y = (_ !! d1)

-- | Convert a vec2 to a tuiple.
-- |
-- | Ex:
-- | ```purs
-- | toTuple origin == 0 /\ 0
-- | ```
toTuple :: forall a. Vec2 a -> a /\ a
toTuple vec = (vec !! d0) /\ (vec !! d1)

-- | Convert a tuple into a vec2
-- |
-- | Ex:
-- | ```purs
-- | fromTuple (0 /\ 0) = origin
-- | ```
fromTuple :: forall a. a /\ a -> Vec2 a
fromTuple = uncurry vec2

-- | Check if both elements of a vector are smaller than
-- | both elements of another vector.
smallerThan :: forall a. Ord a => Vec2 a -> Vec2 a -> Boolean
smallerThan a b = x a < x b && y a < y b

-- | Check if both elements of a vector are greater than
-- | both elements of another vector.
greaterThan :: forall a. Ord a => Vec2 a -> Vec2 a -> Boolean
greaterThan a b = x a > x b && y a > y b

---------- Stuff related to axis
-- | An Axis represents either the x axis or the y axis
data Axis = X | Y

-- | Get back the opposite axis of the one provided
-- |
-- | Ex:
-- | ```purs
-- | other X == Y
-- | other Y == X
-- | ```
other :: Axis -> Axis
other X = Y
other Y = X

-- | Lookup a vec2 by using an axis as the index
-- |
-- | Ex:
-- | ```purs
-- | indexByAxis X (vec2 0 1) == 0
-- | indexByAxis Y (vec2 0 1) == 1
-- | ```
indexByAxis :: forall a. Axis -> Vec2 a -> a
indexByAxis X = x
indexByAxis Y = y

-- | Construct a vector starting from a given axis.
-- | Similar to `vec2`, except the first argument
-- | is not always the x axis. Instead, the first argument
-- | can be either the X or the Y axis.
-- |
-- | You can think of this function as:
-- | ```purs
-- | buildFromAxis X = vec2 
-- | buildFromAxis Y = flip vec2 
-- | ```
-- |
-- | Ex:
-- | ```purs
-- | buildFromAxis X 0 1 == vec2 0 1 
-- | buildFromAxis Y 0 1 == vec2 1 0 
-- | ```
buildFromAxis :: forall a. Axis -> a -> a -> Vec2 a
buildFromAxis X a b = vec2 a b
buildFromAxis Y a b = vec2 b a

-- | Map over the value at the given axis
-- |
-- | Ex:
-- | ```purs
-- | mapAxis Y (_ + 1) origin == vec2 0 1
-- | ```
mapAxis :: forall a. Axis -> (a -> a) -> Vec2 a -> Vec2 a
mapAxis axis = over (_axis axis)

-- | Alias for `mapAxis`
lmapAxis :: forall a. Axis -> (a -> a) -> Vec2 a -> Vec2 a
lmapAxis = mapAxis

-- | Run a function over the opposite of the provided axis.
-- | EG: if the provided axis is X, run the function over the Y axis
-- | Ex:
-- | ```purs
-- | rmapAxis X (_ + 1) origin = vec2 0.0 1.0
-- | ```
rmapAxis :: forall a. Axis -> (a -> a) -> Vec2 a -> Vec2 a
rmapAxis = other >>> mapAxis

-- | Run 2 functions over the different axis of a vector.
-- | The first function is run over the provided axis,
-- | and the second function is run over the other axis
-- |
-- | Ex:
-- | ```purs
-- | bimapAxis Y (_ + 1) (_ - 1) origin == vec2 (-1) 1
-- | ```
bimapAxis :: forall a. Axis -> (a -> a) -> (a -> a) -> Vec2 a -> Vec2 a
bimapAxis axis f g = over (_axis axis) f >>> over (_otherAxis axis) g

---------- Lenses
-- | Similar to `ix`, but for vectors.
_insideVector :: forall a s i. Nat i => Lt i s => i -> Lens' (Vec s a) a
_insideVector index = lens get set
  where
  get vec = vec !! index
  set vec newX = Vec.updateAt index newX vec

-- | Focus on the first element of a vector
_x :: forall a. Lens' (Vec2 a) a
_x = _insideVector d0

-- | Focus on the second element of a vector
_y :: forall a. Lens' (Vec2 a) a
_y = _insideVector d1

-- | Focus on the element of a vector matching a given axis.
-- |
-- | Ex:
-- | ```purs
-- | f (_axis Y) == f _y 
-- | ```
_axis :: forall a. Axis -> Lens' (Vec2 a) a
_axis X = _x
_axis Y = _y

-- | Focus on the elemnt of a vector matching
-- | the opposite of a given axis.
-- |
-- | Ex:
-- | ```purs
-- | f (_axis Y) == f _x
-- | ```
_otherAxis :: forall a. Axis -> Lens' (Vec2 a) a
_otherAxis axis = _axis (other axis)
