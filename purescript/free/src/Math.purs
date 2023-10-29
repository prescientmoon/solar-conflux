module Math where

import Prelude

import Control.Monad.Free (Free, liftF, resume)
import Data.Either (Either(..))

data MathF a 
    = Add Int Int (Int -> a)
    | Multiply Int Int (Int -> a)

type Math = Free MathF

---------- Helpers
eval :: Math Int -> Int
eval = resume >>> case _ of
    Right result -> result
    Left (Add l r cb) -> eval $ cb $ l + r
    Left (Multiply l r cb) -> eval $ cb $ l * r

seven :: Math Int
seven = do
    four <- multiply_ 2 2
    add_ four 3

---------- Free boilerplate
add_ :: Int -> Int -> Math Int
add_ l r = liftF (Add l r identity)

multiply_ :: Int -> Int -> Math Int
multiply_ l r = liftF (Multiply l r identity)

--------- Typeclass instances
derive instance functorMath :: Functor MathF