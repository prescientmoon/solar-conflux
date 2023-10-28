module Main where

import Prelude

import Compose as Compose
import Data.Exists (Exists)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Unsafe.Coerce (unsafeCoerce)


c :: Int -> Int
c = Compose.do
    add2
    double

add2 :: Int -> Int
add2 = (+) 2

double :: Int -> Int
double = (*) 2

newtype I a = I a
type EI = Exists I

type Showable f = forall result. (forall r. Show r => f r -> result) -> result

mkShowable :: forall a f. Show a => f a -> Showable f
mkShowable a f = f a

test :: String
test = do
  I something <- mkShowable $ I 0
  show something
  where
  bind :: forall f result. Showable f -> (forall r. Show r => f r -> result) -> result 
  bind e f = e f

  magicShow :: forall r. r -> String
  magicShow = unsafeCoerce

main :: Effect Unit
main = do
  logShow $ c 3
