module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)


main :: Effect Unit
main = log "hello world"

---------- Existentials
type ShowConstraint a = Show a => a

type Exists :: forall k. (k -> Type) -> Type
type Exists c = forall r. (forall a. c a -> r) -> r

type Showable = Exists ShowConstraint

mkShowable :: forall a. Show a => a -> Showable
mkShowable inner continue = continue inner

demo1 :: Showable
demo1 = mkShowable 1

demo2:: Showable
demo2 = mkShowable "a"

demo3:: Showable
demo3 = mkShowable true

