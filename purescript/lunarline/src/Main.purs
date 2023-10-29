module Main where

import Prelude

import Data.Array.NonEmpty as NA
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (logShow)
import Lunarline.Ast (Case(..), Expression(..), callMany)
import Lunarline.Inline (emptyContext, execute, runInlineM)

myMap :: Expression
myMap = Lambda "h"
  $ Lambda "array"
  $ Match (Var "array")
  $
    NA.cons'
      ( Deconstruct "Cons" [ Named "x", Named "xs" ]
          /\ callMany (Constructor "Cons")
            [ Call (Var "h") (Var "x")
            , callMany (Var "map")
                [ Var "h", Var "xs" ]
            ]
      )
      [ Deconstruct "Nil" [] /\ Constructor "Nil"
      ]

myMap2 :: Expression
myMap2 = Let "map" myMap
  $ Lambda "f"
  $ Lambda "g"
  $ Lambda "list"
  $ callMany (Var "map") [ Var "g", callMany (Var "map") [ Var "f", Var "list" ] ]

main :: Effect Unit
main = do
  logShow myMap2
  logShow $ runInlineM emptyContext $ execute myMap2
