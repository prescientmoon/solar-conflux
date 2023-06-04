module Foo where

import Prelude 

class Foo a where
    foo :: a -> a -> a

instance Foo Int where
    foo a b = a - b
