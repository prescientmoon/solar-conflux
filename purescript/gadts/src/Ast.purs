module Ast where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))


newtype OutputNumber :: forall k. Type -> k -> Type
newtype OutputNumber number boolean = OutputNumber number

newtype OutputBoolean :: forall k. k -> Type -> Type
newtype OutputBoolean number boolean = OutputBoolean boolean

data Ast output
    = Number (output Number Void)
    | Boolean (output Void Number)
    | Add (output (Ast output /\ Ast output) Void)
    | And (output Void (Ast output /\ Ast output))

addition :: Ast OutputNumber
addition = Add $ OutputNumber ((Number $ OutputNumber 0.0) /\ (Number $ OutputNumber 1.1))

class ExtractFromOutput output where
    extractLeft :: forall left. output left Void -> left
    wrapLeft :: forall left right. left -> output left right


interpret :: forall output. Semiring (output Number Boolean) => ExtractFromOutput output => Ast output -> output Number Boolean
interpret (Add inner) = interpret left + interpret right
    where
    left /\ right = extractLeft inner
interpret (Number n) = wrapLeft $ extractLeft n
interpret a = interpret a