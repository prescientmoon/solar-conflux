module PF.Core.Data.Cst where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isAlpha)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Tuple.Nested ((/\))
import PF.ParsingCodec.Parser as PC
import Safe.Coerce (coerce)

newtype VarName = VarName String

data Expression
  = Lambda VarName Expression
  | Var VarName
  | Let VarName Expression Expression
  | Call Expression Expression
  | Pi
      { domain :: Expression
      , codomain :: Expression
      , var :: Maybe VarName
      }
  | Annotate
      { value :: Expression
      , type_ :: Expression
      }
  | Type

-- Explicit syntax:
{- 
- foo
- lam x -> ...
- let a = b in c
- f[a]
- pi x : ty -> body
- x :: y ???
- *
-}
varName :: PC.ParsingCodec VarName VarName
varName = dimap coerce coerce $ PC.takeWhile isAlpha

codec_
  :: PC.ParsingCodec Expression Expression
  -> PC.ParsingCodec Expression Expression
codec_ codec =
  Array.fold
    [ cType
    , cLambda
    ]
  where
  cType = PC.string "*" `PC.withConstantR` Type
  cLambda = PC.dimapMaybe
    translateL
    translateR
    ( PC.tuple
        ( PC.surround
            (PC.string "\\")
            varName
            (PC.string "->")
        )
        codec
    )
    where
    translateR (var /\ expr) = Lambda var expr
    translateL = case _ of
      Lambda var expr -> Just (var /\ expr)
      _ -> Nothing

---------- Typeclass instances
derive instance Eq VarName
derive newtype instance Hashable VarName

derive instance Eq Expression

