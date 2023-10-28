module PF.ParsingCodec.Parser where

import Prelude hiding ((*>), (<*))

import Control.Alternative (guard, (<|>))
import Control.Lazy (class Lazy)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))

data ParsingCodec i o = ParsingCodec
  (String -> Maybe (String /\ o))
  (i -> Maybe String)

string :: String -> ParsingCodec Unit String
string target = ParsingCodec decode encode
  where
  encode _ = Just target
  decode input = do
    let
      { before, after } = String.splitAt
        (String.length target)
        input

    guard (target == before)
    pure (after /\ target)

takeWhile :: (String.CodePoint -> Boolean) -> ParsingCodec String String
takeWhile predicate = ParsingCodec decode encode
  where
  encode = Just
  decode input = do
    let before = String.takeWhile predicate input
    let after = String.drop (String.length before) input
    pure (after /\ before)

ws :: ParsingCodec Unit String
ws = ParsingCodec decode encode
  where
  encode _ = Just " "
  decode input = do
    let
      spaces = String.takeWhile
        (_ == String.codePointFromChar ' ')
        input

    let after = String.drop (String.length spaces) input
    pure (after /\ spaces)

dimapMaybe
  :: forall i o a b
   . (a -> Maybe i)
  -> (o -> b)
  -> ParsingCodec i o
  -> ParsingCodec a b
dimapMaybe mapI mapO (ParsingCodec decode encode) = ParsingCodec
  (\input -> decode input <#> map mapO)
  (mapI >=> encode)

wss :: ParsingCodec Unit String
wss = ParsingCodec decode encode
  where
  encode _ = Just ""
  decode input = do
    let
      spaces = String.takeWhile
        (_ == String.codePointFromChar ' ')
        input

    guard (spaces /= "")

    let after = String.drop (String.length spaces) input
    pure (after /\ spaces)

tuple
  :: forall a b c d
   . ParsingCodec a b
  -> ParsingCodec c d
  -> ParsingCodec (a /\ c) (b /\ d)
tuple (ParsingCodec decodeL encodeL) (ParsingCodec decodeR encodeR) = ParsingCodec decode encode
  where
  encode (inputL /\ inputR) = ado
    a <- encodeL inputL
    b <- encodeR inputR
    in (a <> b)

  decode input = do
    input /\ left <- decodeL input
    remaining /\ right <- decodeR input
    pure $ remaining /\ (left /\ right)

ignoreLeft
  :: forall a b c
   . ParsingCodec Unit a
  -> ParsingCodec b c
  -> ParsingCodec b c
ignoreLeft (ParsingCodec decodeL encodeL) (ParsingCodec decodeR encodeR) = ParsingCodec decode encode
  where
  encode input = ado
    a <- encodeL unit
    b <- encodeR input
    in a <> b

  decode input = do
    input /\ _ <- decodeL input
    remaining /\ right <- decodeR input
    pure $ remaining /\ right

ignoreRight
  :: forall a b c
   . ParsingCodec b c
  -> ParsingCodec Unit a
  -> ParsingCodec b c
ignoreRight (ParsingCodec decodeL encodeL) (ParsingCodec decodeR encodeR) = ParsingCodec decode encode
  where
  encode input = ado
    a <- encodeL input
    b <- encodeR unit
    in a <> b

  decode input = do
    input /\ left <- decodeL input
    remaining /\ _ <- decodeR input
    pure $ remaining /\ left

infixl 4 ignoreRight as <*
infixl 4 ignoreLeft as *>

surround
  :: forall l r i o
   . ParsingCodec Unit l
  -> ParsingCodec i o
  -> ParsingCodec Unit r
  -> ParsingCodec i o
surround left middle right = left *> middle <* right

withConstantL
  :: forall o c
   . Eq c
  => c
  -> ParsingCodec Unit o
  -> ParsingCodec c c
withConstantL inner (ParsingCodec decode encode) = ParsingCodec
  decode'
  encode'
  where
  decode' input = decode input <#> map (const inner)
  encode' output = do
    guard (output == inner)
    encode unit

withConstantR
  :: forall o c
   . Eq c
  => ParsingCodec Unit o
  -> c
  -> ParsingCodec c c
withConstantR = flip withConstantL

infixl 4 withConstantL as <$
infixl 4 withConstantR as $>

decode :: forall i o. ParsingCodec i o -> (String -> Maybe (String /\ o))
decode (ParsingCodec r _) = r

encode :: forall i o. ParsingCodec i o -> (i -> Maybe String)
encode (ParsingCodec _ l) = l

---------- Typeclass instances
instance Profunctor ParsingCodec where
  dimap mapI mapO (ParsingCodec decode encode) = ParsingCodec
    (\input -> decode input <#> map mapO)
    (mapI >>> encode)

instance Semigroup (ParsingCodec i o) where
  append (ParsingCodec d e) (ParsingCodec d' e') =
    ParsingCodec decode encode
    where
    decode i = d i <|> d' i
    encode o = e o <|> e' o

instance Monoid (ParsingCodec s r) where
  mempty = ParsingCodec (const Nothing) (const Nothing)

instance Lazy (ParsingCodec i o) where
  defer mkCodec = ParsingCodec
    (\i -> decode (mkCodec unit) i)
    (\o -> encode (mkCodec unit) o)
