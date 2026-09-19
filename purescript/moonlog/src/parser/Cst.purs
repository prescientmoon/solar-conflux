module Moonlog.Parser.Cst where

import Prelude

import Data.Array (foldl)
import Data.Either (Either)
import Data.Nullable (Nullable)
import Data.Variant (Variant, match)
import Moonlog.Utils.Catch (catchErrors)

---------- Parsing stuff
newtype SourcePosition = SourcePosition { line :: Int, column :: Int }
newtype SourceSpan = SourceSpan { start :: SourcePosition, end :: SourcePosition }

type Token = { type :: String, text :: String, span :: SourceSpan }

newtype WithSpan a = WithSpan { value :: a, span :: SourceSpan }

type ParsingError = 
    { message :: String
    , offset :: Int
    , token :: 
        { type :: String
        , value :: String
        , text :: String
        , offset :: Int
        , lineBreaks :: Int
        , line :: Int
        , col :: Int
        , toString :: Unit -> String }}

---------- Helpers
withoutSpan :: forall a. WithSpan a -> a
withoutSpan (WithSpan { value }) = value

---------- Cst
newtype Pattern = Pattern
    { name :: WithSpan String
    , arguments :: Array Term
    }

type List = 
    { span :: SourceSpan
    , elements :: Array Term
    , tail :: Nullable Term }

type TermR = 
    ( pattern :: Pattern
    , var :: WithSpan String
    , natural :: WithSpan Int
    , list :: List )

newtype Term = Term (Variant TermR)

type Rule = 
    { head :: Pattern
    , body :: Array Pattern }

---------- Typeclass instances
derive instance eqPosition :: Eq SourcePosition

instance ordPosition :: Ord SourcePosition where
    compare (SourcePosition a) (SourcePosition b) = if a.line == b.line then compare a.column b.column else compare a.line b.line

instance semigroupSpan :: Semigroup SourceSpan where
    append (SourceSpan a) (SourceSpan b) = SourceSpan
        { start: min a.start b.start
        , end: max a.end b.end }

class HasSpan a where
    sourceSpan :: a -> SourceSpan

instance hasSpanWithSpan :: HasSpan (WithSpan a) where
    sourceSpan (WithSpan { span }) = span

instance sourceSpanId :: HasSpan SourceSpan where
    sourceSpan = identity

instance hasSpanPattern :: HasSpan Pattern where
    sourceSpan (Pattern { name, arguments }) = foldl (<>) (sourceSpan name) (sourceSpan <$> arguments)

instance hasSpanTerm :: HasSpan Term where
    sourceSpan (Term t) = match 
        { pattern: (sourceSpan :: Pattern -> _)
        , natural: (sourceSpan :: WithSpan Int -> _) 
        , var: (sourceSpan :: WithSpan String -> _)
        , list: _.span 
        } t

instance showSourceSpan :: Show SourceSpan where
    show _ = "{...}"

---------- FFI
foreign import parseImpl :: String -> Array Rule
foreign import parseConstructorImpl :: String -> Pattern

parse :: String -> Either ParsingError (Array Rule)
parse = catchErrors parseImpl

parseConstructor :: String -> Either ParsingError Pattern
parseConstructor = catchErrors parseConstructorImpl
