-- | Lambda calculus ast
module Ast where


import Data.Symbol (SProxy(..))
import Num (class NAreEqual, NProxy(..), Succ, Zero, kind Num)
import Num as Nat
import Ordering (class OrdsAreEqual)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Ordering (GT, LT, kind Ordering)
import Prim.Symbol (class Append, class Compare, class Cons)
import Type.Data.Boolean (class And, class If, class Not)

foreign import kind Ast
foreign import data Var :: Symbol -> Ast
foreign import data Call :: Ast -> Ast -> Ast
foreign import data Lambda :: Symbol -> Ast -> Ast

foreign import kind Left 
foreign import kind Right

foreign import kind Either
foreign import data Left :: Symbol -> Either
foreign import data Right :: Right -> Either

foreign import data RightUnit_ :: Right
foreign import data RightSymbol_ :: Symbol -> Right
foreign import data RightAst :: Ast -> Right

type RightSymbol a = Right (RightSymbol_ a)
type RightUnit = Right RightUnit_

foreign import kind ParsingResult
foreign import data ParsingResult :: Either -> Symbol -> ParsingResult

data PRProxy (a :: ParsingResult) = PRProxy
data EProxy (a :: Either) = EProxy

-------------- Typeclasses
-- Conditionals
class EIf (bool :: Boolean)
    (onTrue :: Either)
    (onFalse :: Either)
    (output :: Either) 
    | bool onTrue onFalse -> output

instance eifTrue :: EIf True onTrue onFalse onTrue
instance eifFalse :: EIf False onTrue onFalse onFalse

-- Merge 2 eithers
class MergeEithers (a :: Either) (b :: Either) (c :: Either) | a b -> c

instance mergeEithersLeft :: MergeEithers (Left err) right (Left err)
else instance mergeEithersLeft' :: MergeEithers left (Left err) (Left err)
else instance mergeEithersRight :: MergeEithers left right right

-- Check if 2 symbols are equal
class SymbolsAreEqual (a :: Symbol) (b :: Symbol) (result :: Boolean) | a b -> result

instance saeT :: SymbolsAreEqual a a True
else instance saeF :: SymbolsAreEqual a b False

-- Measure length of string
class Length (input :: Symbol) (output :: Num) | input -> output

instance lengthEmpty :: Length "" Zero
else instance lengthCons ::
    ( Cons head tail input
    , Length tail length
    ) => Length input (Succ length)

-- Actual parsing
class ParseAst (input :: Symbol) (result :: Ast) | input -> result

-- Parses a literal thing.
class ParseLiteral (lit :: Symbol) (input :: Symbol) (result :: ParsingResult) | lit input -> result

instance parseLiteralGeneral' ::
    ( Length literal litLength
    , Length input inputLength
    , Nat.Compare inputLength litLength ord
    , ParseLiteralEnsureCorrectLength ord literal input result 
    ) => ParseLiteral literal input result

class ParseLiteralEnsureCorrectLength (ord :: Ordering) (lit :: Symbol) (input :: Symbol) (result :: ParsingResult) | ord lit input -> result

instance parseLiteralEnsureCorrectLengthLT ::
    Append "Not enough input text to match literal " literal error 
    => ParseLiteralEnsureCorrectLength LT literal input (ParsingResult (Left error) input)
else instance parseLitealEnsureCorrectLengthGeneral ::
    ParseLiteral' literal input result 
    => ParseLiteralEnsureCorrectLength ord literal input result

class ParseLiteral' (lit :: Symbol) (input :: Symbol) (result :: ParsingResult) | lit input -> result

instance parseLiteralEmpty :: ParseLiteral' "" input (ParsingResult (Right RightUnit_) input)
else instance parseLiteralGeneral ::
    ( Cons litHead litTail literal
    , Cons litInput inputTail input
    , SymbolsAreEqual litHead litInput areEqual
    , Append "Expected " litHead error
    , EIf areEqual RightUnit (Left error) result
    , ParseLiteral' litTail inputTail (ParsingResult result' remaining)
    , MergeEithers result result' result''
    ) => ParseLiteral' literal input (ParsingResult result'' remaining)

-- Skips whitespace
class SkipWhitespace (input :: Symbol) (result :: Symbol) | input -> result

instance skipWhitespaceEmpty :: SkipWhitespace "" ""
else instance skipWhitespaceNonEmpty ::
    ( Cons head tail input
    , SymbolsAreEqual head " " equal
    , SkipWhitespace tail tailResult
    , If equal (SProxy tailResult) (SProxy input) (SProxy result)
    ) => SkipWhitespace input result 

-- Lambda parsing
class ParseLambda (input :: Symbol) (result :: ParsingResult) | input -> result

instance parseLambdaGeneral ::
    ( ParseLiteral "\\" input (ParsingResult either remaining)
    , SkipWhitespace remaining remaining'
    , ParseIdentifier remaining' (ParsingResult arg remaining'')
    , SkipWhitespace remaining'' remaining'''
    , ParseLiteral "." remaining''' (ParsingResult either' remaining'''')
    , SkipWhitespace remaining'''' remaining'''''
    , ParseIdentifier remaining''''' (ParsingResult body remaining'''''')
    , MergeEithers either either' mergedEither
    , MergeEithers arg body mergedEither'
    , MergeEithers mergedEither mergedEither' mergedEither''
    , ParseLambda' mergedEither'' arg body remaining'''''' result
    ) => ParseLambda input result

class ParseLambda' 
    (either :: Either) 
    (arg :: Either) 
    (body :: Either) 
    (input :: Symbol) 
    (result :: ParsingResult) 
    | either input -> result

instance parseLambda'Left :: ParseLambda' (Left left) arg body remaining (ParsingResult (Left left) remaining)
else instance parseLambda'Right :: 
    ParseLambda' 
        right 
        (Right (RightSymbol_ arg)) 
        (Right (RightSymbol_ body)) 
        remaining
        (ParsingResult (Right (RightAst (Lambda arg (Var body)))) remaining)

-- Parses a single alphabetic character
class IsAlphaChar (input :: Symbol) (result :: Boolean) | input -> result

instance isAlphaChar ::
    ( Compare "@" input a
    , Compare "{" input b
    , OrdsAreEqual a LT a'
    , OrdsAreEqual b GT b'
    , And a' b' result
    ) => IsAlphaChar input result

-- Parses a single identifier
class ParseIdentifier (input :: Symbol) (result :: ParsingResult) | input -> result

instance parseIdentifierEnsureCorrectLength ::
    ( ParseIdentifier' input result remaining
    , Length result resultLength
    , NAreEqual resultLength Zero empty
    , Not empty success
    , EIf empty (Left "Empty identifier") (RightSymbol result) result'
     ) => ParseIdentifier input (ParsingResult result' remaining)

class ParseIdentifier' (input :: Symbol) (result :: Symbol) (remaining :: Symbol) | input -> result remaining

instance parseIdentifierGeneral :: 
    ( IsAlphaChar input good
    , ParseIdentifier'' good input result remaining
    ) => ParseIdentifier' input result remaining

class ParseIdentifier'' 
    (goodFirstChar :: Boolean) 
    (input :: Symbol) 
    (result :: Symbol) 
    (remaining :: Symbol) | goodFirstChar input -> result remaining

instance parseIdentifierGood :: 
    ( Cons head remaining input
    , ParseIdentifier' remaining tail remaining'
    , Append head tail result 
    ) => ParseIdentifier'' True input result remaining'
else instance parseIdentifierBad :: ParseIdentifier'' False input "" input

-- Helpers
length :: forall s n. Length s n => SProxy s -> NProxy n
length _ = NProxy

parseIdentifier :: 
    forall input result. 
    ParseIdentifier input result => 
    SProxy input -> 
    PRProxy result
parseIdentifier _ = PRProxy

parseLambda :: 
    forall input result.
    ParseLambda input result =>
    SProxy input ->
    PRProxy result
parseLambda _ = PRProxy

parseLiteral :: 
    forall literal input result.
    ParseLiteral literal input result =>
    SProxy literal ->
    SProxy input ->
    PRProxy result
parseLiteral _ _ = PRProxy

-- Tests
-- Parse identifier
a :: PRProxy (ParsingResult (Left "Empty identifier") "")
a = parseIdentifier (SProxy :: SProxy "")

b :: PRProxy (ParsingResult (Right (RightSymbol_ "abc")) "")
b = parseIdentifier (SProxy :: SProxy "abc")

c :: PRProxy (ParsingResult (Right (RightSymbol_ "abc")) " something else CAPS")
c = parseIdentifier (SProxy :: SProxy "abc something else CAPS")

d :: PRProxy (ParsingResult (Left "Empty identifier") "     doesn\'t automatically skip whitespace")
d = parseIdentifier (SProxy :: SProxy "     doesn't automatically skip whitespace")

-- Parsing literals
lit :: PRProxy (ParsingResult (Right RightUnit_) " hmmmm!!!")
lit = parseLiteral (SProxy :: SProxy "something") (SProxy :: SProxy "something hmmmm!!!")

lit' :: PRProxy (ParsingResult (Left "Expected s") "ing hmmmm!!!")
lit' = parseLiteral (SProxy :: SProxy "something") (SProxy :: SProxy "   something hmmmm!!!")

lit'' :: PRProxy (ParsingResult (Left "Not enough input text to match literal something") "someth")
lit'' = parseLiteral (SProxy :: SProxy "something") (SProxy :: SProxy "someth")

lit''' :: PRProxy (ParsingResult (Left "Expected e") "g hmmm")
lit''' = parseLiteral (SProxy :: SProxy "something") (SProxy :: SProxy "somnething hmmm")

lit'''' :: PRProxy (ParsingResult (Left "Expected s") "mmm")
lit'''' = parseLiteral (SProxy :: SProxy "something") (SProxy :: SProxy "zomethi hmmm")

-- Lambda parsing
lam :: PRProxy (ParsingResult (Right (RightAst (Lambda "a" (Var "a")))) "")
lam = parseLambda (SProxy :: SProxy "\\a. a")

-- Lenghts
lengthTest :: NProxy (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
lengthTest = length (SProxy :: SProxy "Adriel")