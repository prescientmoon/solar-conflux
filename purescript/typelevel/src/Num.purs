module Num where

import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Ordering (EQ, GT, LT, kind Ordering)
import Prim.Symbol (class Cons)

foreign import kind Num

foreign import data Zero :: Num
foreign import data Succ :: Num -> Num

data NProxy (i :: Num)
  = NProxy

-- Predecessor
class Pred (input :: Num) (output :: Num) | input -> output, output -> input

instance predSucc :: Pred (Succ a) a

-- Conditionals
class NumIf 
  (bool :: Boolean)
  (onTrue :: Num)
  (onFalse :: Num)
  (output :: Num) 
  | bool onTrue onFalse -> output

instance ifTrue :: NumIf True onTrue onFalse onTrue
instance ifFalse :: NumIf False onTrue onFalse onFalse

-- Addition
class Add (a :: Num) (b :: Num) (output :: Num) 
  | a b -> output
  , a output -> b

instance addZero :: Add Zero a a
else instance addSucc :: Add a b c => Add (Succ a) b (Succ c)

-- Substraction 
class Sub (a :: Num) (b :: Num) (output :: Num) 
  | a b -> output
  , b output -> a

instance subZero :: Sub a Zero a
else instance subSucc :: Sub a b c => Sub (Succ a) (Succ b) c

-- Multiplication 
class Multiply (a :: Num) (b :: Num) (output :: Num) 
  | a b -> output

instance multiplyZero :: Multiply Zero a Zero
else instance multiplySucc :: (Multiply a b c, Add b c result) => Multiply (Succ a) b result

-- Division
class Divide'
  (a :: Num) 
  (b :: Num) 
  (result :: Num) 
  (mod :: Num) 
  (ord :: Ordering)
  | a b ord -> result mod

instance divideZero :: Divide' Zero a Zero Zero LT
else instance divideMod :: Divide' a b Zero a LT
else instance divideSucc :: (Divide a b result mod, Sub a' b a) => Divide' a' b (Succ result) mod ord

class Divide 
  (a :: Num) 
  (b :: Num) 
  (result :: Num) 
  (mod :: Num) 
  | a b -> result mod

instance divideAndCompare :: (Compare a b ord, Divide' a b result mod ord) => Divide a b result mod

-- Raising to power
class Pow (base :: Num) (power :: Num) (output :: Num) 
  | base power -> output
  -- Does this work?
  -- , power output -> base

instance powZero :: Pow Zero a (Succ Zero) 
else instance powOne :: Pow a (Succ Zero) a
else instance powSucc :: (Pow a b c, Multiply c a result) => Pow a (Succ b) result 

-- Equality checking 
class NAreEqual (a :: Num) (b :: Num) (result :: Boolean) | a b -> result

instance areEqualZero :: NAreEqual Zero Zero True
else instance areEqualSucc :: NAreEqual a b c => NAreEqual (Succ a) (Succ b) c
else instance areNotEqual :: NAreEqual a b False

class Factorial (input :: Num) (output :: Num) | input -> output

instance factorialZero :: Factorial Zero (Succ Zero)
instance factorialSucc :: (Factorial a a', Multiply a' (Succ a) result) => Factorial (Succ a) result

-- Ordering
class Compare (a :: Num) (b :: Num) (output :: Ordering) 
  | a b -> output

instance compareEqual :: Compare Zero Zero EQ
else instance compareZeroLower :: Compare Zero a LT
else instance compareZeroGreater :: Compare a Zero GT
else instance compareSucc :: Compare a b c => Compare (Succ a) (Succ b) c

-- Parsing
class ParseNum (input :: Symbol) (output :: Num) | input -> output

instance parseNum :: (Cons head tail input, ParseNum' head tail output) => ParseNum input output

class ParseNum' (head :: Symbol) (tail :: Symbol) (output :: Num) | head tail -> output

instance parseNumSingle :: ParseDigit input result => ParseNum' input "" result
else instance parseNumHeadTail :: 
  ( ParseDigit head resultHead
  , ParseNum tail resultTail
  , Multiply resultHead (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))) result' 
  , Add result' resultTail result )
    => ParseNum' head tail result

class ParseDigit (input :: Symbol) (output :: Num) | input -> output, output -> input

instance parseDigit0 :: ParseDigit "0" Zero
instance parseDigit1 :: ParseDigit "1" (Succ Zero)
instance parseDigit2 :: ParseDigit "2" (Succ (Succ Zero))
instance parseDigit3 :: ParseDigit "3" (Succ (Succ (Succ Zero)))
instance parseDigit4 :: ParseDigit "4" (Succ (Succ (Succ (Succ Zero))))
instance parseDigit5 :: ParseDigit "5" (Succ (Succ (Succ (Succ (Succ Zero)))))
instance parseDigit6 :: ParseDigit "6" (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
instance parseDigit7 :: ParseDigit "7" (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))
instance parseDigit8 :: ParseDigit "8" (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))
instance parseDigit9 :: ParseDigit "9" (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))

-- Constructors
zero :: NProxy Zero
zero = NProxy

succ :: forall a. NProxy a -> NProxy (Succ a)
succ _ = NProxy

pred :: forall i o. Pred i o => NProxy i -> NProxy o
pred _ = NProxy

add :: forall a b c. Add a b c => NProxy a -> NProxy b -> NProxy c
add _ _ = NProxy

sub :: forall a b c. Sub a b c => NProxy a -> NProxy b -> NProxy c
sub _ _ = NProxy

multiply :: forall a b c. Multiply a b c => NProxy a -> NProxy b -> NProxy c
multiply _ _ = NProxy

pow :: forall a b c. Pow a b c => NProxy a -> NProxy b -> NProxy c
pow _ _  = NProxy 

divide ::forall a b c d. Divide a b c d => NProxy a -> NProxy b -> { result :: NProxy c, mod :: NProxy d }
divide _ _  = { mod: NProxy, result: NProxy }

div :: forall a b c d. Divide a b c d => NProxy a -> NProxy b -> NProxy c
div _ _  = NProxy

mod :: forall a b c d. Divide a b c d => NProxy a -> NProxy b -> NProxy d
mod _ _  = NProxy

parse :: forall a n. ParseNum a n => SProxy a -> NProxy n
parse _ = NProxy

equal :: forall a b. NAreEqual a b True => NProxy a -> NProxy b -> Unit
equal _ _ = unit

factorial :: forall a b. Factorial a b => NProxy a -> NProxy b
factorial _ = NProxy

-- Basic values
type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four
type Six = Succ Five
type Seven = Succ Six
type Eight = Succ Seven
type Nine = Succ Eight
type Ten = Succ Nine

-- Tests
one :: NProxy One
one = succ zero

two :: NProxy Two
two = succ one

three :: NProxy Three
three = succ two

five :: NProxy (Succ (Succ (Succ (Succ (Succ Zero)))))
five = add two three

-- Solvable when the first param is missing
two' :: NProxy Two
two' = sub five three

six :: NProxy (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
six = multiply three two

fifteen :: NProxy (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
fifteen = multiply three two

eight :: NProxy (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))
eight = pow two three

one' :: NProxy (Succ Zero)
one' = pow zero three

four :: NProxy (Succ Three)
four = succ three

five' :: NProxy (Succ (Succ (Succ (Succ Zero))))
five' = pow two two
 where
 _25 = add two (add eight fifteen)

sixDivThree :: NProxy (Succ (Succ Zero))
sixDivThree = div six three

sevenDivThree :: 
  { mod :: NProxy (Succ Zero)
  , result :: NProxy (Succ (Succ Zero))
  }
sevenDivThree = divide (succ six) three

parse13 :: NProxy (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))))))
parse13 = parse _13
  where
  _13 :: SProxy "13"
  _13 = SProxy

parse64 :: Unit
parse64 = equal (parse _64) (pow two six)
  where
  _64 :: SProxy "64"
  _64 = SProxy

fac3 :: NProxy (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
fac3 = factorial (parse _3)
  where
  _3 :: SProxy "3"
  _3 = SProxy