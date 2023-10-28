module My.Integers 

import My.Nats
import My.Structures
import Syntax.PreorderReasoning
import My.Syntax.Rewrite
import My.Signs

%default total

public export
ℤ : Type
ℤ = (ℕ, ℕ)

public export
addIntegers : ℤ -> ℤ -> ℤ 
addIntegers (x, y) (z, w) = (x + z, y + w)

public export 
toNat : ℤ -> (Sign, ℕ) 
toNat (Z, S x) = (Negative, S x)
toNat (x, Z) = (Positive, x)
toNat ((S x), (S y)) = toNat $ assert_smaller (S x, S y) (x, y)

public export
multiplyIntegers : ℤ -> ℤ -> ℤ 
multiplyIntegers (x, y) (z, w) = (x * z + y * w, x * w + y * z)

public export
negateInteger : ℤ -> ℤ 
negateInteger (x, y) = (y, x)

public export
substractIntegers : ℤ -> ℤ -> ℤ
substractIntegers a b = (?l, ?r)

public export
normalForm : ℤ -> ℤ 
normalForm ((S x), (S y)) = normalForm (assert_smaller (S x, S y) (x, y))
normalForm a = a 

-- absoluteValue : ℤ -> ℕ 
-- absoluteValue (Z, b) = b
-- absoluteValue ((S x), y) = absoluteValue (x, y)

public export
fromNat : ℕ -> ℤ
fromNat n = (n, Z)

public export
fromActualInteger : Integer -> ℤ
fromActualInteger 0 = (Z, Z)
fromActualInteger n = 
    if n > 0 then 
      fromNat (fromInteger n) 
    else 
      negateInteger (fromNat (fromInteger (-n)))

public export
Num ℤ where
  (+) = addIntegers
  (*) = multiplyIntegers
  fromInteger = fromActualInteger

public export
Neg ℤ where
  negate = negateInteger
  (-) = substractIntegers

---------- Equivalence
public export
integersAreEquivalent : ℤ -> ℤ -> Type
integersAreEquivalent (x, y) (z, w) = x + w = z + y

public export
equivalenceIsReflexive : integersAreEquivalent a a
equivalenceIsReflexive {a = (x, y)} = Refl

public export
equivalenceIsTransitive : {a, b, c: ℤ} -> integersAreEquivalent a b -> integersAreEquivalent b c -> integersAreEquivalent a c
equivalenceIsTransitive {a = (z, w)} {b = (v, s)} {c = (t, u)} zsISvw vuISts = id 
  $ … (z + u = t + w)
  $ My.Nats.substractionPreservesEquality s
    $ … ((z + u) + s = (t + w) + s)
  $ rewrite My.Nats.additionIsCommutative z u
    in …l ((u + z) + s) 
  $ rewrite My.Nats.additionIsAssociative u z s 
    in …l (u + (z + s))
  $ rewrite zsISvw 
    in …l (u + (v + w))
  $ rewrite sym $ My.Nats.additionIsAssociative u v w
    in …l ((u + v) + w)
  $ rewrite My.Nats.additionIsCommutative t w
    in …r ((w + t) + s)
  $ rewrite My.Nats.additionIsAssociative w t s
    in …r (w + (t + s))
  $ rewrite My.Nats.additionIsCommutative w (t + s)
    in …r ((t + s) + w)
  $ My.Nats.additionPreservesEquality w
    $ … (u + v = t + s)
  $ rewrite My.Nats.additionIsCommutative u v
    in …l (v + u)
  $ vuISts

public export
equivalenceIsSymmetric : {a, b: ℤ} -> integersAreEquivalent a b -> integersAreEquivalent b a
equivalenceIsSymmetric {a = (y, z)} {b = (w, v)} x = sym x

public export
My.Structures.Setoid ℤ where
  (<->) = integersAreEquivalent
  reflexivity = equivalenceIsReflexive
  transitivity = equivalenceIsTransitive
  symmetry = equivalenceIsSymmetric

---------- Addition proofs
public export
additionIsCommutative : CommutativityProof My.Integers.addIntegers
additionIsCommutative (Z, y) (x, z) = (x + (z + y) = (x + 0) + (y + z))
  .... rewrite My.Nats.additionRightIdentity x in (x + (z + y) = x + (y + z))
  .... rewrite My.Nats.additionIsCommutative y z in (x + (z + y) = x + (z + y))
  .... Refl
additionIsCommutative ((S x), y) (z, w) = id
  $ … (1 + ((x + z) + (w + y)) = (z + (1 + x)) + (y + w))
  $ rewrite My.Nats.additionIsCommutative y w 
      in …r ((z + (1 + x)) + (w + y))
  $ rewrite My.Nats.additionIsCommutative z (1 + x) 
      in …r (1 + (x + z) + (w + y)) 
  $ Refl

public export
additionIsAssociative : AssociativityProof My.Integers.addIntegers
additionIsAssociative (x, y) (z, w) (v, s) = id 
  $ … ((((x + z) + v) + (y + (w + s))) = ((x + (z + v) + ((y + w) + s))))
  $ rewrite My.Nats.additionIsAssociative x z v 
    in …l ((x + (z + v) + (y + (w + s))))
  $ rewrite My.Nats.additionIsAssociative y w s
    in …r ((x + (z + v) + (y + (w + s))))
  $ Refl

public export
additionRightIdentity : RightIdentityProof My.Integers.addIntegers 0
additionRightIdentity (x, y) = id
  $ … ((x + 0) + y = x + (y + 0))
  $ rewrite My.Nats.additionRightIdentity x
    in …l (x + y)
  $ rewrite My.Nats.additionRightIdentity y
    in …r (x + y)
  $ Refl

public export
additionLeftIdentity : LeftIdentityProof My.Integers.addIntegers 0
additionLeftIdentity (x, y) = Refl

---------- Multiplication proofs
multiplyToNatResults : (Sign, ℕ) -> (Sign, ℕ) -> (Sign, ℕ)
multiplyToNatResults (x, z) (y, w) = (multiplySigns x y, z * w)

ToNatDistributesMultiplication : ℤ -> ℤ -> Type
ToNatDistributesMultiplication a b = toNat (a * b) = multiplyToNatResults (toNat a) (toNat b)

toNatDistributesMultiplication : (a, b: ℤ) -> ToNatDistributesMultiplication a b
toNatDistributesMultiplication (Z, Z) (Z, Z) = Refl
toNatDistributesMultiplication (Z, Z) (Z, (S x)) = ?toNatDistributesMultiplication_rhs_9

toNatDistributesMultiplication (Z, Z) ((S x), w) = ?toNatDistributesMultiplication_rhs_7

toNatDistributesMultiplication (Z, (S x)) (z, w) = ?toNatDistributesMultiplication_rhs_5

toNatDistributesMultiplication ((S x), y) (z, w) = ?toNatDistributesMultiplication_rhs_3



multiplicationIsAssociative : AssociativityProof My.Integers.multiplyIntegers
multiplicationIsAssociative 0 b c = ?multiplicationIsAssociative_rhs
multiplicationIsAssociative a b c = ?multiplicationIsAssociative_rhs_1


---------- Interface implementations
public export
[additionSemigroup] My.Structures.Semigroup ℤ where
  ∘ = addIntegers
  associativityProof = additionIsAssociative


public export
[additionMonoid] My.Structures.Monoid ℤ using My.Integers.additionSemigroup where
  empty = 0
  rightIdentityProof = My.Integers.additionRightIdentity
  leftIdentityProof = My.Integers.additionLeftIdentity

[multiplicationSemigroup] My.Structures.Semigroup ℤ where
  ∘ = multiplyIntegers
  associativityProof = multiplicationIsAssociative

---------- Constants to play around with
seven : ℤ 
seven = 7

minusFour : ℤ
minusFour = -4

three : ℤ 
three = 3

three' : ℤ
three' = minusFour + seven
