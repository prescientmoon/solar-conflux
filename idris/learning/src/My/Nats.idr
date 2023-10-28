module My.Nats
 
import My.Structures

%default total
%hide Z
%hide S


public export
data ℕ : Type where
  Z : ℕ 
  S : ℕ -> ℕ 

public export
fromIntegerNat : Integer -> ℕ 
fromIntegerNat 0 = Z
fromIntegerNat n =
  if (n > 0) then
    S (fromIntegerNat (assert_smaller n (n - 1)))
  else
    Z

one : ℕ
one = S Z

public export
add : ℕ -> ℕ -> ℕ 
add Z a = a
add (S a) b = S (add a b)

public export
multiply : ℕ -> ℕ -> ℕ 
multiply Z a = Z
multiply (S a) b = add b (multiply a b)

public export
raiseToPower : ℕ -> ℕ -> ℕ 
raiseToPower a Z = one
raiseToPower a (S b) = multiply a (raiseToPower a b)

public export
monus : ℕ -> ℕ -> ℕ 
monus (S a) (S b) = monus a b
monus a Z = a
monus _ (S _) = Z

public export
naturalInduction : (P: ℕ -> Type) -> P Z -> ({x: ℕ} -> P x -> P (S x)) -> (x: ℕ) -> P x
naturalInduction p base recurse Z = base
naturalInduction p base recurse (S a) = recurse (naturalInduction p base recurse a) 

public export
Num ℕ where
  fromInteger = fromIntegerNat 
  (+) = add
  (*) = multiply

public export
%hint
setoidNats : My.Structures.Setoid ℕ
setoidNats = trivialSetoid ℕ

---------- Proofs
public export
succCommutesAddition : (a, b: ℕ) -> add (S a) b = add a (S b)
succCommutesAddition Z a = Refl
succCommutesAddition (S c) b = let 
  rec = succCommutesAddition c b
  in rewrite rec in Refl

public export
additionIsAssociative : AssociativityProof My.Nats.add
additionIsAssociative Z b c = Refl
additionIsAssociative (S a) b c = let
  rec = additionIsAssociative a b c
  in rewrite rec in Refl

public export
additionRightIdentity : RightIdentityProof My.Nats.add 0
additionRightIdentity Z = Refl
additionRightIdentity (S x) = rewrite additionRightIdentity x in Refl

public export
additionIsCommutative : CommutativityProof My.Nats.add
additionIsCommutative Z b = sym (additionRightIdentity b)
additionIsCommutative (S x) Z = rewrite additionIsCommutative x Z in Refl
additionIsCommutative (S x) (S y) = 
    rewrite sym (succCommutesAddition x y) in
    rewrite additionIsCommutative y (S x) in 
      Refl

----- Multiplication proofs
public export
multiplicationRightNullification : (a: ℕ) -> multiply a 0 = 0
multiplicationRightNullification Z = Refl
multiplicationRightNullification (S x) = rewrite multiplicationRightNullification x in Refl

public export
multiplicationRightIdentity : (a: ℕ) -> a * 1 = a
multiplicationRightIdentity Z = Refl
multiplicationRightIdentity (S x) = rewrite multiplicationRightIdentity x in Refl

public export
multiplicationLeftIdentity : (a: ℕ) -> a = 1 * a
multiplicationLeftIdentity a = rewrite additionRightIdentity a in Refl

public export
multiplicationDistributesAddition : (a, b, c: ℕ) -> a * (b + c) = a * b + a * c
multiplicationDistributesAddition Z b c = Refl
multiplicationDistributesAddition (S x) b c
  = let rec = multiplicationDistributesAddition x b c 
  in rewrite rec 
  in rewrite additionIsAssociative b c ((x * b) + (x * c))
  in rewrite additionIsAssociative b (x * b) (c + (x * c))
  in rewrite additionIsCommutative (x * b) (c + (x * c))
  in rewrite additionIsAssociative c (x * c) (x * b)
  in rewrite additionIsCommutative (x * b) (x * c)
  in Refl

public export
succIsPlusOne : (a: ℕ) -> S a = a + 1
succIsPlusOne Z = Refl
succIsPlusOne (S x) = rewrite additionIsCommutative x 1 in Refl

public export
multiplicationisCommutative : CommutativityProof My.Nats.multiply
multiplicationisCommutative Z b = sym (multiplicationRightNullification b)
multiplicationisCommutative (S x) b = 
  rewrite succIsPlusOne x in
  rewrite multiplicationDistributesAddition b x 1 in
  rewrite multiplicationRightIdentity b in
  rewrite additionIsCommutative b (x * b) in
  rewrite multiplicationisCommutative x b in
  Refl


public export
multiplicationIsAssociative : AssociativityProof My.Nats.multiply
multiplicationIsAssociative Z b c = Refl
multiplicationIsAssociative (S x) y c =  
  rewrite multiplicationisCommutative (y + (x * y)) c in
  rewrite multiplicationDistributesAddition c y (x * y) in
  rewrite multiplicationisCommutative y c in
  rewrite sym (multiplicationIsAssociative c x y) in
  rewrite sym (multiplicationIsAssociative x c y) in
  rewrite sym (multiplicationisCommutative x c) in
  Refl

---------- Monus proofs
public export
xMinusXIsZero : (a: ℕ) -> (monus a a) = 0
xMinusXIsZero Z = Refl
xMinusXIsZero (S x) = xMinusXIsZero x

public export
additionNullifiesMonus : (a, b: ℕ) -> (monus (a + b) b) = a
additionNullifiesMonus Z b = xMinusXIsZero b
additionNullifiesMonus (S x) Z = rewrite additionRightIdentity x in Refl
additionNullifiesMonus x (S y) = rewrite sym $ succCommutesAddition x y in additionNullifiesMonus x y

---------- Equality proofs
public export
additionPreservesEquality : {a, b: ℕ} -> (c: ℕ) -> (a = b) -> (a + c = b + c)
additionPreservesEquality c prf = cong (+c) prf

public export
substractionPreservesEquality : {a, b: ℕ} -> (c: ℕ) -> (a + c = b + c) -> (a = b)
substractionPreservesEquality c prf = let
    middle : (monus (a + c) c = monus (b + c) c)
    middle = cong (\e => monus e c) prf

    left : (a = monus (a + c) c)
    left = sym $ additionNullifiesMonus a c

    right : (monus (b + c) c = b)
    right = additionNullifiesMonus b c
    in (left `trans` middle) `trans` right

public export
multiplicationPreservesEquality : {a, b: ℕ} -> (c: ℕ) -> (a = b) -> (a * c = b * c)
multiplicationPreservesEquality c prf = cong (*c) prf

---------- Interace implementations
public export
[additionSemigroup] My.Structures.Semigroup ℕ where
  ∘ = add
  associativityProof = additionIsAssociative

public export
[additionMonoid] My.Structures.Monoid ℕ using additionSemigroup where
  empty = 0
  rightIdentityProof a = additionRightIdentity a
  leftIdentityProof a = Refl

public export
[multiplicationSemigroup] My.Structures.Semigroup ℕ where
  ∘ = multiply
  associativityProof = multiplicationIsAssociative
 
public export
[multiplicationMonoid] My.Structures.Monoid ℕ using multiplicationSemigroup where
   empty = 1
   rightIdentityProof = multiplicationRightIdentity
   leftIdentityProof  = multiplicationLeftIdentity

