module My.Signs

import My.Structures

public export
data Sign = Positive | Negative

public export
negateSign : Sign -> Sign
negateSign Positive = Negative
negateSign Negative = Positive

public export
multiplySigns : Sign -> Sign -> Sign
multiplySigns Positive b = b
multiplySigns Negative b = negateSign b

public export
%hint
setoidSign : My.Structures.Setoid Sign
setoidSign = trivialSetoid Sign

doubleNegationIdentity : (a: Sign) -> negateSign (negateSign a) = a
doubleNegationIdentity Positive = Refl
doubleNegationIdentity Negative = Refl

multiplicationIsAssociative : AssociativityProof My.Signs.multiplySigns
multiplicationIsAssociative Positive b c = Refl
multiplicationIsAssociative Negative Positive c = Refl
multiplicationIsAssociative Negative Negative c = sym $ doubleNegationIdentity c

multiplicationRightIdentity : RightIdentityProof My.Signs.multiplySigns Positive 
multiplicationRightIdentity Positive = Refl
multiplicationRightIdentity Negative = Refl

multiplicationLeftIdentity : LeftIdentityProof My.Signs.multiplySigns Positive
multiplicationLeftIdentity _ = Refl

multiplicationIsCommutative : CommutativityProof My.Signs.multiplySigns 
multiplicationIsCommutative Positive b = sym $ multiplicationRightIdentity b
multiplicationIsCommutative Negative Positive = Refl
multiplicationIsCommutative Negative Negative = Refl

public export
My.Structures.Semigroup Sign where
  ∘ = multiplySigns
  associativityProof = multiplicationIsAssociative

public export
My.Structures.Monoid Sign where
  empty = Positive
  rightIdentityProof = multiplicationRightIdentity
  leftIdentityProof = multiplicationLeftIdentity
