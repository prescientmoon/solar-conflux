module My.Structures

%hide Semigroup
%hide Monoid
%hide empty

infix 5 <->

public export
interface Setoid t where
  constructor MkSetoid
  (<->) : t -> t -> Type
  reflexivity : {0 a: t} -> a <-> a
  transitivity : {a,b,c: t} -> (a <-> b) -> (b <-> c) -> (a <-> c)
  symmetry : {a, b: t} -> (a <-> b) -> (b <-> a)

public export
trivialSetoid : (t: Type) -> Setoid t
trivialSetoid _ = MkSetoid Equal Refl (\a,b => trans a b) (\a => sym a)

public export
AssociativityProof : {x: Type} -> Setoid x => (x -> x -> x) -> Type
AssociativityProof {x} t = (a, b, c: x) -> (t (t a b) c) <-> (t a (t b c))

public export
RightIdentityProof : {x: Type} -> Setoid x => (x -> x -> x) -> x -> Type
RightIdentityProof t e = (a: x) -> t a e <-> a

public export
LeftIdentityProof : {x: Type} -> Setoid x => (x -> x -> x) -> x -> Type
LeftIdentityProof t e = (a: x) -> a <-> t e a

public export
RightInverseProof : {x: Type} -> Setoid x => (x -> x -> x) -> (x -> x) -> x -> Type
RightInverseProof t inverse e = (a: x) -> t a (inverse a) <-> e

public export
LeftInverseProof : {x: Type} -> Setoid x => (x -> x -> x) -> (x -> x) -> x -> Type
LeftInverseProof t inverse e = (a: x) -> t (inverse a) a <-> e

public export
CommutativityProof : {x: Type} -> Setoid x => (x -> x -> x) -> Type
CommutativityProof t = (a, b: x) -> t a b <-> t b a

public export
rightToLeftIdentity : 
  {x: Type} -> Setoid x => 
  (f: x -> x -> x) -> (e: x) -> 
  CommutativityProof f -> 
  RightIdentityProof f e -> 
  LeftIdentityProof f e
rightToLeftIdentity f e commutativity rightIdentity x = symmetry $ transitivity (commutativity e x) (rightIdentity x) 

public export
leftToRightIdentity : 
  {x: Type} -> Setoid x => 
  (f: x -> x -> x) -> (e: x) -> 
  CommutativityProof f -> 
  LeftIdentityProof f e -> 
  RightIdentityProof f e
leftToRightIdentity f e commutativity leftIdentity x = symmetry $ transitivity (leftIdentity x) (commutativity e x)

public export
interface Setoid t => Semigroup t where
   ∘ : t -> t -> t
   associativityProof : AssociativityProof ∘

public export
interface Semigroup t => Monoid t where
  empty : t
  rightIdentityProof : RightIdentityProof ∘ empty
  leftIdentityProof : LeftIdentityProof ∘ empty

public export
interface Monoid t => Group t where
  ⁻¹ : t -> t
  rightInverseProof : RightInverseProof ∘ (⁻¹) My.Structures.empty
  leftInverseProof : LeftInverseProof ∘ (⁻¹) My.Structures.empty

