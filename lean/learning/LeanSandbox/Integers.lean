import LeanSandbox.Nat

macro "nat_ring_all"  : tactic => `(simp_all [Nat.mul_assoc, Nat.mul_comm, Nat.mul_left_comm, Nat.add_assoc, Nat.add_left_comm, Nat.add_comm, Nat.left_distrib, Nat.right_distrib])
macro "nat_ring"  : tactic => `(simp [Nat.mul_assoc, Nat.mul_comm, Nat.mul_left_comm, Nat.add_assoc, Nat.add_left_comm, Nat.add_comm, Nat.left_distrib, Nat.right_distrib])
macro "quotient_madness" : tactic => `(simp [Quotient.mk', Quotient.mk, Quotient.liftOn₂, Quotient.lift₂, Quotient.lift])

structure RawInt where
  pos : Nat
  neg : Nat
  deriving Repr

private def eqv : (x y: RawInt) → Prop
  | ⟨a, b⟩, ⟨c, d⟩ => a + d = c + b

infix:50 " ~ " => eqv

private theorem eqv.refl (x: RawInt) : x ~ x := rfl
private theorem eqv.symm {x y: RawInt} (xy: x ~ y):  y ~ x := Eq.symm xy
/-
a - b   c - d   e - f
a + d = c + b
c + f = e + d
 => a + f = e + b -- the target

a + d + c + f = c + b + e + d
a + f + e + b -- done
-/
private theorem eqv.trans {x y z: RawInt} (xy: x ~ y) (yz: y ~ z): x ~ z := by
  have summed: _ := Nat.add_equations xy yz
  apply @Nat.add_right_cancel _ (y.pos + y.neg) _
  nat_ring_all

private theorem is_equivalence: Equivalence eqv := 
  { refl := eqv.refl, symm := eqv.symm, trans := eqv.trans }

instance rawIntSetoid: Setoid RawInt where
  r := eqv
  iseqv := is_equivalence

def MyInt: Type := 
  Quotient rawIntSetoid

private theorem eqv.sound: x ~ y → Quotient.mk' x = Quotient.mk' y  := Quot.sound

@[simp]
def MyInt.mk (pos neg: Nat): MyInt := Quotient.mk' ⟨pos, neg⟩

notation "{ " a₁ ", " a₂ " }" => MyInt.mk a₁ a₂

@[simp, inline]
private def MyInt.ofRawInt(raw: RawInt) := MyInt.mk raw.pos raw.neg

@[simp, inline]
private def RawInt.ofNat(nat: Nat): RawInt := ⟨nat, 0⟩

@[simp, inline]
private def MyInt.ofNat(nat: Nat): MyInt := {nat, 0}

private instance rawIntOfNat: OfNat RawInt n where
  ofNat := RawInt.ofNat n

instance myIntOfNat: OfNat MyInt n where
  ofNat := MyInt.ofNat n

namespace MyInt
  private def negateRawInt: RawInt → MyInt
    | ⟨pos, neg⟩ => {neg, pos}

  /- 
  a - b = c - d 
  a + d = c + b

  b + c = d + a
  b - a = d - c
  -/
  private theorem negateRawInt.respects {x y: RawInt} (xy: x ~ y): negateRawInt x = negateRawInt y := by
    apply eqv.sound
    simp_all [eqv, Nat.add_comm] 

  def negate (τ: MyInt): MyInt :=
    Quotient.liftOn τ negateRawInt @negateRawInt.respects

  instance negMyInt: Neg MyInt where
    neg := negate

  private theorem double_neg_elim: ∀x, x = negate (negate x) := by
    intro x
    induction x using Quotient.ind
    rfl

  private def addRawInts: RawInt → RawInt → MyInt
    | ⟨a, b⟩, ⟨c, d⟩ => {a + c, b + d}

  private theorem addRawInts.respects 
    {a b c d: RawInt} 
    (ac: a ~ c)
    (bd: b ~ d): addRawInts a b = addRawInts c d := by
    have summed: _ := Nat.add_equations ac bd
    apply eqv.sound
    simp [eqv] at summed ⊢ 
    nat_ring_all 

  private theorem addRawInts.comm (a b: RawInt): addRawInts a b = addRawInts b a := by
    simp_all [addRawInts, Nat.add_comm]

  def add (τ β: MyInt): MyInt :=
    Quotient.liftOn₂ τ β addRawInts @addRawInts.respects

  private instance hAddRawInts: HAdd RawInt RawInt MyInt where
    hAdd := addRawInts
  instance addMyInts: Add MyInt where
    add := add

  def sub (a b: MyInt): MyInt := a + (-b)

  instance subMyInt: Sub MyInt where
    sub := sub

  @[simp]
  theorem sub.x_minus_x_is_zero (a: MyInt): a - a = 0 := by
    simp_all [HSub.hSub, sub, HAdd.hAdd, add, negate, Neg.neg, MyInt.ofNat] 
    induction a using Quotient.ind
    apply eqv.sound
    simp [eqv]
    apply Nat.add_comm

  theorem add.comm: ∀x y: MyInt, x + y = y + x := by
    intro x y
    simp_all [HAdd.hAdd, add] 
    induction x, y using Quotient.ind₂
    quotient_madness
    apply addRawInts.comm

  theorem add.assoc(x y z: MyInt): x + (y + z) = (x + y) + z := by
    simp_all [HAdd.hAdd, add] 

    induction x, y using Quotient.ind₂
    induction z using Quotient.ind 

    apply eqv.sound
    simp [eqv]
    nat_ring_all

  @[simp]
  theorem add.zero(x: MyInt): x + 0 = x := by
    simp_all [HAdd.hAdd, add]
    induction x using Quotient.ind
    apply eqv.sound
    simp [eqv]

  /-
  (a - b) * (c - d)
  ac - bc - ad + bd
  -/
  private def multiplyRawInts: RawInt → RawInt → MyInt
    | ⟨a, b⟩, ⟨c, d⟩ => {a * c + b * d, b * c + a * d}

  /-
  ac : c.neg + a.pos = a.neg + c.pos
  bd : d.neg + b.pos = b.neg + d.pos
  ⊢ a.neg * b.neg + (a.pos * b.pos + (c.pos * d.neg + c.neg * d.pos)) =
    c.neg * d.neg + (a.pos * b.neg + (a.neg * b.pos + c.pos * d.pos))

  a - b c - d e - f g - h

  f + a = b + e
  h + c = d + g

  bd + ac + eh + fg = fh + ad + bc + eg

  bd + ac + fc + eh + fg + ec = fh + ad + bc + ec + eg + fc
  + cf + ce
  bd + c(a + f) + eh + fg + ec = fh + ad + c(b + e) + eg + fc
  bd + eh + fg + ec = fh + ad + eg + fc
  bd + e(h + c) + fg = f(h + c) + ad + eg
  + bg + ag
  b(d + g) + e(h + c) + fg + ag = f(h + c) + a(d + g) + bg + eg
  (h + c)(b + e) + g(a + f) = (h + c)(f + a) + g(b + e)
  -/
  private theorem multiplyRawInts.respects: ∀
    {x y z w: RawInt} 
    (xz: x ~ z)
    (yw: y ~ w), (multiplyRawInts x y = multiplyRawInts z w)
  | ⟨a, b⟩, ⟨c, d⟩, ⟨e, f⟩, ⟨g, h⟩ => by
    intro xz yw
    apply eqv.sound
    simp_all [eqv] 

    have first: (c + h) * (b + e) + g * (a + f) + c * (a + f) 
      = (c + h) * (f + a) + g * (b + e) + c * (b + e) := by
      simp [Nat.add_comm, xz, yw]

    have second: b * (d + g) + e * (c + h) + c * (a + f) + f * g + a * g
      = f * (c + h) + a * (d + g) + c * (b + e) + b * g + e * g := by
      simp [yw, xz] at first ⊢ 
      conv at first in g * (e + b) => rw [<-xz]
      conv at first => tactic => nat_ring
      nat_ring
      exact first

    conv at second => tactic => nat_ring
    apply @Nat.subtract_to_equation_left _ _
      (a * g + b * g + c * f + c * e)

    nat_ring_all

  def multiply (τ β: MyInt): MyInt :=
    Quotient.liftOn₂ τ β multiplyRawInts @multiplyRawInts.respects

  private instance hMulRawInt: HMul RawInt RawInt MyInt where
    hMul := multiplyRawInts 
  instance mulMyInt: Mul MyInt where
    mul := multiply 

  private theorem multiplyRawInts.comm (a b: RawInt): a * b = b * a := by
    apply eqv.sound
    simp [eqv]
    simp_all [multiplyRawInts, Nat.mul_comm]
    nat_ring_all

  theorem multiply.comm (a b: MyInt): a * b = b * a := by
    simp_all [Mul.mul, multiply] 
    induction a, b using Quotient.ind₂
    quotient_madness
    apply multiplyRawInts.comm

  theorem multiply.assoc(x y z: MyInt): x * (y * z) = (x * y) * z := by
    simp_all [Mul.mul, multiply] 

    induction x, y using Quotient.ind₂
    induction z using Quotient.ind 

    apply eqv.sound
    simp [eqv]
    nat_ring_all

  @[simp]
  theorem multiply.one(x: MyInt): x * 1 = 1 * x := by
    simp_all [Mul.mul, multiply]
    induction x using Quotient.ind
    apply eqv.sound
    simp [eqv]

  @[simp]
  theorem multiply.zero(x: MyInt): x * 0 = 0 := by
    simp_all [Mul.mul, multiply]
    induction x using Quotient.ind
    apply eqv.sound
    simp [eqv]

  theorem left_distrib(x y z: MyInt): x * (y + z) = x * y + x * z := by
    simp_all [Mul.mul, Add.add, add, multiply] 

    induction x, y using Quotient.ind₂
    induction z using Quotient.ind 

    apply eqv.sound
    simp [eqv]
    nat_ring_all

  theorem right_distrib(x y z: MyInt): (x + y) * z = x * z + y * z := by
    simp_all [Mul.mul, Add.add, add, multiply] 

    induction x, y using Quotient.ind₂
    induction z using Quotient.ind 

    apply eqv.sound
    simp [eqv]
    nat_ring_all

  /-
  notes on division?
  t * (c - d) + r = a - b 
  t * c + b + r = a + t * d
  -/
  @[simp]
  def is_even(x: MyInt) := ∃h, h + h = x
  @[simp]
  def is_odd(x: MyInt) := ∃h, h + h + 1 = x

  theorem double_is_even(x: MyInt): is_even (2 * x) := by
    simp
    exists x
    induction x using Quotient.ind
    apply eqv.sound
    simp [eqv, Nat.double.addition_is_multiplication]

  theorem raw_int_induction 
    (P: MyInt → Prop) 
    (pz: P 0) 
    (pn: ∀k, P k ↔ P (k + 1)):
    (x: RawInt) → ∃k, k ~ x ∧ P (MyInt.ofRawInt k)
  | ⟨0, 0⟩ => ⟨0, ⟨rfl, pz⟩⟩
  | ⟨Nat.succ a, 0⟩ => by 
    have ⟨⟨kp, kn⟩, pk⟩ := raw_int_induction P pz pn ⟨a, 0⟩
    exists (⟨kp + 1, kn⟩ : RawInt)
    apply And.intro 
    . simp [eqv, Nat.succ_add]
      rw [<-pk.left]
      simp [Nat.add_zero]
    . apply (@pn {kp, kn}).mp
      exact pk.right
  | ⟨Nat.succ a, Nat.succ b⟩ => by 
    have ⟨k, pk⟩ := raw_int_induction P pz pn ⟨a, b⟩
    exists k
    apply And.intro
    . simp [eqv, Nat.succ_add]
      rw [<-pk.left]
      simp_arith
    . exact pk.right
  | ⟨0, Nat.succ a⟩ => by
    have ⟨⟨kp, kn⟩, pk⟩ := raw_int_induction P pz pn ⟨0, a⟩
    exists (⟨kp, kn + 1⟩ : RawInt)
    apply And.intro 
    . have pkl := pk.left
      simp [eqv, Nat.succ_add, Nat.add_zero] at pkl ⊢ 
      rw [<-pkl]
      simp_arith
    . have recurse := (@pn {kp, kn + 1}).mpr
      have rewriter: {kp, kn + 1} + 1 = {kp, kn} := by
        apply eqv.sound
        simp [eqv]
        simp_arith
      rw [rewriter] at recurse
      exact (recurse pk.right)

  theorem int_induction 
    (P: MyInt → Prop) 
    (zero: P 0) 
    (succ: ∀k, P k ↔ P (k + 1)):
      ∀k, P k := by
      intro k
      induction k using Quotient.ind 
      rename RawInt => kRaw
      have ⟨e, ⟨eIsK, proof⟩⟩ := raw_int_induction P zero succ kRaw
      have eIsKQuot : MyInt.ofRawInt e = MyInt.ofRawInt kRaw := by
        exact (eqv.sound eIsK)
      simp [Quotient.mk'] at eIsKQuot
      rw [<-eIsKQuot]
      exact proof 

  theorem add_left_cancel {a b c: MyInt}: a + b = a + c → b = c := by
    intro hip
    induction b, c using Quotient.ind₂ 
    induction a using Quotient.ind 
    rename RawInt => c
    simp_all [HAdd.hAdd, Add.add, add]
    conv at hip => tactic => quotient_madness
    /- apply eqv.sound  -/
    /- simp [eqv] -/
    /- nat_ring -/
    simp_all [MyInt.addRawInts, Quotient.mk', Quotient.mk]
    sorry

/-     induction a using int_induction with -/
/-     | zero =>  -/
/-       rw [add.comm 0 c, add.comm 0 b] -/
/-       simp_all -/
/-     | succ k =>  -/
/-       apply Iff.intro -/
/-       . intro win  -/
/-         intro previous -/
/-         have p: k + b = k + c := by  -/
/-           rw [add.comm k 1] at previous -/
/-           induction b, c using Quotient.ind₂  -/
/-           induction k using Quotient.ind  -/
/-           rename RawInt => c -/
/-           simp [HAdd.hAdd, Add.add, add] -/
/-           apply eqv.sound  -/
/-           simp [eqv] -/
/-           nat_ring -/
/-  -/
/-           sorry -/
/-         exact (win p) -/
/-       . sorry -/
/-  -/

  theorem odd_and_even_contradict(x: MyInt): ¬(is_odd x ∧ is_even x)
  | ⟨⟨h₁, oddProof⟩, ⟨h₂, evenProof⟩⟩ => by
    have wrong: (1: MyInt) = 0 := by 
      apply @add_left_cancel (h₁ + h₂)
      exact oddProof.trans evenProof.symm
      sorry
    contradiction
  
  theorem odds_not_even(x: MyInt): is_odd x ↔ ¬(is_even x) := by
    apply Iff.intro
    case mp =>
      intro oddProof
      intro evenProof
      apply odd_and_even_contradict x
      exact ⟨oddProof, evenProof⟩
    case mpr =>
      simp [is_even, is_odd]
      sorry

end MyInt 
