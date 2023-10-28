-- single line comment
universe u

def m: Nat := 1

#check m
#check m

#check (m + m)
#eval (m + m)

#check Nat × Nat

def Γ := 2 + m
 

def double (a: Nat) := a + a

#eval double (double 3)

section composition
  variable (α β γ: Type)

  def compose (f: α → β) (g: β → γ): (α -> γ) := fun x: α => g (f x)
end composition

def quadruble := compose _ _ _ double double

#print compose

section composition'
  variable (α β γ : Type)
  variable (g : β → γ) (f : α → β) (h : α → α)
  variable (x : α)

  def compose' := g (f x)
  def doTwice := h (h x)
  def doThrice := h (h (h x))
end composition'

def a: Nat := 3

#eval double a
#print compose'
#check (Nat : Sort 1)
#check (Prop : Sort 1)
#check (Prop : Type)


section proofs 
  theorem hmm (α: Prop) (β: Prop): Prop := α
  theorem hmm2 : Prop -> Prop -> Prop := 
    fun α β => show Prop from α

  theorem hmm3 (hm: α): α := hm

  axiom myProof: Prop

  #check hmm
  #check hmm2
  #check hmm3 myProof
  #check @hmm3
  #check @hmm3 myProof
end proofs

section logic
  variable {α β π: Prop}
  variable (ha: α) (hb: β)

  theorem pair: α -> β -> α ∧ β := fun a b => And.intro a b
  theorem unpair: (α → β → π) → α ∧ β → π := fun f a => 
    show π from f (And.left a) (And.right a)

  theorem pairsCommute: (α ∧ β) → (β ∧ α) := fun p => 
    show (β ∧ α) from And.intro (And.right p) (And.left p)

  example (h: α ∧ β): β ∧ α := ⟨h.right, h.left⟩

  theorem thrice: (f: α) -> α ∧ α ∧ α := fun f => ⟨ f, f, f ⟩

  theorem negateFunc: (β → α) → ¬α → ¬β := 
    fun f notₐ b => 
      show False from notₐ (f b)

  #print pairsCommute 
  #check (⟨ha, hb⟩: α ∧ β)

  theorem exFalso: (α ∧ ¬ α) → β :=
    fun contradiction => 
      show β from (contradiction.right contradiction.left).elim

  theorem pairIso: α ∧ β ↔ β ∧ α := ⟨ pairsCommute, pairsCommute ⟩
end logic

section classical
  open Classical 

  variable (α : Prop)

  def dneT: Prop := ¬¬α → α

  theorem doubleNegation: α ↔ ¬¬α :=
    suffices l: α → ¬¬α from 
    suffices r: ¬¬α → α from ⟨ l, r ⟩
    show dneT α from fun doubleNegA => (em α).elim
      (fun p: α => p)
      (fun p: ¬α => absurd p doubleNegA)
    show α → ¬¬α from (fun a f => f a)

  #print byCases

  theorem dne: dneT α := fun nnₚ =>
    byCases id
      (fun nₚ => (nnₚ nₚ).elim)

  #print byContradiction

  theorem dne': dneT α := fun nnₚ =>
    byContradiction fun nₚ => nnₚ nₚ 
end classical

section exercises
  variable (p q r : Prop)
    
  theorem noContradictions : ¬(p ↔ ¬p) := 
    fun contradiction => 
      suffices someP: p from contradiction.mp someP someP
      suffices someNotP: ¬p from show p from contradiction.mpr someNotP
      show ¬p from fun p => contradiction.mp p p
end exercises

section quantifiers
  variable (α: Type) (r: α → α → Prop)
  variable (transitivity: ∀x y z, r x y → r y z → r x z)
  variable (n: Nat)

  variable (trans2: ∀x y z, r x y → r y z → r x z)
  variable (trans3: ∀τ β ω, r τ β → r β ω → r τ ω)

  axiom someA: α 

  #print someA
  #check transitivity

  #print Eq.subst

  theorem substTest: (a b c: Nat) → (myProp: Nat -> Prop) → 
    (a + b = c) → myProp c → myProp (a + b) :=
    fun a b c myProp p₁ l  => @Eq.subst Nat (fun a => myProp a) c (a + b) p₁.symm l
  theorem substTest': {a b c: Nat} → {myProp: Nat -> Prop} → 
    (a + b = c) → myProp c → myProp (a + b) :=
    fun p₁ l  => p₁ ▸ l

  #check @congr
  #print congrArg
  #print congrFun

  theorem congrArgTest: {a b: Nat} → (c: Nat) → (a = b) → (a + c = b + c) :=
    fun c p₁ => congrArg (fun a => a + c) p₁

end quantifiers

section calc_
  variable (a b c d e : Nat)
  variable (h1 : a = b)
  variable (h2 : b = c + 1)
  variable (h3 : c = d)
  variable (h4 : e = 1 + d)

  theorem T : a = e :=
    calc
      a = b      := h1
      _ = c + 1  := h2
      _ = d + 1  := congrArg Nat.succ h3
      _ = 1 + d  := Nat.add_comm d 1
      _ = e      := h4.symm

  theorem T₂ : a = e :=
    calc
      a = b := h1
      _ = c + 1 := h2
      _ = d + 1 := by rw [h3]
      _ = 1 + d := by rw [Nat.add_comm]
      _ = e     := by rw [h4]

  theorem T₃ : a = e :=
    by rw [h1, h2, h3, Nat.add_comm, h4]

  theorem T₄ : a = e := 
    by simp [h1, h2, h3, Nat.add_comm, h4]

  example (a b c d : Nat) (h1 : a = b) (h2 : b ≤ c) (h3 : c + 1 < d) : a < d :=
    calc
      a = b     := h1
      _ < b + 1 := Nat.lt_succ_self b
      _ ≤ c + 1 := Nat.succ_le_succ h2
      _ < d     := h3

  variable (x y: Nat)

  theorem A₁: (x + y) * (x + y) = x * x + y * x + x * y + y * y :=
    calc
      (x + y) * (x + y) = (x + y) * x + (x + y) * y  := by rw [Nat.mul_add]
          _ = x * x + y * x + (x * y + y * y)        := by simp [Nat.add_mul]
          _ = x * x + y * x + x * y + y * y          := by rw [←Nat.add_assoc]

  theorem A₂: (x + y) * (x + y) = x * x + y * x + x * y + y * y :=
    by simp [Nat.add_mul, Nat.mul_add, Nat.add_assoc]

  axiom forallTest: ¬(∀a: Nat, a + a = a + 1)

  theorem existsOne: ∃a: Nat, a + a = a + 1 := ⟨ 1, rfl ⟩
  theorem existsBiggerThanOne: ∃a: Nat, a > 1 := 
    have r: 1 < 2 := Nat.succ_lt_succ (Nat.zero_lt_succ 0)
    ⟨ 2, r ⟩

  example (x : Nat) (h : x > 0) : ∃ y, y < x :=
    ⟨ 0, h ⟩
end calc_

section exists_print
  variable (g : Nat → Nat → Nat)
  variable (hg : g 0 0 = 0)

  theorem gex1 : ∃ x, g x x = x := ⟨0, hg⟩
  theorem gex2 : ∃ x, g x 0 = x := ⟨0, hg⟩
  theorem gex3 : ∃ x, g 0 0 = x := ⟨0, hg⟩
  theorem gex4 : ∃ x, g x x = 0 := ⟨0, hg⟩

  theorem gex5 : ∃ x, g x x = 0 := ⟨0, hg⟩
  theorem gex6 : ∃ x, g x x = 0 := ⟨0, hg⟩ 
  theorem gex7 : ∃ x, g x x = 0 := ⟨0, hg⟩

  set_option pp.explicit true  -- display implicit arguments
  #print gex1
  #print gex2
  #print gex3
  #print gex4
end exists_print

section existentials 
  variable (α : Type) (p q : α → Prop)

  example (h : ∃ x, p x ∧ q x) : ∃ x, q x ∧ p x :=
    match h with
    | ⟨w, hw⟩ => ⟨w, hw.right, hw.left⟩

  theorem _matches : (n: Nat) → n + 1 = 1 + n := fun n =>
    match n with
    | Nat.succ w => calc
        Nat.succ w + 1 = Nat.succ (w + 1) := rfl
        _              = Nat.succ (1 + w) := by rw [_matches w]
        _              = 1 + Nat.succ w := rfl
    | Nat.zero => rfl
end existentials

section tactics
  theorem test (p q : Prop) (hp: p) (hq: q): p ∧ q ∧ p := by
    apply And.intro
    case left => 
      exact hp
    case right =>
      apply And.intro hq hp

  #print test

  theorem test₂ (p q r : Prop) : p ∧ (q ∨ r) ↔ (p ∧ q) ∨ (p ∧ r) := by
    apply Iff.intro
    . intro h
      apply Or.elim h.right
      . intro hq
        apply Or.inl
        exact ⟨h.left, hq⟩
      . intro hr 
        apply Or.inr
        exact ⟨h.left, hr⟩
    . intro h
      apply Or.elim h
      . intro pq 
        apply And.intro 
        . exact pq.left
        . exact Or.inl pq.right
      . intro pr 
        apply And.intro
        . exact pr.left
        . exact Or.inr pr.right

  #print test₂
end tactics
