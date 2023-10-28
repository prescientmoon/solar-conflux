namespace Nat
  theorem add_equations: ∀{a b c d: Nat}, a = b → c = d → a + c = b + d := by
    intro a b c d ab cd
    rw [ab, cd]

  theorem add_to_equation_right: ∀{a b c: Nat}, a = b → a + c = b + c := by
    intro a b c ab
    exact add_equations ab rfl

  theorem succ_equation : ∀{a b: Nat}, Nat.succ a = Nat.succ b → a = b := by
    intro a b
    apply Eq.mp -- (a + 1 = b + 1) = a + b
    apply Nat.succ.injEq -- this was already here?

  inductive A : Nat -> Type
    | ooo: (n: Nat) → A n 

  theorem subtract_to_equation_right: ∀{a b c: Nat}, a + c = b + c → a = b := by
    intro a b c acbc
    induction c with
    | zero => 
      repeat rw [Nat.add_zero] at acbc
      exact acbc
    | succ pc inner => 
      /- 
        a + S pc = b + S pc -- comm
        S pc + a = S pc + b -- addition definition
        S (pc + a) = S (pc + b) -- injective constructor
        pc + a = pc + b
      -/
      rw [Nat.add_comm a (Nat.succ pc), Nat.add_comm b (Nat.succ pc)] at acbc
      simp [Nat.succ_add, Nat.add_comm] at acbc
      exact (inner acbc)

  theorem subtract_to_equation_left: ∀{a b c: Nat}, c + a = c + b → a = b := by
    intro a b c cacb
    rw [Nat.add_comm c a, Nat.add_comm c b] at cacb
    exact (subtract_to_equation_right cacb)

  theorem add_equation_both_sides_right: ∀{a b c: Nat}, (a = b) ↔ (a + c = b + c) := by
    intro a b c
    apply Iff.intro
    . exact add_to_equation_right
    . exact subtract_to_equation_right

  theorem multiply_equation_left: ∀{a b c: Nat}, (a = b) → (c * a = c * b) := by 
    intro a b c ab
    rw [ab]

  theorem double.addition_is_multiplication (x: Nat): 2 * x = x + x := by
    induction x with
    | zero => simp
    | succ px ic => 
      simp [<-Nat.add_one, Nat.left_distrib, ic, Nat.add_left_comm, Nat.add_assoc]
end Nat
