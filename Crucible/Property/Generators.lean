import Crucible.Property.Random
import Lean

/-!
# Generators for Property Testing

Provides the Arbitrary typeclass and generator combinators for
producing random test values.
-/

namespace Crucible.Property

/-- Typeclass for types that can generate arbitrary random values. -/
class Arbitrary (α : Type u) where
  /-- Generator for random values of this type. -/
  arbitrary : Gen α

export Arbitrary (arbitrary)

/-! ## Generator Combinators (needed before Arbitrary instances) -/

namespace Gen

/-- Generate list of exactly n elements. -/
def listOfN (n : Nat) (g : Gen α) : Gen (List α) :=
  ⟨fun r size =>
    let rec go (r : RandState) (remaining : Nat) (acc : List α) : List α × RandState :=
      match remaining with
      | 0 => (acc.reverse, r)
      | m + 1 =>
        let (x, r') := g.run r size
        go r' m (x :: acc)
    go r n []⟩

/-- Generate list with size-dependent length. -/
def listOf (g : Gen α) : Gen (List α) :=
  sized fun size => do
    let len ← choose 0 size
    listOfN len g

/-- Generate non-empty list. -/
def listOf1 (g : Gen α) : Gen (List α) :=
  sized fun size => do
    let len ← choose 1 (max 1 size)
    listOfN len g

/-- Generate array with size-dependent length. -/
def arrayOf (g : Gen α) : Gen (Array α) := do
  let xs ← listOf g
  pure xs.toArray

/-- Generate Option with given probability of Some (0.0 to 1.0). -/
def optionOf (g : Gen α) (someProb : Float := 0.67) : Gen (Option α) := do
  let f ← float01
  if f < someProb then some <$> g
  else pure none

/-- Filter generated values (returns None if too many failures). -/
def filter (g : Gen α) (p : α → Bool) (maxTries : Nat := 100) : Gen (Option α) :=
  let rec loop : Nat → Gen (Option α)
    | 0 => pure none
    | n + 1 => do
      let x ← g
      if p x then pure (some x)
      else loop n
  loop maxTries

/-- Generate values satisfying predicate (uses default if too many failures). -/
def suchThat [Inhabited α] (g : Gen α) (p : α → Bool) : Gen α := do
  match ← filter g p 100 with
  | some x => pure x
  | none => pure default

/-- Generate pair of values. -/
def pair (ga : Gen α) (gb : Gen β) : Gen (α × β) := do
  let a ← ga
  let b ← gb
  pure (a, b)

/-- Generate triple of values. -/
def triple (ga : Gen α) (gb : Gen β) (gc : Gen γ) : Gen (α × β × γ) := do
  let a ← ga
  let b ← gb
  let c ← gc
  pure (a, b, c)

/-- Scale the size parameter for nested generation. -/
def scale (f : Nat → Nat) (g : Gen α) : Gen α :=
  resize f g

/-- Generate with smaller size (useful for recursive structures). -/
def smaller (g : Gen α) : Gen α :=
  scale (· / 2) g

/-- Pick uniformly from a non-empty list. -/
def elements [Inhabited α] (xs : List α) : Gen α := do
  if xs.length > 0 then
    let idx ← choose 0 (xs.length - 1)
    pure (xs.getD idx default)
  else
    pure default

/-- Pick uniformly from multiple generators. -/
def oneOf [Inhabited α] (gs : List (Gen α)) : Gen α := do
  if gs.length > 0 then
    let idx ← choose 0 (gs.length - 1)
    gs.getD idx (pure default)
  else
    pure default

/-- Pick from generators with weights.
    frequency [(3, genA), (1, genB)] picks genA 75% of the time. -/
def frequency [Inhabited α] (gs : List (Nat × Gen α)) : Gen α := do
  let total := gs.foldl (fun acc (w, _) => acc + w) 0
  if total == 0 then
    match gs.head? with
    | some (_, g) => g
    | none => pure default
  else
    let target ← choose 0 (total - 1)
    let rec pick (remaining : Nat) : List (Nat × Gen α) → Gen α
      | [] => pure default
      | (w, g) :: rest =>
        if remaining < w then g
        else pick (remaining - w) rest
    pick target gs

end Gen


/-! ## Arbitrary Instances -/

namespace Arbitrary

/-- Nat generator: size-dependent range [0, size]. -/
instance : Arbitrary Nat where
  arbitrary := Gen.sized fun size => Gen.choose 0 size

/-- Int generator: size-dependent range [-size, size]. -/
instance : Arbitrary Int where
  arbitrary := Gen.sized fun size =>
    Gen.chooseInt (-Int.ofNat size) (Int.ofNat size)

/-- Bool generator: 50/50 probability. -/
instance : Arbitrary Bool where
  arbitrary := Gen.bool

/-- Char generator: alphanumeric characters. -/
instance : Arbitrary Char where
  arbitrary := do
    let choice ← Gen.choose 0 2
    match choice with
    | 0 => do -- lowercase letter
      let n ← Gen.choose 0 25
      pure (Char.ofNat (97 + n))
    | 1 => do -- uppercase letter
      let n ← Gen.choose 0 25
      pure (Char.ofNat (65 + n))
    | _ => do -- digit
      let n ← Gen.choose 0 9
      pure (Char.ofNat (48 + n))

/-- UInt8 generator. -/
instance : Arbitrary UInt8 where
  arbitrary := do
    let n ← Gen.choose 0 255
    pure (UInt8.ofNat n)

/-- UInt16 generator. -/
instance : Arbitrary UInt16 where
  arbitrary := do
    let n ← Gen.choose 0 65535
    pure (UInt16.ofNat n)

/-- UInt32 generator: size-dependent. -/
instance : Arbitrary UInt32 where
  arbitrary := Gen.sized fun size =>
    Gen.choose 0 (min size 4294967295) |>.map UInt32.ofNat

/-- UInt64 generator: size-dependent. -/
instance : Arbitrary UInt64 where
  arbitrary := Gen.sized fun size =>
    Gen.choose 0 size |>.map UInt64.ofNat

/-- Float generator: [-size, size] range. -/
instance : Arbitrary Float where
  arbitrary := Gen.sized fun size => do
    let f ← Gen.float01
    pure ((f * 2.0 - 1.0) * size.toFloat)

/-- String generator: size-dependent length. -/
instance : Arbitrary String where
  arbitrary := Gen.sized fun size => do
    let len ← Gen.choose 0 size
    let chars ← Gen.listOfN len (arbitrary : Gen Char)
    pure (String.ofList chars)

/-- Option generator: ~33% None, ~67% Some. -/
instance [Arbitrary α] : Arbitrary (Option α) where
  arbitrary := do
    let choice ← Gen.choose 0 2
    if choice == 0 then pure none
    else some <$> arbitrary

/-- Pair generator. -/
instance [Arbitrary α] [Arbitrary β] : Arbitrary (α × β) where
  arbitrary := do
    let a ← arbitrary
    let b ← arbitrary
    pure (a, b)

/-- List generator: size-dependent length. -/
instance [Arbitrary α] : Arbitrary (List α) where
  arbitrary := Gen.sized fun size => do
    let len ← Gen.choose 0 size
    Gen.listOfN len arbitrary

/-- Array generator: size-dependent length. -/
instance [Arbitrary α] : Arbitrary (Array α) where
  arbitrary := do
    let xs ← (arbitrary : Gen (List α))
    pure xs.toArray

end Arbitrary

end Crucible.Property


/-! ## Derive Arbitrary Macro -/

open Lean Elab Command Meta

/-- Derive handler for Arbitrary.

For a structure `Foo` with fields `x : A` and `y : B`, generates:
```
instance : Arbitrary Foo where
  arbitrary := do
    let x ← arbitrary
    let y ← arbitrary
    pure (Foo.mk x y)
```
-/
initialize registerDerivingHandler ``Crucible.Property.Arbitrary fun typeNames => do
  if typeNames.size != 1 then
    throwError "Arbitrary can only be derived for a single type at a time"
  let typeName := typeNames[0]!

  -- Get structure info
  let env ← getEnv
  let some info := getStructureInfo? env typeName
    | throwError "Arbitrary can only be derived for structures, not {typeName}"

  -- Get field names
  let fields := info.fieldNames
  let typeIdent := mkIdent typeName

  if fields.isEmpty then
    -- Empty structure: just construct it
    let ctorIdent := mkIdent (typeName ++ `mk)
    let cmd ← `(instance : Crucible.Property.Arbitrary $typeIdent where
                  arbitrary := pure $ctorIdent)
    elabCommand cmd
    return true

  -- Build the do block with let bindings and constructor call
  let mut stmts : Array (TSyntax `Lean.Parser.Term.doSeqItem) := #[]

  -- Generate: let field ← arbitrary for each field
  for field in fields do
    let fieldIdent := mkIdent field
    let stmt ← `(Lean.Parser.Term.doSeqItem| let $fieldIdent ← Crucible.Property.arbitrary)
    stmts := stmts.push stmt

  -- Generate: pure (Foo.mk field1 field2 ...)
  let ctorIdent := mkIdent (typeName ++ `mk)
  let fieldIdents := fields.map fun f => mkIdent f
  -- Build constructor application term
  let mut ctorApp : TSyntax `term := ctorIdent
  for fieldIdent in fieldIdents do
    ctorApp ← `($ctorApp $fieldIdent)

  let returnStmt ← `(Lean.Parser.Term.doSeqItem| pure $ctorApp)
  stmts := stmts.push returnStmt

  let doBlock ← `(do $stmts*)

  let cmd ← `(instance : Crucible.Property.Arbitrary $typeIdent where
                arbitrary := $doBlock)
  elabCommand cmd
  return true
