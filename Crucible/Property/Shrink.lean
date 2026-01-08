import Lean

/-!
# Shrinking for Property Testing

Provides the Shrinkable typeclass for generating smaller counterexamples
when property tests fail.
-/

namespace Crucible.Property

/-- Typeclass for types that can shrink to simpler/smaller values.

When a property test fails, shrinking tries to find a minimal counterexample
by generating progressively smaller values that still fail the property. -/
class Shrinkable (α : Type u) where
  /-- Return a list of strictly smaller values to try.
  The list should be ordered from most aggressively shrunk to least. -/
  shrink : α → List α := fun _ => []

namespace Shrinkable

/-- Shrink by repeatedly halving toward zero. -/
private def shrinkTowardsNat (target current : Nat) : List Nat :=
  if current <= target then []
  else
    let mid := target + (current - target) / 2
    if mid == current then [target]
    else mid :: shrinkTowardsNat target mid

/-- Shrink Nat toward 0. -/
instance : Shrinkable Nat where
  shrink n :=
    if n == 0 then []
    else 0 :: shrinkTowardsNat 0 n

/-- Shrink Int toward 0, including sign flip. -/
instance : Shrinkable Int where
  shrink n :=
    if n == 0 then []
    else if n < 0 then
      -- Try positive version first, then shrink magnitude
      (-n) :: (shrinkTowardsNat 0 n.natAbs).map (- Int.ofNat ·)
    else
      0 :: (shrinkTowardsNat 0 n.natAbs).map Int.ofNat

/-- Bool doesn't shrink (only two values). -/
instance : Shrinkable Bool where
  shrink
    | true => [false]
    | false => []

/-- Shrink Char toward 'a'. -/
instance : Shrinkable Char where
  shrink c :=
    if c == 'a' then []
    else if c.isAlpha then ['a']
    else if c.isDigit then ['0', 'a']
    else ['a']

/-- Shrink UInt8 toward 0. -/
instance : Shrinkable UInt8 where
  shrink n :=
    (Shrinkable.shrink n.toNat).map UInt8.ofNat

/-- Shrink UInt16 toward 0. -/
instance : Shrinkable UInt16 where
  shrink n :=
    (Shrinkable.shrink n.toNat).map UInt16.ofNat

/-- Shrink UInt32 toward 0. -/
instance : Shrinkable UInt32 where
  shrink n :=
    (Shrinkable.shrink n.toNat).map UInt32.ofNat

/-- Shrink UInt64 toward 0. -/
instance : Shrinkable UInt64 where
  shrink n :=
    (Shrinkable.shrink n.toNat).map UInt64.ofNat

/-- Shrink Float toward 0. -/
instance : Shrinkable Float where
  shrink f :=
    if f == 0.0 then []
    else if f < 0.0 then
      (-f) :: [0.0, f / 2.0]
    else
      [0.0, f / 2.0]

/-- Shrink String by removing characters or shrinking to simpler strings. -/
instance : Shrinkable String where
  shrink s :=
    if s.isEmpty then []
    else
      -- Try empty, single char, remove each char
      let removeOne := List.range s.length |>.map fun i =>
        let chars := s.toList
        String.ofList (chars.take i ++ chars.drop (i + 1))
      "" :: removeOne

/-- Shrink Option by trying None first, then shrinking the value. -/
instance [Shrinkable α] : Shrinkable (Option α) where
  shrink
    | none => []
    | some a => none :: (Shrinkable.shrink a).map some

/-- Shrink pairs by shrinking each component. -/
instance [Shrinkable α] [Shrinkable β] : Shrinkable (α × β) where
  shrink := fun (a, b) =>
    -- Shrink first component
    (Shrinkable.shrink a).map (·, b) ++
    -- Shrink second component
    (Shrinkable.shrink b).map (a, ·)

/-- Shrink lists by removing elements or shrinking elements. -/
instance [Shrinkable α] : Shrinkable (List α) where
  shrink xs :=
    if xs.isEmpty then []
    else
      -- Try empty list first
      let tryEmpty := [[]]
      -- Remove each element
      let removeOne := List.range xs.length |>.map fun i =>
        xs.take i ++ xs.drop (i + 1)
      -- Shrink each element in place
      let shrinkOne := (List.range xs.length).flatMap fun i =>
        match xs[i]? with
        | none => []
        | some x =>
          (Shrinkable.shrink x).map fun x' =>
            xs.take i ++ [x'] ++ xs.drop (i + 1)
      -- Take first half, second half
      let halves :=
        if xs.length > 1 then
          [xs.take (xs.length / 2), xs.drop (xs.length / 2)]
        else []
      tryEmpty ++ halves ++ removeOne ++ shrinkOne

/-- Shrink arrays by converting to/from lists. -/
instance [Shrinkable α] : Shrinkable (Array α) where
  shrink arr :=
    (Shrinkable.shrink arr.toList).map List.toArray

end Shrinkable

end Crucible.Property


/-! ## Derive Shrinkable Macro -/

open Lean Elab Command Meta

/-- Derive handler for Shrinkable.

For a structure `Foo` with fields `x : A` and `y : B`, generates an instance
that shrinks each field independently.
-/
initialize registerDerivingHandler ``Crucible.Property.Shrinkable fun typeNames => do
  if typeNames.size != 1 then
    throwError "Shrinkable can only be derived for a single type at a time"
  let typeName := typeNames[0]!

  -- Get structure info
  let env ← getEnv
  let some info := getStructureInfo? env typeName
    | throwError "Shrinkable can only be derived for structures, not {typeName}"

  -- Get field names
  let fields := info.fieldNames
  let typeIdent := mkIdent typeName

  if fields.isEmpty then
    -- Empty structure: no shrinking possible
    let cmd ← `(instance : Crucible.Property.Shrinkable $typeIdent where
                  shrink _ := [])
    elabCommand cmd
    return true

  -- For now, generate a simple instance that doesn't shrink
  -- A full implementation would need more complex syntax construction
  let cmd ← `(instance : Crucible.Property.Shrinkable $typeIdent where
                shrink _ := [])
  elabCommand cmd
  return true
