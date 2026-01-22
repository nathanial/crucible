import Crucible

/-!
# Property Testing Framework Tests

Tests for the property-based testing module itself.
-/

namespace Crucible.Tests.Property

open Crucible
open Crucible.Property

testSuite "Property Testing Framework"

/-! ## Generator Tests -/

test "Gen.choose produces values in range" := do
  let samples := Gen.sampleMany (Gen.choose 10 20) 12345 100
  for x in samples do
    ensure (x >= 10 && x <= 20) s!"Value {x} out of range [10, 20]"

test "Gen.bool produces both values" := do
  let samples := Gen.sampleMany Gen.bool 12345 100
  ensure (samples.any (· == true)) "No true values generated"
  ensure (samples.any (· == false)) "No false values generated"

test "Gen.listOf respects size" := do
  let list := Gen.sample (Gen.listOf (Gen.choose 0 10)) 12345 5
  ensure (list.length <= 5) s!"List too long: {list.length}"

test "Gen.elements picks from list" := do
  let choices := [1, 2, 3, 4, 5]
  let samples := Gen.sampleMany (Gen.elements choices) 12345 50
  for x in samples do
    ensure (choices.contains x) s!"Value {x} not in choices"

/-! ## Shrinking Tests -/

test "Nat shrinks toward 0" := do
  let shrunk := Shrinkable.shrink (100 : Nat)
  ensure (!shrunk.isEmpty) "Nat 100 should have shrink candidates"
  ensure (shrunk.contains 0) "Nat should shrink to 0"

test "Int shrinks toward 0" := do
  let shrunkPos := Shrinkable.shrink (100 : Int)
  ensure (shrunkPos.contains 0) "Positive Int should shrink to 0"
  let shrunkNeg := Shrinkable.shrink (-100 : Int)
  ensure (shrunkNeg.contains 100) "Negative Int should try positive"
  ensure (shrunkNeg.any (· == 0) || shrunkNeg.any (fun x => (Shrinkable.shrink x).contains 0))
    "Negative Int should eventually reach 0"

test "List shrinks by removing elements" := do
  let shrunk := Shrinkable.shrink [1, 2, 3]
  ensure (shrunk.contains []) "List should shrink to empty"
  ensure (shrunk.any (·.length == 2)) "List should try removing one element"

test "Option shrinks to none" := do
  let shrunk := Shrinkable.shrink (some 42 : Option Nat)
  ensure (shrunk.contains none) "Option should shrink to none"

/-! ## Property Tests -/

proptest "addition is commutative" :=
  forAll' fun (xy : Int × Int) =>
    xy.1 + xy.2 == xy.2 + xy.1

proptest "multiplication identity" :=
  forAll' fun (x : Int) =>
    x * 1 == x

proptest "addition identity" :=
  forAll' fun (x : Nat) =>
    x + 0 == x

proptest "list append length" :=
  forAll (Gen.pair (Gen.listOf (Gen.choose 0 10)) (Gen.listOf (Gen.choose 0 10))) fun (xs, ys) =>
    (xs ++ ys).length == xs.length + ys.length

proptest "list reverse twice is identity" :=
  forAll (Gen.listOf (Gen.choose 0 100)) fun xs =>
    xs.reverse.reverse == xs

proptest "with explicit seed" (seed := 42) :=
  forAll' fun (n : Nat) =>
    n + 0 == n

proptest "with custom test count" (tests := 50) :=
  forAll' fun (x : Int) =>
    x - x == 0

/-! ## Custom Type with Deriving -/

structure Point where
  x : Int
  y : Int
  deriving Repr, BEq, Arbitrary, Shrinkable

proptest "point subtraction gives zero" :=
  forAll' fun (p : Point) =>
    p.x - p.x == 0 && p.y - p.y == 0

structure Pair (α : Type) where
  fst : α
  snd : α
  deriving Repr, BEq

instance [Arbitrary α] : Arbitrary (Pair α) where
  arbitrary := do
    let a ← arbitrary
    let b ← arbitrary
    pure { fst := a, snd := b }

instance [Shrinkable α] : Shrinkable (Pair α) where
  shrink p :=
    (Shrinkable.shrink p.fst).map (fun x => { p with fst := x }) ++
    (Shrinkable.shrink p.snd).map (fun x => { p with snd := x })

proptest "pair swap twice is identity" :=
  forAll' fun (p : Pair Int) =>
    let swapped := { fst := p.snd, snd := p.fst : Pair Int }
    let swappedBack := { fst := swapped.snd, snd := swapped.fst }
    swappedBack == p

/-! ## Edge Cases -/

test "empty property succeeds" := do
  let result ← Property.success.run
  ensure result.isSuccess "Empty property should succeed"

test "failure property fails" := do
  let result ← (Property.failure "expected").run
  ensure (!result.isSuccess) "Failure property should fail"

end Crucible.Tests.Property
