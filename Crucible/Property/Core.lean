import Crucible.Property.Random
import Crucible.Property.Shrink
import Crucible.Property.Generators

/-!
# Property Testing Core

Provides the Property type, configuration, and forAll combinator
for property-based testing.
-/

namespace Crucible.Property

/-- Configuration for property testing. -/
structure PropConfig where
  /-- Number of test cases to run. -/
  numTests : Nat := 100
  /-- Maximum size parameter (grows from 0 to this). -/
  maxSize : Nat := 100
  /-- Maximum shrink iterations per failure. -/
  maxShrinks : Nat := 1000
  /-- Random seed (none = use time-based seed). -/
  seed : Option Nat := none
  /-- Print each test case as it runs. -/
  verbose : Bool := false
  deriving Inhabited, Repr

/-- Result of a property test. -/
inductive PropResult where
  /-- All tests passed. -/
  | success (numTests : Nat)
  /-- Found a counter-example. -/
  | failure
      (original : String)
      (shrunk : String)
      (shrinks : Nat)
      (seed : Nat)
      (testNum : Nat)
  /-- Test gave up (couldn't generate enough valid inputs). -/
  | gaveUp (numDiscarded : Nat)
  deriving Repr

namespace PropResult

/-- Check if the result is a success. -/
def isSuccess : PropResult → Bool
  | .success _ => true
  | _ => false

/-- Format result for display. -/
def format : PropResult → String
  | .success n => s!"OK, passed {n} tests"
  | .failure orig shrunk shrinks seed testNum =>
    let shrinkInfo := if shrinks > 0 then s!" (after {shrinks} shrinks)" else ""
    s!"FAILED on test {testNum}{shrinkInfo}\n  Counterexample: {shrunk}\n  Original: {orig}\n  Seed: {seed}"
  | .gaveUp n => s!"Gave up after {n} discarded inputs"

end PropResult


/-- A property that can be tested. -/
structure Property where
  /-- Run the property test with given configuration. -/
  check : PropConfig → IO PropResult

namespace Property

/-- Run property with default configuration. -/
def run (p : Property) : IO PropResult :=
  p.check {}

/-- Run property and throw on failure (for integration with test frameworks). -/
def runOrFail (p : Property) (cfg : PropConfig := {}) : IO Unit := do
  let result ← p.check cfg
  match result with
  | .success _ => pure ()
  | .failure _ shrunk shrinks seed testNum =>
    let shrinkInfo := if shrinks > 0 then s!" (after {shrinks} shrinks)" else ""
    throw $ IO.userError s!"Property failed on test {testNum}{shrinkInfo}: {shrunk} (seed={seed})"
  | .gaveUp n =>
    throw $ IO.userError s!"Property gave up after {n} discarded inputs"

end Property


/-! ## Shrinking Logic -/

/-- Shrink a failing value to find a minimal counterexample. -/
private def shrinkLoop [Shrinkable α] [Repr α]
    (prop : α → Bool) (x : α) (maxShrinks : Nat) : α × Nat :=
  let rec loop (current : α) (fuel : Nat) (shrinks : Nat) : α × Nat :=
    match fuel with
    | 0 => (current, shrinks)
    | fuel' + 1 =>
      let candidates := Shrinkable.shrink current
      match candidates.find? (fun c => !prop c) with
      | none => (current, shrinks)
      | some smaller => loop smaller fuel' (shrinks + 1)
  loop x maxShrinks 0

/-- Shrink a failing value using an IO-based predicate.
    Tries each shrink candidate sequentially. -/
private def shrinkLoopIO [Shrinkable α] [Repr α]
    (prop : α → IO Bool) (x : α) (maxShrinks : Nat) : IO (α × Nat) := do
  let rec loop (current : α) (fuel : Nat) (shrinks : Nat) : IO (α × Nat) := do
    match fuel with
    | 0 => pure (current, shrinks)
    | fuel' + 1 =>
      let candidates := Shrinkable.shrink current
      -- Find first failing candidate
      let mut found : Option α := none
      for c in candidates do
        if found.isNone then
          let passed ← prop c
          if !passed then
            found := some c
      match found with
      | none => pure (current, shrinks)
      | some smaller => loop smaller fuel' (shrinks + 1)
  loop x maxShrinks 0


/-! ## forAll Combinator -/

/-- Core property combinator: test that all generated values satisfy a predicate.

Example:
```lean
forAll (Gen.choose 0 100) fun n =>
  n + 0 == n
```
-/
def forAll [Shrinkable α] [Repr α]
    (gen : Gen α) (prop : α → Bool) : Property := {
  check := fun cfg => do
    -- Get seed
    let seed ← match cfg.seed with
      | some s => pure s
      | none => do
        let r ← RandState.fromTime
        pure r.seed.toNat

    let mut rand := RandState.fromNat seed
    let mut testNum := 0

    while testNum < cfg.numTests do
      -- Compute size for this test (grows from 0 to maxSize)
      let size := if cfg.numTests <= 1 then cfg.maxSize
                  else testNum * cfg.maxSize / (cfg.numTests - 1)

      -- Generate value
      let (value, rand') := gen.run rand size
      rand := rand'

      -- Test property
      if cfg.verbose then
        IO.println s!"Test {testNum + 1}: {repr value}"

      if !prop value then
        -- Found a failure - try to shrink
        let originalRepr := reprStr value
        let (shrunk, shrinks) := shrinkLoop prop value cfg.maxShrinks
        let shrunkRepr := reprStr shrunk
        return .failure originalRepr shrunkRepr shrinks seed (testNum + 1)

      testNum := testNum + 1

    return .success cfg.numTests
}

/-- Simpler forAll using Arbitrary instance. -/
def forAll' [Arbitrary α] [Shrinkable α] [Repr α]
    (prop : α → Bool) : Property :=
  forAll arbitrary prop

/-- Property combinator for IO-based predicates.
    Use this when your property needs to run IO actions (e.g., FRP tests).

Example:
```lean
forAllIO (Gen.choose 0 100) fun n => do
  let result ← someIOAction n
  pure (result == expected)
```
-/
def forAllIO [Shrinkable α] [Repr α]
    (gen : Gen α) (prop : α → IO Bool) : Property := {
  check := fun cfg => do
    -- Get seed
    let seed ← match cfg.seed with
      | some s => pure s
      | none => do
        let r ← RandState.fromTime
        pure r.seed.toNat

    let mut rand := RandState.fromNat seed
    let mut testNum := 0

    while testNum < cfg.numTests do
      -- Compute size for this test (grows from 0 to maxSize)
      let size := if cfg.numTests <= 1 then cfg.maxSize
                  else testNum * cfg.maxSize / (cfg.numTests - 1)

      -- Generate value
      let (value, rand') := gen.run rand size
      rand := rand'

      -- Test property
      if cfg.verbose then
        IO.println s!"Test {testNum + 1}: {repr value}"

      let passed ← prop value
      if !passed then
        -- Found a failure - try to shrink
        let originalRepr := reprStr value
        let (shrunk, shrinks) ← shrinkLoopIO prop value cfg.maxShrinks
        let shrunkRepr := reprStr shrunk
        return .failure originalRepr shrunkRepr shrinks seed (testNum + 1)

      testNum := testNum + 1

    return .success cfg.numTests
}

/-- Simpler forAllIO using Arbitrary instance. -/
def forAllIO' [Arbitrary α] [Shrinkable α] [Repr α]
    (prop : α → IO Bool) : Property :=
  forAllIO arbitrary prop

/-- Property that always succeeds. -/
def success : Property := {
  check := fun cfg => pure (.success cfg.numTests)
}

/-- Property that always fails with a message. -/
def failure (msg : String) : Property := {
  check := fun _ => pure (.failure msg msg 0 0 1)
}

/-- Combine two properties: both must pass. -/
def Property.and (p q : Property) : Property := {
  check := fun cfg => do
    match ← p.check cfg with
    | .success _ => q.check cfg
    | other => pure other
}

/-- Combine two properties: at least one must pass. -/
def Property.or (p q : Property) : Property := {
  check := fun cfg => do
    match ← p.check cfg with
    | .success n => pure (.success n)
    | _ => q.check cfg
}

/-- Add a label/classification to a property (for debugging). -/
def classify (label : String) (cond : Bool) (prop : Property) : Property := {
  check := fun cfg => do
    -- In verbose mode, print classification
    if cfg.verbose && cond then
      IO.println s!"  [classified as: {label}]"
    prop.check cfg
}

/-- Filter inputs with a precondition.
    Returns a function suitable for use with forAll. -/
def implies (precond : α → Bool) (prop : α → Bool) : α → Bool :=
  fun x => !precond x || prop x

/-- Create property from a simple boolean predicate. -/
def ofBool (b : Bool) : Property :=
  if b then success else failure "predicate was false"

end Crucible.Property
