# API Reference

Complete reference for Crucible's public API.

## Core Types

### TestCase

A single test with metadata.

```lean
structure TestCase where
  name : String
  run : IO Unit
  timeoutMs : Option Nat := none
  retryCount : Option Nat := none
  skip : Option SkipReason := none
  xfail : Bool := false
  xfailReason : Option String := none
```

### TestResults

Results from running test suites.

```lean
structure TestResults where
  suites : Array SuiteResult := #[]
  totalElapsedMs : Nat := 0
```

**Properties:**
- `suiteCount : Nat` - Number of suites run
- `passed : Nat` - Total passed tests
- `failed : Nat` - Total failed tests
- `skipped : Nat` - Total skipped tests
- `xfailed : Nat` - Expected failures that failed (good)
- `xpassed : Nat` - Expected failures that passed (bad)
- `total : Nat` - Total tests run (excludes skipped)
- `allPassed : Bool` - True if no failures
- `toExitCode : UInt32` - 0 if passed, 1 if failed

### SuiteResult

Results for a single suite.

```lean
structure SuiteResult where
  name : String
  passed : Nat := 0
  failed : Nat := 0
  skipped : Nat := 0
  xfailed : Nat := 0
  xpassed : Nat := 0
  elapsedMs : Nat := 0
```

### Fixture

Setup/teardown hooks for a test suite.

```lean
structure Fixture where
  beforeAll : Option (IO Unit) := none
  afterAll : Option (IO Unit) := none
  beforeEach : Option (IO Unit) := none
  afterEach : Option (IO Unit) := none
```

### SoftAssertContext

Context for soft assertions.

```lean
structure SoftAssertContext where
  failures : IO.Ref (Array String)
```

## Test Definition

### `test` Macro

Define a test case.

```lean
test "name" := do
  -- test body

test "name" (timeout := ms) := do ...
test "name" (retry := count) := do ...
test "name" (skip := "reason") := do ...
test "name" (skip) := do ...  -- unconditional skip
test "name" (xfail := "reason") := do ...
```

### `testSuite` Command

Declare a test suite in the current namespace.

```lean
testSuite "Suite Name"
```

### `#generate_tests` Command

Collect all tests defined in the current namespace.

```lean
#generate_tests
```

## Test Runners

### `runAllSuites`

Run all registered test suites.

```lean
runAllSuites : IO UInt32
runAllSuites (timeout := ms) : IO UInt32
runAllSuites (retry := count) : IO UInt32
runAllSuites (timeout := ms) (retry := count) : IO UInt32
```

### `runAllSuitesWithResults`

Run suites and return structured results.

```lean
runAllSuitesWithResults : IO TestResults
runAllSuitesWithResults (timeout := ms) : IO TestResults
```

### `runAllSuitesFiltered`

Run suites with CLI filtering.

```lean
runAllSuitesFiltered (args : List String) : IO UInt32
runAllSuitesFiltered args (timeout := ms) : IO UInt32
runAllSuitesFiltered args (retry := count) : IO UInt32
```

### `runAllSuitesFilteredWithResults`

Run filtered suites with structured results.

```lean
runAllSuitesFilteredWithResults (args : List String) : IO TestResults
```

## Assertions

### Equality

```lean
-- Infix (recommended)
actual ≡ expected      -- \equiv
optVal ≡? expected     -- \equiv?

-- Function
shouldBe actual expected
```

### Options

```lean
shouldBeSome (opt : Option α) (expected : α)
shouldBeNone (opt : Option α)
```

### Boolean

```lean
ensure (cond : Bool) (msg : String)
shouldSatisfy (cond : Bool) (msg : String)
shouldMatch (actual : α) (pred : α → Bool) (desc : String)
```

### Numeric

```lean
shouldBeNear (actual expected : Float) (eps : Float := 0.0001)
shouldBeApprox (actual expected : Float) (eps : Float := 0.0001)
shouldBeBetween (actual : α) (min max : α)  -- requires Ord
```

### Collections

```lean
shouldHaveLength (actual : List α) (expected : Nat)
shouldContain (actual : List α) (expected : α)
shouldContainAll (actual : List α) (expected : List α)
shouldBeEmpty (actual : List α)
shouldNotBeEmpty (actual : List α)
```

### Strings

```lean
shouldStartWith (actual : String) (prefix : String)
shouldEndWith (actual : String) (suffix : String)
shouldContainSubstr (actual : String) (substring : String)
```

### Exceptions

```lean
shouldThrow (action : IO α)
shouldThrowWith (action : IO α) (substring : String)
shouldThrowMatching (action : IO α) (pred : String → Bool)
shouldNotThrow (action : IO α)
```

### Except

```lean
shouldBeOk (result : Except ε α) (context : String := "Operation") : IO α
shouldBeErr (result : Except ε α)
```

### Context

```lean
withContext (assertion : IO Unit) (context : String)
withMessage (msg : String) (assertion : IO Unit)
```

## Soft Assertions

### `withSoftAsserts`

Create soft assertion context.

```lean
withSoftAsserts (block : SoftAssertContext → IO Unit) : IO Unit
```

### Soft Assertion Methods

All methods on `SoftAssertContext`:

```lean
soft.ensure (cond : Bool) (msg : String)
soft.shouldBe actual expected
soft.shouldBeNear actual expected eps
soft.shouldBeSome opt expected
soft.shouldBeNone opt
soft.shouldSatisfy cond msg
soft.shouldMatch actual pred desc
soft.shouldHaveLength list n
soft.shouldContain list elem
soft.shouldContainAll list elems
soft.shouldStartWith str prefix
soft.shouldEndWith str suffix
soft.shouldContainSubstr str sub
soft.shouldBeBetween val min max
soft.shouldBeEmpty list
soft.shouldNotBeEmpty list
```

## Property Testing

### `proptest` Command

Define a property test.

```lean
proptest "description" := property
proptest "description" (tests := count) := property
proptest "description" (seed := n) := property
```

### Property Combinators

```lean
forAll (gen : Gen α) (prop : α → Bool) : Property
forAll' (prop : α → Bool) : Property  -- uses Arbitrary instance
```

### PropConfig

```lean
structure PropConfig where
  numTests : Nat := 100
  maxSize : Nat := 100
  maxShrinks : Nat := 1000
  seed : Option Nat := none
  verbose : Bool := false
```

### Gen Combinators

```lean
Gen.choose (lo hi : Nat) : Gen Nat
Gen.chooseInt (lo hi : Int) : Gen Int
Gen.bool : Gen Bool
Gen.float01 : Gen Float
Gen.elements (xs : List α) : Gen α
Gen.oneOf (gs : List (Gen α)) : Gen α
Gen.frequency (gs : List (Nat × Gen α)) : Gen α
Gen.listOf (g : Gen α) : Gen (List α)
Gen.listOfN (n : Nat) (g : Gen α) : Gen (List α)
Gen.listOf1 (g : Gen α) : Gen (List α)
Gen.arrayOf (g : Gen α) : Gen (Array α)
Gen.optionOf (g : Gen α) : Gen (Option α)
Gen.pair (ga : Gen α) (gb : Gen β) : Gen (α × β)
Gen.triple (ga gb gc) : Gen (α × β × γ)
Gen.sized (f : Nat → Gen α) : Gen α
Gen.resize (f : Nat → Nat) (g : Gen α) : Gen α
Gen.scale (f : Nat → Nat) (g : Gen α) : Gen α
Gen.smaller (g : Gen α) : Gen α
Gen.filter (g : Gen α) (p : α → Bool) : Gen (Option α)
Gen.suchThat (g : Gen α) (p : α → Bool) : Gen α
```

### Arbitrary Typeclass

```lean
class Arbitrary (α : Type u) where
  arbitrary : Gen α
```

**Deriving:** Use `deriving Arbitrary` on structures.

### Shrinkable Typeclass

```lean
class Shrinkable (α : Type u) where
  shrink : α → List α
```

**Deriving:** Use `deriving Shrinkable` on structures.

## CLI Module

### CLI.parseArgs

Parse command-line arguments into a filter.

```lean
CLI.parseArgs (args : List String) : IO TestFilter
```

### CLI.helpRequested

Check if help was requested.

```lean
CLI.helpRequested (args : List String) : Bool
```

### CLI.printHelp

Print help message.

```lean
CLI.printHelp : IO Unit
```

### TestFilter

```lean
structure TestFilter where
  testPatterns : List String := []
  suitePatterns : List String := []
  exactMatch : Bool := false
```
