# Property Testing

QuickCheck-style property-based testing with automatic shrinking.

## Overview

Property testing generates random inputs and verifies that properties hold for all of them. When a test fails, Crucible automatically shrinks the input to find a minimal counterexample.

```lean
proptest "addition is commutative" :=
  forAll' fun (a, b) : (Int × Int) =>
    a + b == b + a
```

## Basic Usage

### The `proptest` Command

Define property tests with `proptest`:

```lean
proptest "list reverse twice is identity" :=
  forAll' fun (xs : List Int) =>
    xs.reverse.reverse == xs
```

### The `forAll` Combinator

Test properties over generated values:

```lean
proptest "absolute value is non-negative" :=
  forAll (Gen.chooseInt (-1000) 1000) fun n =>
    n.natAbs >= 0
```

### The `forAll'` Combinator

Use `Arbitrary` instances for automatic generation:

```lean
proptest "string length is non-negative" :=
  forAll' fun (s : String) =>
    s.length >= 0
```

## Generators

### Built-in Generators

| Generator | Description |
|-----------|-------------|
| `Gen.choose lo hi` | Random Nat in range [lo, hi] |
| `Gen.chooseInt lo hi` | Random Int in range [lo, hi] |
| `Gen.bool` | Random Bool |
| `Gen.float01` | Random Float in [0, 1) |
| `Gen.elements xs` | Random element from list |
| `Gen.oneOf gs` | Random generator from list |
| `Gen.listOf g` | Random-length list |
| `Gen.listOfN n g` | Fixed-length list |
| `Gen.optionOf g` | Random Option |
| `Gen.pair ga gb` | Pair of values |

### Using Generators

```lean
proptest "bounded addition" :=
  forAll (Gen.pair (Gen.choose 0 100) (Gen.choose 0 100)) fun (a, b) =>
    a + b <= 200
```

### Size-Dependent Generation

Generators can depend on the "size" parameter that grows during testing:

```lean
proptest "list properties" :=
  forAll (Gen.sized fun size => Gen.listOfN size (Gen.choose 0 100)) fun xs =>
    xs.length <= 100
```

## The Arbitrary Typeclass

Types with `Arbitrary` instances can be generated automatically.

### Built-in Instances

- `Nat`, `Int`, `Bool`, `Char`
- `UInt8`, `UInt16`, `UInt32`, `UInt64`
- `Float`, `String`
- `Option α` (if `α` has `Arbitrary`)
- `List α`, `Array α` (if `α` has `Arbitrary`)
- `(α × β)` (if both have `Arbitrary`)

### Deriving Arbitrary

For structures, derive `Arbitrary` automatically:

```lean
structure Point where
  x : Int
  y : Int
  deriving Arbitrary

proptest "point origin distance" :=
  forAll' fun (p : Point) =>
    (p.x * p.x + p.y * p.y) >= 0
```

## Shrinking

When a test fails, Crucible tries to find a simpler counterexample.

### Built-in Shrinking

- `Nat` shrinks toward 0
- `Int` shrinks toward 0
- `List` shrinks by removing elements or shrinking elements
- `Option` shrinks `some x` to `none` or `some (shrink x)`

### Deriving Shrinkable

```lean
structure Point where
  x : Int
  y : Int
  deriving Arbitrary, Shrinkable
```

### Shrinking Output

```
FAILED on test 42 (after 5 shrinks)
  Counterexample: Point { x := 0, y := -1 }
  Original: Point { x := 483, y := -72 }
  Seed: 12345
```

## Configuration

### Test Count

Run more or fewer tests:

```lean
proptest "thorough check" (tests := 1000) :=
  forAll' fun (n : Nat) => ...
```

Default: 100 tests

### Fixed Seed

Use a specific seed for reproducibility:

```lean
proptest "reproducible" (seed := 42) :=
  forAll' fun (n : Nat) => ...
```

### PropConfig

Full configuration options:

```lean
structure PropConfig where
  numTests : Nat := 100       -- Number of test cases
  maxSize : Nat := 100        -- Maximum size parameter
  maxShrinks : Nat := 1000    -- Maximum shrink iterations
  seed : Option Nat := none   -- Random seed
  verbose : Bool := false     -- Print each test case
```

## Writing Properties

### Good Properties

1. **Mathematical laws**: commutativity, associativity, identity
2. **Invariants**: properties that should always hold
3. **Round-trip**: `f (g x) == x`
4. **Reference implementation**: compare to known-correct code

### Examples

```lean
-- Commutativity
proptest "addition commutes" :=
  forAll' fun (a, b) : (Int × Int) =>
    a + b == b + a

-- Associativity
proptest "append is associative" :=
  forAll' fun (a, b, c) : (List Int × List Int × List Int) =>
    (a ++ b) ++ c == a ++ (b ++ c)

-- Identity
proptest "empty list is identity for append" :=
  forAll' fun (xs : List Int) =>
    xs ++ [] == xs && [] ++ xs == xs

-- Round-trip
proptest "reverse is self-inverse" :=
  forAll' fun (xs : List Int) =>
    xs.reverse.reverse == xs

-- Invariant
proptest "sorted list stays sorted after insert" :=
  forAll (Gen.listOf (Gen.choose 0 100)) fun xs =>
    let sorted := xs.mergeSort
    forAll (Gen.choose 0 100) fun n =>
      isSorted (insert n sorted)
```

## Custom Generators

### Combining Generators

```lean
def genEvenNat : Gen Nat := do
  let n ← Gen.choose 0 100
  pure (n * 2)

proptest "even numbers" :=
  forAll genEvenNat fun n =>
    n % 2 == 0
```

### Conditional Generation

```lean
def genNonEmpty : Gen (List Nat) :=
  Gen.listOf1 (Gen.choose 0 100)

proptest "non-empty list has head" :=
  forAll genNonEmpty fun xs =>
    xs.head?.isSome
```

### Frequency-Based Generation

```lean
def genMaybeZero : Gen Int :=
  Gen.frequency [
    (1, pure 0),           -- 10% chance of zero
    (9, Gen.chooseInt (-100) 100)  -- 90% other
  ]
```

## Integration with Test Suites

Property tests integrate with `testSuite` and `#generate_tests`:

```lean
namespace MathTests
open Crucible
open Crucible.Property

testSuite "Math Properties"

proptest "addition is commutative" :=
  forAll' fun (a, b) : (Int × Int) =>
    a + b == b + a

proptest "multiplication is associative" :=
  forAll' fun (a, b, c) : (Int × Int × Int) =>
    (a * b) * c == a * (b * c)

#generate_tests

end MathTests
```

## Best Practices

1. **Start with simple properties**: Build complexity gradually
2. **Use descriptive names**: Property names should explain what's being tested
3. **Test edge cases**: Ensure generators can produce edge values
4. **Keep properties pure**: Avoid side effects in property predicates
5. **Run enough tests**: Increase test count for critical properties
6. **Save failing seeds**: Use fixed seeds to reproduce failures
