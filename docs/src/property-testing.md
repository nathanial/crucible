# Property Testing

Traditional example-based tests verify that specific inputs produce expected outputs: given input X, expect output Y. This works well when you can enumerate the interesting cases, but some behaviors apply across entire domains. Addition is commutative for all integers, not just the ones you happened to test. List reversal is self-inverse for lists of any length. When a property should hold universally, testing a handful of examples provides weak evidence at best.

Property testing takes a different approach. Instead of specifying individual test cases, you describe properties that should always be true, and the framework generates hundreds of random inputs to verify them. If any input violates the property, the test fails and shows you the counterexample—often a small, simple case that makes the bug obvious.

## Overview

Crucible's property testing follows the QuickCheck tradition. You write a property as a function from random inputs to a boolean, and Crucible generates inputs, checks the property, and when a failure occurs, shrinks the input to find the simplest counterexample.

```lean
proptest "addition is commutative" :=
  forAll' fun (a, b) : (Int × Int) =>
    a + b == b + a
```

This property says that for any pair of integers, swapping the order of addition doesn't change the result. Crucible will generate 100 random pairs, check each one, and report any that fail. Since addition really is commutative, this test passes—but if it didn't, you'd see exactly which pair violated the property.

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

When property testing finds a failing input, that input is often large and complicated—a list with 47 elements, an integer like 8294716. The actual bug might only require 2 elements or the number -1. Finding the minimal failing case by hand is tedious; shrinking automates it.

Shrinking works by repeatedly trying smaller versions of the failing input. If a smaller version still fails the property, it becomes the new candidate, and shrinking continues from there. Eventually, no smaller failing input can be found, and that's your counterexample.

The result is often revelatory. Instead of staring at a 47-element list wondering what went wrong, you see a 2-element list that makes the bug obvious.

### Built-in Shrinking

Each type shrinks toward a "zero" value:

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

The hardest part of property testing is figuring out what properties to test. Unlike example-based tests where you pick inputs and outputs, property tests require thinking about the general behavior of your code—what's true for all valid inputs?

### Good Properties

Some property patterns appear across many domains. Mathematical laws like commutativity (`a + b == b + a`) and associativity (`(a + b) + c == a + (b + c)`) directly translate to tests. If your function is supposed to be idempotent, test that applying it twice gives the same result as applying it once.

Invariants are properties that should always hold regardless of what you do. A sorted list should stay sorted after inserting an element. A balanced tree should stay balanced after any operation. These make excellent properties because they capture essential correctness conditions.

Round-trip properties test that encoding and decoding are inverses: `decode (encode x) == x`. This pattern works for serializers, parsers, compressors, and anything that transforms data reversibly.

Finally, if you have a slow-but-obviously-correct reference implementation and a fast-but-complex optimized version, test that they agree on all inputs. This oracle testing catches bugs in the optimized code while relying on the reference for correctness.

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

Property testing rewards a gradual approach. Start with the simplest property you can think of—something you're confident should pass. This validates that your generators work and your property syntax is correct. Then build toward more interesting properties, adding complexity only when the simpler ones pass.

Naming matters more in property tests than in example tests. A test named "test_1" tells you nothing when it fails; a test named "sorting preserves length" tells you exactly what invariant was violated. Since you'll be reading failure output to understand counterexamples, invest in names that explain what should have been true.

Generators need to cover edge cases. If your list generator never produces empty lists, you might miss bugs that only manifest on empty input. Check that your generators can produce the smallest values (0, empty list, empty string) as well as larger ones. Most built-in generators already do this, but custom generators need explicit thought.

Properties should be pure functions—no side effects, no printing, no IO beyond what's needed to compute the result. Side effects make shrinking unreliable because the same input might behave differently on different runs. If you need to test effectful code, wrap it so the property can compare results purely.

The default 100 tests catches most bugs, but critical code deserves more. Bump the count for properties that are central to correctness, especially ones with large input spaces. A property over pairs of 64-bit integers has more possible inputs than you can exhaustively test, so more random samples provide more confidence.

When a test fails, save the seed. Including `(seed := 12345)` in your property makes the failure reproducible, which is essential for debugging. Once you fix the bug, you can either keep the seed (as a regression test) or remove it (to resume random testing).
