import Crucible.Property.Random
import Crucible.Property.Shrink
import Crucible.Property.Generators
import Crucible.Property.Core
import Crucible.Property.Syntax

/-!
# Property-Based Testing for Crucible

A lightweight property-based testing module that integrates with Crucible's
test runner. Provides random value generation, shrinking of counterexamples,
and the `proptest` command for defining property tests.

## Usage

```lean
import Crucible

open Crucible
open Crucible.Property

testSuite "Math Properties"

-- Simple property using Arbitrary instance
proptest "addition is commutative" :=
  forAll' fun (x : Int) =>
  forAll' fun (y : Int) =>
    x + y == y + x

-- With custom generator
proptest "list reverse twice is identity" :=
  forAll (Gen.listOf arbitrary) fun xs =>
    xs.reverse.reverse == xs

-- With explicit seed for reproduction
proptest "fixed seed test" (seed := 42) :=
  forAll' fun (n : Nat) => n + 0 == n

-- With custom test count
proptest "thorough test" (tests := 1000) :=
  forAll' fun (x : Int) => x * 1 == x
```

## Custom Types

Use `deriving Arbitrary, Shrinkable` for automatic generator/shrinker synthesis:

```lean
structure Point where
  x : Int
  y : Int
  deriving Repr, BEq, Arbitrary, Shrinkable

proptest "point operations" :=
  forAll' fun (p : Point) =>
    p.x - p.x == 0
```

## Components

- `Gen α` - Generator monad for producing random values
- `Arbitrary α` - Typeclass for types with generators
- `Shrinkable α` - Typeclass for types that can shrink
- `Property` - A property that can be tested
- `forAll`, `forAll'` - Property combinators
- `proptest` - Command for defining property tests
-/
