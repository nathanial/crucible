# Common Mistakes

This page covers common pitfalls when using Crucible and how to avoid them.

## Forgetting `#generate_tests`

Tests won't run if you forget the collection command:

```lean
-- ❌ Tests defined but never collected
namespace MyTests
open Crucible

testSuite "My Suite"

test "one" := do 1 ≡ 1
test "two" := do 2 ≡ 2

end MyTests  -- Missing #generate_tests!
```

```lean
-- ✓ Tests collected and will run
namespace MyTests
open Crucible

testSuite "My Suite"

test "one" := do 1 ≡ 1
test "two" := do 2 ≡ 2

#generate_tests  -- Required!

end MyTests
```

The `#generate_tests` command must come after all test definitions and before `end`.

## Forgetting `open Crucible`

Without opening Crucible, you'll get "unknown identifier" errors:

```lean
-- ❌ Unknown identifier 'testSuite'
namespace MyTests

testSuite "My Suite"  -- Error!

test "one" := do 1 ≡ 1  -- Error!
```

```lean
-- ✓ Crucible opened
namespace MyTests
open Crucible

testSuite "My Suite"

test "one" := do 1 ≡ 1
```

## Forgetting to Import Test Modules

Your test runner must import all test modules:

```lean
-- ❌ Tests in MyTests.lean won't run
import Crucible

def main : IO UInt32 := runAllSuites
```

```lean
-- ✓ Import all test modules
import Crucible
import Tests.MyTests
import Tests.OtherTests

def main : IO UInt32 := runAllSuites
```

Each file containing tests must be imported in your `Tests/Main.lean`.

## Confusing `≡` with `==`

`≡` is an assertion that throws on failure. `==` returns a boolean:

```lean
-- ❌ This compiles but doesn't assert anything!
test "wrong" := do
  let _ := (1 + 1) == 2  -- Just returns true, discarded
  pure ()

-- ✓ This actually asserts equality
test "correct" := do
  (1 + 1) ≡ 2  -- Throws if not equal
```

Remember: `≡` (type `\equiv`) for assertions, `==` for boolean expressions.

## Using `≡` Outside a Test

The `≡` operator only works inside a `test` block:

```lean
-- ❌ Can't use ≡ at top level
def checkValue : IO Unit := do
  1 ≡ 1  -- Error: not in test context
```

```lean
-- ✓ Use inside a test
test "value check" := do
  1 ≡ 1
```

For helper functions, use `ensure` or return a boolean:

```lean
def isValid (x : Nat) : IO Bool := pure (x > 0)

test "validation" := do
  let valid ← isValid 42
  ensure valid "should be valid"
```

## Wrong Namespace Structure

Each suite needs its own namespace with `testSuite`, tests, and `#generate_tests`:

```lean
-- ❌ Missing testSuite declaration
namespace Tests
open Crucible

test "one" := do 1 ≡ 1

#generate_tests
end Tests
```

```lean
-- ✓ Proper structure
namespace Tests
open Crucible

testSuite "My Tests"  -- Required!

test "one" := do 1 ≡ 1

#generate_tests
end Tests
```

## Multiple Suites in One Namespace

Each `testSuite` should be in its own namespace:

```lean
-- ❌ Two suites in one namespace causes confusion
namespace Tests
open Crucible

testSuite "Suite A"
test "a1" := do 1 ≡ 1

testSuite "Suite B"  -- This replaces Suite A!
test "b1" := do 2 ≡ 2

#generate_tests
end Tests
```

```lean
-- ✓ Separate namespaces
namespace Tests.SuiteA
open Crucible
testSuite "Suite A"
test "a1" := do 1 ≡ 1
#generate_tests
end Tests.SuiteA

namespace Tests.SuiteB
open Crucible
testSuite "Suite B"
test "b1" := do 2 ≡ 2
#generate_tests
end Tests.SuiteB
```

## Using `runAllSuites` Without `(args)`

For CLI filtering to work, pass command-line arguments:

```lean
-- ❌ CLI filtering won't work
def main : IO UInt32 := runAllSuites

-- ✓ Enables --test, --suite filtering
def main (args : List String) : IO UInt32 :=
  runAllSuitesFiltered args
```

## Property Test Without Opening Property Module

Property testing requires opening the Property module:

```lean
-- ❌ Unknown 'forAll''
test "commutative" := do
  forAll' fun (a, b) : (Int × Int) => a + b == b + a  -- Error!
```

```lean
-- ✓ Open Property module
open Crucible.Property

proptest "commutative" :=
  forAll' fun (a, b) : (Int × Int) => a + b == b + a
```

## Assertions in Pure Functions

Assertions like `≡` and `ensure` are `IO` actions. They can't be used in pure functions:

```lean
-- ❌ Can't use IO assertions in pure function
def checkPure (x : Nat) : Bool :=
  x ≡ 42  -- Error: expected Bool, got IO Unit
```

```lean
-- ✓ Use pure comparison
def checkPure (x : Nat) : Bool := x == 42

-- ✓ Or make it IO
def checkIO (x : Nat) : IO Unit := do
  x ≡ 42
```

## Tips for Avoiding Mistakes

1. **Use the template**: Start from a working example rather than from scratch

2. **Check imports**: Ensure `import Crucible` and `open Crucible` are present

3. **Run early, run often**: Run `lake test` after adding each test

4. **Watch for warnings**: The compiler often hints at issues

5. **Follow the pattern**: `namespace` → `open Crucible` → `testSuite` → tests → `#generate_tests` → `end`
