# Quick Start

This guide walks you through writing your first test suite with Crucible.

## Your First Test

Create a test file `Tests/MyTests.lean`:

```lean
import Crucible

namespace MyTests
open Crucible

testSuite "My First Suite"

test "addition works" := do
  (1 + 1) ≡ 2

test "strings can be compared" := do
  "hello".length ≡ 5

#generate_tests

end MyTests
```

Let's break this down:

1. `import Crucible` - Import the test framework
2. `namespace MyTests` - Create a namespace for your tests
3. `open Crucible` - Bring Crucible definitions into scope
4. `testSuite "My First Suite"` - Declare a named test suite
5. `test "name" := do` - Define a test with a name
6. `≡` - The equality assertion operator (type `\equiv`)
7. `#generate_tests` - Collect all tests defined above

## The Test Runner

Create `Tests/Main.lean` to run your tests:

```lean
import Crucible
import Tests.MyTests

open Crucible

def main : IO UInt32 := do
  runAllSuites
```

## Running Tests

```bash
lake test
```

You'll see output like:

```
My First Suite
──────────────
[1/2]  addition works... ✓ (1ms)
[2/2]  strings can be compared... ✓ (0ms)

Results: 2 passed

────────────────────────────────────────
Summary: 2 passed, 0 failed (100.0%)
         1 suites, 2 tests run
         Completed in 0.01s
────────────────────────────────────────
```

## Multiple Test Suites

You can define multiple suites in different namespaces:

```lean
import Crucible

namespace ArithmeticTests
open Crucible

testSuite "Arithmetic"

test "addition" := do (1 + 1) ≡ 2
test "subtraction" := do (5 - 3) ≡ 2

#generate_tests

end ArithmeticTests

namespace StringTests
open Crucible

testSuite "Strings"

test "length" := do "hello".length ≡ 5
test "append" := do ("a" ++ "b") ≡ "ab"

#generate_tests

end StringTests
```

## Testing with IO

Tests run in `IO`, so you can test effectful code:

```lean
test "file operations" := do
  let content ← IO.FS.readFile "test-data.txt"
  content.length > 0 |> ensure "file should not be empty"
```

## Using Assertions

Crucible provides several assertion styles:

```lean
-- Equality (recommended)
(actual) ≡ expected

-- Option unwrapping
String.toNat? "42" ≡? 42

-- Boolean conditions
ensure (value > 0) "value should be positive"

-- Named assertions
shouldBe actual expected
shouldBeSome option expected
shouldBeNone option
```

See the [Assertions](./assertions.md) page for the complete list.

## Next Steps

- [Writing Tests](./writing-tests.md) - Learn more about test syntax
- [Test Suites](./test-suites.md) - Organize tests into suites
- [Assertions](./assertions.md) - All available assertions
- [Fixtures](./fixtures.md) - Setup and teardown hooks
