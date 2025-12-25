# Crucible

A lightweight test framework for Lean 4.

Crucible provides simple, declarative test definitions with built-in assertions and a clean runner that produces readable output.

## Installation

Add to your `lakefile.lean`:

```lean
require crucible from git "https://github.com/nathanial/crucible" @ "master"
```

Then run:

```bash
lake update
lake build
```

## Quick Start

```lean
import Crucible

namespace MyTests
open Crucible

testSuite "Arithmetic"

test "addition works" := do
  (1 + 1) ≡ 2

test "multiplication works" := do
  (3 * 4) ≡ 12

#generate_tests

end MyTests
```

In your test runner (`Tests/Main.lean`):

```lean
import Crucible
import MyTests

open Crucible

def main : IO UInt32 := do
  runAllSuites
```

## Assertions

| Assertion | Description |
|-----------|-------------|
| `a ≡ b` or `shouldBe a b` | Assert equality |
| `opt ≡? val` or `shouldBeSome opt val` | Assert `Option` contains value |
| `shouldBeNone opt` | Assert `Option` is `none` |
| `shouldSatisfy cond msg` | Assert condition is true |
| `shouldMatch val pred desc` | Assert value satisfies predicate |
| `shouldBeNear a b eps` | Assert floats are approximately equal (default eps: 0.0001) |
| `shouldBeApprox a b eps` | Alias for `shouldBeNear` |
| `shouldHaveLength list n` | Assert list has expected length |
| `shouldContain list elem` | Assert list contains element |
| `ensure cond msg` | Throw if condition is false |
| `ensureEq a b msg` | Throw if values not equal (legacy) |

## Features

- **Declarative syntax**: Use `test "name" := do` to define tests
- **Test suites**: Group tests with `testSuite "name"`
- **Automatic collection**: `#generate_tests` collects all tests in the namespace
- **Automatic suite runner**: `runAllSuites` runs every registered suite
- **Test timeouts**: Configure per-test or suite-wide timeouts
- **Test retries**: Configure automatic retry count for flaky tests
- **Clean output**: Formatted test results with pass/fail counts
- **IO support**: Tests run in `IO`, supporting effectful operations

## Timeouts and Retries

Configure timeouts and retries at the test level:

```lean
test "network call" (timeout := 5000) := do
  -- test times out after 5 seconds
  someNetworkCall

test "flaky test" (retry := 3) := do
  -- automatically retries up to 3 times on failure
  flakyOperation

test "both" (timeout := 2000) (retry := 2) := do
  -- combines timeout and retry
  riskyOperation
```

Or configure defaults for the entire suite runner:

```lean
def main : IO UInt32 := do
  runAllSuites (timeout := 10000)  -- 10 second default timeout
  -- or
  runAllSuites (retry := 2)  -- retry all tests up to 2 times
  -- or
  runAllSuites (timeout := 5000) (retry := 1)  -- both
```

## Building & Testing

```bash
lake build
```

## License

MIT License - see [LICENSE](LICENSE) for details.
