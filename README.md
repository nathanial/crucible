# Crucible

Ever written Lean 4 tests like this?

```lean
def test_addition : IO Unit := do
  if 1 + 1 != 2 then
    throw (IO.userError "addition failed")
```

With Crucible, write this instead:

```lean
test "addition works" := do
  (1 + 1) ≡ 2
```

A lightweight test framework for Lean 4 with declarative syntax, 30+ built-in assertions, property testing, and zero dependencies.

## Installation

Add to your `lakefile.lean`:

```lean
require crucible from git "https://github.com/nathanial/crucible" @ "v0.1.0"
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
- **Automatic discovery**: Tests are discovered automatically—no registration needed
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

## Documentation

- [Quick Start](./docs/src/quick-start.md) — Write your first tests
- [Assertions Reference](./docs/src/assertions.md) — 30+ built-in assertions
- [Fixtures](./docs/src/fixtures.md) — Setup and teardown hooks
- [Property Testing](./docs/src/property-testing.md) — QuickCheck-style testing
- [CLI Reference](./docs/src/cli.md) — Filter and run tests
- [API Reference](./docs/src/api-reference.md) — Complete API docs

## License

MIT License - see [LICENSE](LICENSE) for details.
