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
| `ensure cond msg` | Throw if condition is false |
| `ensureEq a b msg` | Throw if values not equal |

## Features

- **Declarative syntax**: Use `test "name" := do` to define tests
- **Test suites**: Group tests with `testSuite "name"`
- **Automatic collection**: `#generate_tests` collects all tests in the namespace
- **Automatic suite runner**: `runAllSuites` runs every registered suite
- **Clean output**: Formatted test results with pass/fail counts
- **IO support**: Tests run in `IO`, supporting effectful operations

## Building & Testing

```bash
lake build
```

## License

MIT License - see [LICENSE](LICENSE) for details.
