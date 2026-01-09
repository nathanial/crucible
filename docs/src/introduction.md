# Introduction

Crucible is a lightweight test framework for Lean 4. It provides simple, declarative test definitions with built-in assertions and a clean runner that produces readable output.

## Features

- **Declarative syntax**: Use `test "name" := do` to define tests
- **Test suites**: Group tests with `testSuite "name"`
- **Automatic collection**: `#generate_tests` collects all tests in a namespace
- **Automatic suite runner**: `runAllSuites` runs every registered suite
- **Rich assertions**: Equality, Option unwrapping, numeric comparisons, and more
- **Test timeouts**: Configure per-test or suite-wide timeouts
- **Test retries**: Automatic retry count for flaky tests
- **Skip and xfail**: Mark tests as skipped or expected to fail
- **Soft assertions**: Collect multiple failures per test
- **Property testing**: QuickCheck-style testing with shrinking
- **CLI filtering**: Run specific tests or suites from the command line
- **Clean output**: Formatted test results with pass/fail counts and timing

## Example

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

## Why Crucible?

Crucible was designed with a few key principles:

1. **Minimal boilerplate**: Define tests with just a name and a do block
2. **Discoverable**: Tests are automatically collected and run
3. **Readable output**: Clear pass/fail reporting with timing information
4. **Flexible**: Support for fixtures, retries, timeouts, and property testing
5. **Zero dependencies**: Uses only Lean's standard library

## Getting Started

Head to the [Installation](./installation.md) page to add Crucible to your project, then check out the [Quick Start](./quick-start.md) guide to write your first tests.
