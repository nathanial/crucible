# Introduction

## The Problem

Testing in Lean 4 without a framework means writing boilerplate:

```lean
def test_addition : IO Unit := do
  let result := 1 + 1
  if result != 2 then
    throw (IO.userError s!"Expected 2, got {result}")

def test_list_length : IO Unit := do
  let xs := [1, 2, 3]
  if xs.length != 3 then
    throw (IO.userError s!"Expected length 3, got {xs.length}")

def main : IO Unit := do
  test_addition
  test_list_length
  IO.println "All tests passed"
```

No test discovery. No clear output. No rich assertions. Just manual error checking.

## The Solution

Crucible provides declarative test definitions with built-in assertions:

```lean
import Crucible

namespace MyTests
open Crucible

testSuite "Basics"

test "addition works" := do
  (1 + 1) ≡ 2

test "list length" := do
  shouldHaveLength [1, 2, 3] 3

#generate_tests
end MyTests
```

Run with `lake test` and get clean, informative output:

```
Basics
──────
[1/2]  addition works... ✓ (1ms)
[2/2]  list length... ✓ (0ms)

Summary: 2 passed, 0 failed (100.0%)
```

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
