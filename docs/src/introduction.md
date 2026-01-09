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

Testing should be easy enough that you actually do it. Many testing frameworks impose ceremony—configuration files, special annotations, complex setup rituals—that creates friction between having an idea and verifying it works. Crucible takes the opposite approach: if you can write a `do` block, you can write a test.

The design centers on removing barriers. You don't register tests manually or maintain lists of test functions. Instead, you write tests where they make sense, and Crucible finds them automatically. The `#generate_tests` command scans the current namespace and collects everything marked with `test`, so adding a new test is just writing it—nothing else to update, no boilerplate to maintain.

Output matters too. When tests fail, you need to understand why quickly. Crucible shows exactly what was expected versus what was received, with timing information so you can spot slow tests at a glance. When everything passes, the output stays clean and informative without drowning you in noise.

Crucible deliberately avoids external dependencies. It uses only Lean's standard library, which means no version conflicts, no transitive dependency headaches, and one less thing to worry about when upgrading your toolchain. This constraint also keeps the library focused—rather than pulling in half the ecosystem, Crucible does one thing well.

The feature set grew from real testing needs: fixtures for setup and teardown, timeouts for tests that might hang, retries for flaky network calls, skip and expected-failure markers for work in progress, and property-based testing for when examples aren't enough. Each feature exists because the alternative—working around its absence—was worse than adding it.

## Getting Started

Head to the [Installation](./installation.md) page to add Crucible to your project, then check out the [Quick Start](./quick-start.md) guide to write your first tests.
