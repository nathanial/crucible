import Crucible.Core
import Crucible.SuiteRegistry
import Crucible.Macros
import Crucible.Filter
import Crucible.CLI
import Crucible.Output

/-!
# Crucible Test Framework

A lightweight test framework for Lean 4 with automatic test discovery.

## Architecture Overview

```
Crucible.lean        -- This file: main entry point, re-exports all modules
├── Core.lean        -- TestCase, assertions (≡, shouldBe, etc.), test runners
├── Macros.lean      -- `test` and `#generate_tests` syntax
├── SuiteRegistry    -- `testSuite` command for auto-discovery
├── Filter.lean      -- TestFilter type for selective test runs
├── CLI.lean         -- Command-line argument parsing
└── Output.lean      -- ANSI colors and formatting
```

## Quick Start Template

Copy this template to create a new test file:

```lean
import Crucible

namespace MyProject.Tests

open Crucible

testSuite "My Feature Tests"

test "basic equality" := do
  1 + 1 ≡ 2

test "option unwrapping" := do
  (some 42) ≡? 42

test "string operations" := do
  "hello".length ≡ 5
  shouldContainSubstr "hello world" "world"

#generate_tests

end MyProject.Tests
```

Then in `Tests/Main.lean`:

```lean
import Crucible
import MyProject.Tests

open Crucible

def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
```

## Assertion Reference

### Core Assertions (most common)
- `actual ≡ expected` -- Equality check (type `\equiv` for ≡)
- `optionVal ≡? expected` -- Unwrap Option and check equality
- `shouldBe actual expected` -- Same as ≡, function form
- `shouldBeSome opt expected` -- Same as ≡?, function form
- `ensure condition "message"` -- Boolean assertion with message

### Numeric Assertions
- `shouldBeNear actual expected (eps := 0.0001)` -- Float comparison
- `shouldBeBetween value min max` -- Range check (inclusive)

### Collection Assertions
- `shouldHaveLength list n` -- Check list length
- `shouldContain list element` -- Check list contains element
- `shouldContainAll list elements` -- Check list contains all elements
- `shouldBeEmpty list` -- Check list is empty
- `shouldNotBeEmpty list` -- Check list is not empty

### String Assertions
- `shouldStartWith str prefix` -- Prefix check
- `shouldEndWith str suffix` -- Suffix check
- `shouldContainSubstr str substring` -- Substring check

### Exception Assertions
- `shouldThrow action` -- Expect any exception
- `shouldThrowWith action "substring"` -- Expect exception containing substring
- `shouldThrowMatching action predicate` -- Expect exception matching predicate
- `shouldNotThrow action` -- Expect no exception

### Result Type Assertions
- `shouldBeOk (result : Except ε α)` -- Unwrap Ok, throw on Error
- `shouldBeErr (result : Except ε α)` -- Expect Error
- `shouldBeNone (opt : Option α)` -- Expect none

### Predicate Assertions
- `shouldSatisfy condition "description"` -- Boolean with description
- `shouldMatch value predicate "description"` -- Value satisfies predicate

### Context Helpers
- `assertion |> withContext "context"` -- Add context to error message
- `withMessage "message" assertion` -- Replace error message entirely

## Test Modifiers

```lean
-- Skip a test (won't run, shows as skipped)
test "not ready" (skip := "waiting for API v2") := do
  ...

-- Skip without reason
test "disabled" (skip) := do
  ...

-- Expected failure (test should fail, passing is an error)
test "known bug" (xfail := "issue #42") := do
  ...

-- Timeout in milliseconds
test "slow operation" (timeout := 5000) := do
  ...

-- Retry on failure
test "flaky network" (retry := 3) := do
  ...
```

## Fixtures (Setup/Teardown)

```lean
namespace MyTests

testSuite "Database Tests"

-- Runs once before all tests in this suite
beforeAll := do
  IO.println "Setting up database connection"

-- Runs once after all tests (even if tests fail)
afterAll := do
  IO.println "Closing database connection"

-- Runs before each individual test
beforeEach := do
  IO.println "Starting transaction"

-- Runs after each individual test (even if test fails)
afterEach := do
  IO.println "Rolling back transaction"

test "insert record" := do
  ...

#generate_tests

end MyTests
```

## CLI Filtering

```bash
lake test -- --test parse           # Tests containing "parse"
lake test -- --suite "HTTP Parser"  # Suites containing "HTTP Parser"
lake test -- -t foo -t bar          # Tests matching "foo" OR "bar"
lake test -- --exact -t "my test"   # Exact match mode
lake test -- --help                 # Show filter options
```

## Common Patterns

### Testing IO Actions

```lean
test "file operations" := do
  let content ← IO.FS.readFile "test.txt"
  content.length > 0 |> shouldSatisfy "file not empty"
```

### Testing with Temporary State

```lean
test "counter operations" := do
  let counter ← IO.mkRef 0
  counter.modify (· + 1)
  counter.modify (· + 1)
  (← counter.get) ≡ 2
```

### Multiple Assertions

```lean
test "user validation" := do
  let user := { name := "Alice", age := 25 }
  user.name ≡ "Alice"
  user.age ≡ 25
  shouldSatisfy (user.age >= 18) "user is adult"
```

### Testing Exceptions

```lean
test "division by zero" := do
  shouldThrowWith (divide 1 0) "division by zero"
```

## Lakefile Configuration

```lean
@[test_driver]
lean_exe tests where
  root := `Tests.Main
  supportInterpreter := true

-- Run with: lake test
```
-/
