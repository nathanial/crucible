# CLAUDE.md - Crucible

Lightweight test framework for Lean 4 with declarative syntax, automatic discovery, and property testing.

## Build & Test

```bash
lake build      # Build the library
lake test       # Run tests
```

## Architecture

```
Crucible.lean        -- Main entry point, re-exports all modules
├── Core.lean        -- TestCase, assertions, test runners
├── Macros.lean      -- `test` syntax macro
├── SuiteRegistry    -- `testSuite` command and auto-discovery
├── Filter.lean      -- TestFilter for selective runs
├── CLI.lean         -- Command-line argument parsing
├── Output.lean      -- ANSI colors and formatting
└── Property/        -- Property-based testing
    ├── Random.lean      -- RandState, Gen monad
    ├── Shrink.lean      -- Shrinkable typeclass
    ├── Generators.lean  -- Arbitrary typeclass
    ├── Core.lean        -- Property, forAll, check
    └── Syntax.lean      -- `proptest` command
```

## Quick Reference

### Writing Tests

```lean
import Crucible

namespace MyTests
open Crucible

testSuite "My Feature"

test "basic equality" := do
  1 + 1 ≡ 2              -- Type \equiv for ≡
  (some 42) ≡? 42        -- Option unwrap + equality

test "with timeout" (timeout := 5000) := do
  slowOperation

test "with retry" (retry := 3) := do
  flakyOperation

test "skipped" (skip := "reason") := do
  notReady

test "expected failure" (xfail := "issue #42") := do
  knownBug

end MyTests
```

### Test Runner (Tests/Main.lean)

```lean
import Crucible
import MyTests

open Crucible

def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
```

### Common Assertions

| Assertion | Description |
|-----------|-------------|
| `a ≡ b` | Equality |
| `opt ≡? val` | Option contains value |
| `shouldBeNone opt` | Option is none |
| `shouldSatisfy cond msg` | Boolean check |
| `shouldBeNear a b eps` | Float approximation |
| `shouldContain list elem` | List membership |
| `shouldThrow action` | Expects exception |
| `shouldThrowWith action "msg"` | Exception with substring |

### Fixtures

```lean
beforeAll := do IO.println "setup"
afterAll := do IO.println "teardown"
beforeEach := do IO.println "before test"
afterEach := do IO.println "after test"
```

### CLI Filtering

```bash
lake test -- --test parse           # Tests containing "parse"
lake test -- --suite "HTTP"         # Suites containing "HTTP"
lake test -- -t foo -t bar          # Multiple patterns (OR)
lake test -- --exact -t "my test"   # Exact match
```

## Key Types

- `TestCase` - Named test with IO action, timeout, retry, skip, xfail options
- `TestResults` - Aggregated pass/fail/skip counts with suite breakdown
- `SuiteResult` - Results for a single suite
- `Fixture` - beforeAll/afterAll/beforeEach/afterEach hooks
- `TestFilter` - CLI filter patterns

## Development Notes

- Tests are auto-discovered via `testCaseExtension` environment extension
- `testSuite` registers namespace in `suiteExtension`
- `runAllSuites` macro queries extensions at compile time
- Timeouts use dedicated threads to avoid thread pool exhaustion
- Property tests use `proptest` command with `Arbitrary` typeclass
