import Crucible.Core
import Crucible.SuiteRegistry
import Crucible.Macros
import Crucible.Filter
import Crucible.CLI

/-!
# Crucible

A lightweight test framework for Lean 4.

## Basic Usage

```lean
import Crucible

namespace MyTests

testSuite "My Test Suite"

test "addition works" := do
  (1 + 1) ≡ 2

test "option handling" := do
  (some 42) ≡? 42

#generate_tests

end MyTests
```

Then in Main.lean:
```lean
import Crucible
import MyTests

open Crucible

def main : IO UInt32 := runAllSuites
```

## Test Filtering

Use `runAllSuitesFiltered` instead of `runAllSuites` to enable command-line filtering:

```lean
def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
```

Then run with filter options:
```bash
lake test -- --test parse           # Tests containing "parse"
lake test -- --suite "HTTP Parser"  # Suites containing "HTTP Parser"
lake test -- -t foo -t bar          # Tests matching "foo" OR "bar"
lake test -- --exact -t "my test"   # Exact match mode
lake test -- --help                 # Show filter options
```
-/
