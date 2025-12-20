import Crucible.Core
import Crucible.SuiteRegistry
import Crucible.Macros

/-!
# Crucible

A lightweight test framework for Lean 4.

## Usage

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

def main : IO UInt32 := do
  runTests "My Test Suite" MyTests.cases
```
-/
