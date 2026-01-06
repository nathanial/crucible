/-!
# Test Filtering

Provides types and functions for filtering tests by name patterns.
-/

namespace Crucible

/-- Filter specification for selecting tests/suites to run. -/
structure TestFilter where
  /-- Patterns to match against test names (substring match by default). -/
  testPatterns : List String := []
  /-- Patterns to match against suite names (substring match by default). -/
  suitePatterns : List String := []
  /-- If true, patterns are interpreted as exact matches instead of substrings. -/
  exactMatch : Bool := false
  deriving Repr, Inhabited

namespace TestFilter

/-- Empty filter that matches everything. -/
def empty : TestFilter := {}

/-- Check if filter matches all (no patterns specified). -/
def matchesAll (f : TestFilter) : Bool :=
  f.testPatterns.isEmpty && f.suitePatterns.isEmpty

/-- Check if a suite name matches the filter. -/
def matchesSuite (f : TestFilter) (suiteName : String) : Bool :=
  if f.suitePatterns.isEmpty then true
  else if f.exactMatch then f.suitePatterns.contains suiteName
  else f.suitePatterns.any (suiteName.toSlice.contains ·.toSlice)

/-- Check if a test name matches the filter. -/
def matchesTest (f : TestFilter) (testName : String) : Bool :=
  if f.testPatterns.isEmpty then true
  else if f.exactMatch then f.testPatterns.contains testName
  else f.testPatterns.any (testName.toSlice.contains ·.toSlice)

end TestFilter

end Crucible
