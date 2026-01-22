import Lean
import Crucible.SuiteRegistry
import Crucible.Filter
import Crucible.CLI
import Crucible.Output

/-!
# Core Test Framework Types and Assertions

This module provides:
1. **TestCase** - The fundamental test structure
2. **Assertions** - Functions to check values (`shouldBe`, `≡`, etc.)
3. **Soft Assertions** - Record failures without stopping execution
4. **Test Runners** - `runTests`, `runAllSuites`, `runAllSuitesFiltered`
5. **Fixtures** - Setup/teardown hooks for test suites

## Key Types

- `TestCase` - A named test with an `IO Unit` action and optional timeout/retry/skip/xfail
- `TestResults` - Counts of passed/failed/skipped/xfailed/xpassed tests
- `Fixture` - Hooks: `beforeAll`, `afterAll`, `beforeEach`, `afterEach`
- `TestOutcome` - Result of a single test (passed/failed/skipped/xfailed/xpassed)
- `SoftAssertContext` - Context for collecting soft assertion failures

## Assertion Operators

The `≡` operator (type `\equiv`) is the primary assertion:
```lean
test "example" := do
  actual ≡ expected      -- Equality check
  optionVal ≡? expected  -- Option unwrap + equality
```

## Soft Assertions

Use `withSoftAsserts` to collect multiple failures without stopping:
```lean
test "multiple checks" := withSoftAsserts fun soft => do
  soft.shouldBe user.age 25
  soft.shouldBe user.name "Alice"
  soft.ensure (user.active) "user should be active"
  -- All failures reported at end
```

## Test Runner Functions

- `runAllSuites` - Run all registered test suites
- `runAllSuitesFiltered args` - Run with CLI filtering support
- `runTests "Suite Name" cases` - Run a specific list of test cases

These are typically used as compile-time macros that discover test suites automatically.
-/

namespace Crucible

/-- Check if a string contains a substring. -/
private def containsSubstr (s : String) (sub : String) : Bool :=
  (s.splitOn sub).length > 1

/-- Helper to await a task and return its result. -/
def awaitTask (task : IO (Task α)) : IO α := do
  let t ← task
  return t.get

/-- Why a test is being skipped. -/
inductive SkipReason where
  | unconditional : String → SkipReason
  | conditional : String → SkipReason
  deriving Repr, Inhabited

/-- A test case with a name and a monadic test action. -/
structure TestCase where
  name : String
  run : IO Unit
  timeoutMs : Option Nat := none
  retryCount : Option Nat := none
  /-- If set, the test is skipped with the given reason. -/
  skip : Option SkipReason := none
  /-- If true, the test is expected to fail (xfail). A passing xfail test is a failure. -/
  xfail : Bool := false
  /-- Reason why the test is expected to fail. -/
  xfailReason : Option String := none

/-- Return a copy of the test case with a timeout configured (in milliseconds). -/
def TestCase.withTimeout (tc : TestCase) (timeoutMs : Nat) : TestCase :=
  { tc with timeoutMs := some timeoutMs }

/-- Return a copy of the test case with a retry count configured. -/
def TestCase.withRetry (tc : TestCase) (retryCount : Nat) : TestCase :=
  { tc with retryCount := some retryCount }

/-- Return a copy of the test case marked as skipped. -/
def TestCase.withSkip (tc : TestCase) (reason : String := "skipped") : TestCase :=
  { tc with skip := some (.unconditional reason) }

/-- Return a copy of the test case marked as expected to fail. -/
def TestCase.withXfail (tc : TestCase) (reason : String := "expected failure") : TestCase :=
  { tc with xfail := true, xfailReason := some reason }

/-- Results from running a single test suite. -/
structure SuiteResult where
  /-- Human-readable name of the suite. -/
  name : String
  /-- Number of tests that passed. -/
  passed : Nat := 0
  /-- Number of tests that failed. -/
  failed : Nat := 0
  /-- Number of tests that were skipped. -/
  skipped : Nat := 0
  /-- Number of expected failures that failed (good). -/
  xfailed : Nat := 0
  /-- Number of expected failures that passed (bad). -/
  xpassed : Nat := 0
  /-- Time taken to run this suite in milliseconds. -/
  elapsedMs : Nat := 0
  deriving Repr, Inhabited

namespace SuiteResult

/-- Total number of tests run (excludes skipped). -/
def total (r : SuiteResult) : Nat := r.passed + r.failed + r.xfailed + r.xpassed

/-- Total number of tests including skipped. -/
def totalWithSkipped (r : SuiteResult) : Nat := r.total + r.skipped

/-- Check if all tests in this suite passed. -/
def allPassed (r : SuiteResult) : Bool := r.failed == 0 && r.xpassed == 0

end SuiteResult

/--
Aggregated results from running all test suites.

Provides per-suite breakdown via `suites` array and computed aggregate properties.

**Example:**
```lean
let results ← runAllSuites
IO.println s!"Total passed: {results.passed}"
IO.println s!"Suites run: {results.suiteCount}"
for suite in results.suites do
  IO.println s!"  {suite.name}: {suite.passed}/{suite.total} passed"
return results.toExitCode
```
-/
structure TestResults where
  /-- Per-suite results in execution order. -/
  suites : Array SuiteResult := #[]
  /-- Total time for all suites in milliseconds. -/
  totalElapsedMs : Nat := 0
  deriving Repr, Inhabited

namespace TestResults

/-- Number of suites that were run. -/
def suiteCount (r : TestResults) : Nat := r.suites.size

/-- Total passed tests across all suites. -/
def passed (r : TestResults) : Nat := r.suites.foldl (fun acc s => acc + s.passed) 0

/-- Total failed tests across all suites. -/
def failed (r : TestResults) : Nat := r.suites.foldl (fun acc s => acc + s.failed) 0

/-- Total skipped tests across all suites. -/
def skipped (r : TestResults) : Nat := r.suites.foldl (fun acc s => acc + s.skipped) 0

/-- Total expected failures (xfailed) across all suites. -/
def xfailed (r : TestResults) : Nat := r.suites.foldl (fun acc s => acc + s.xfailed) 0

/-- Total unexpected passes (xpassed) across all suites. -/
def xpassed (r : TestResults) : Nat := r.suites.foldl (fun acc s => acc + s.xpassed) 0

/-- Total number of tests run (excludes skipped). -/
def total (r : TestResults) : Nat := r.passed + r.failed + r.xfailed + r.xpassed

/-- Total number of tests including skipped. -/
def totalWithSkipped (r : TestResults) : Nat := r.total + r.skipped

/-- Check if all tests passed (xfailed counts as pass, xpassed counts as fail). -/
def allPassed (r : TestResults) : Bool := r.failed == 0 && r.xpassed == 0

/-- Convert to process exit code: 0 if all passed, 1 otherwise. -/
def toExitCode (r : TestResults) : UInt32 := if r.allPassed then 0 else 1

/-- Add a suite result to the aggregate. -/
def addSuite (r : TestResults) (s : SuiteResult) : TestResults :=
  { r with suites := r.suites.push s }

end TestResults

/--
Test fixture with setup/teardown hooks.

- `beforeAll`: Runs once before all tests in the suite
- `afterAll`: Runs once after all tests in the suite (even if tests fail)
- `beforeEach`: Runs before each individual test
- `afterEach`: Runs after each individual test (even if test fails)
-/
structure Fixture where
  beforeAll : Option (IO Unit) := none
  afterAll : Option (IO Unit) := none
  beforeEach : Option (IO Unit) := none
  afterEach : Option (IO Unit) := none

/-- Empty fixture with no hooks. -/
def Fixture.empty : Fixture := {}

/-- Assert that a condition is true. -/
def ensure (cond : Bool) (msg : String) : IO Unit := do
  if !cond then
    throw <| IO.userError s!"Expected: {msg}"

/--
Assert that two values are equal.

**Deprecated:** This function uses a non-standard parameter order (`msg, expected, actual`).
Use `shouldBe actual expected` or the `≡` infix operator instead.

**Migration:**
```lean
-- Old:
ensureEq "values match" expected actual
-- New:
actual ≡ expected
-- Or with context:
(actual ≡ expected) |> withContext "values match"
```
-/
@[deprecated "Use `shouldBe` or `≡` instead" (since := "2025-01-01")]
def ensureEq [BEq α] [Repr α] (msg : String) (expected : α) (actual : α) : IO Unit := do
  if expected != actual then
    throw <| IO.userError s!"Expected: {msg}\n  expected: {repr expected}\n  actual:   {repr actual}"

-- ============================================================================
-- Modern assertion helpers with cleaner syntax
-- ============================================================================

/-- Assert that actual equals expected. -/
def shouldBe [BEq α] [Repr α] (actual : α) (expected : α) : IO Unit := do
  if actual != expected then
    throw <| IO.userError s!"Expected {repr expected}, got {repr actual}"

/-- Check if two floats are approximately equal within epsilon. -/
def floatNear (a b : Float) (eps : Float := 0.0001) : Bool :=
  Float.abs (a - b) < eps

/-- Assert that two floats are approximately equal within epsilon. -/
def shouldBeNear (actual expected : Float) (eps : Float := 0.0001) : IO Unit := do
  if !floatNear actual expected eps then
    throw <| IO.userError s!"Expected {expected} (±{eps}), got {actual}"

/-- Alias for `shouldBeNear` for approximate comparisons. -/
def shouldBeApprox (actual expected : Float) (eps : Float := 0.0001) : IO Unit :=
  shouldBeNear actual expected eps

/-- Assert that an Option contains the expected value. -/
def shouldBeSome [BEq α] [Repr α] (actual : Option α) (expected : α) : IO Unit := do
  match actual with
  | some v =>
    if v != expected then
      throw <| IO.userError s!"Expected some {repr expected}, got some {repr v}"
  | none =>
    throw <| IO.userError s!"Expected some {repr expected}, got none"

/-- Assert that an Option is none. -/
def shouldBeNone [Repr α] (actual : Option α) : IO Unit := do
  match actual with
  | some v => throw <| IO.userError s!"Expected none, got some {repr v}"
  | none => pure ()

/-- Assert that a condition is true with a message. -/
def shouldSatisfy (cond : Bool) (msg : String := "condition") : IO Unit := do
  if !cond then
    throw <| IO.userError s!"Expected {msg} to be true"

/-- Assert that a value satisfies a predicate. -/
def shouldMatch [Repr α] (actual : α) (pred : α → Bool) (desc : String := "predicate") : IO Unit := do
  if !pred actual then
    throw <| IO.userError s!"Expected {repr actual} to satisfy {desc}"

/-- Assert that a list has the expected length. -/
def shouldHaveLength [Repr α] (actual : List α) (expected : Nat) : IO Unit := do
  if actual.length != expected then
    throw <| IO.userError s!"Expected list of length {expected}, got length {actual.length}"

/-- Assert that a list contains the expected element. -/
def shouldContain [BEq α] [Repr α] (actual : List α) (expected : α) : IO Unit := do
  if !actual.contains expected then
    throw <| IO.userError s!"Expected list to contain {repr expected}"

-- ============================================================================
-- Exception assertions
-- ============================================================================

/--
Assert that an action throws an exception.

**Example:**
```lean
test "division by zero throws" := do
  shouldThrow (divide 1 0)
```
-/
def shouldThrow (action : IO α) : IO Unit := do
  try
    let _ ← action
    throw <| IO.userError "Expected action to throw, but it completed successfully"
  catch _ =>
    pure ()

/--
Assert that an action throws an exception with a message containing the given substring.

**Example:**
```lean
test "invalid input error message" := do
  shouldThrowWith (parseNumber "abc") "invalid"
```
-/
def shouldThrowWith (action : IO α) (expectedSubstring : String) : IO Unit := do
  try
    let _ ← action
    throw <| IO.userError s!"Expected action to throw containing \"{expectedSubstring}\", but it completed successfully"
  catch e =>
    let msg := toString e
    if !containsSubstr msg expectedSubstring then
      throw <| IO.userError s!"Expected error containing \"{expectedSubstring}\", got: {msg}"

/--
Assert that an action throws an exception matching a predicate.

**Example:**
```lean
test "specific error type" := do
  shouldThrowMatching (riskyOp) fun msg => msg.startsWith "NetworkError"
```
-/
def shouldThrowMatching (action : IO α) (pred : String → Bool) (desc : String := "predicate") : IO Unit := do
  try
    let _ ← action
    throw <| IO.userError s!"Expected action to throw matching {desc}, but it completed successfully"
  catch e =>
    let msg := toString e
    if !pred msg then
      throw <| IO.userError s!"Expected error matching {desc}, got: {msg}"

/--
Assert that an action does NOT throw an exception.

**Example:**
```lean
test "valid input succeeds" := do
  shouldNotThrow (parseNumber "42")
```
-/
def shouldNotThrow (action : IO α) : IO Unit := do
  try
    let _ ← action
    pure ()
  catch e =>
    throw <| IO.userError s!"Expected action not to throw, but got: {e}"

-- ============================================================================
-- Additional comparison assertions
-- ============================================================================

/-- Assert that a list contains all expected elements (order independent). -/
def shouldContainAll [BEq α] [Repr α] (actual : List α) (expected : List α) : IO Unit := do
  for item in expected do
    if !actual.contains item then
      throw <| IO.userError s!"Expected list to contain {repr item}, got {repr actual}"

/-- Assert that a string starts with the expected prefix. -/
def shouldStartWith (actual : String) (expectedPrefix : String) : IO Unit := do
  if !actual.startsWith expectedPrefix then
    throw <| IO.userError s!"Expected \"{actual}\" to start with \"{expectedPrefix}\""

/-- Assert that a string ends with the expected suffix. -/
def shouldEndWith (actual : String) (suffix : String) : IO Unit := do
  if !actual.endsWith suffix then
    throw <| IO.userError s!"Expected \"{actual}\" to end with \"{suffix}\""

/-- Assert that a string contains the expected substring. -/
def shouldContainSubstr (actual : String) (substring : String) : IO Unit := do
  if !containsSubstr actual substring then
    throw <| IO.userError s!"Expected \"{actual}\" to contain \"{substring}\""

/-- Assert that a value is between min and max (inclusive). -/
def shouldBeBetween [Ord α] [Repr α] (actual : α) (min max : α) : IO Unit := do
  match compare actual min, compare actual max with
  | .lt, _ => throw <| IO.userError s!"Expected {repr actual} to be >= {repr min}"
  | _, .gt => throw <| IO.userError s!"Expected {repr actual} to be <= {repr max}"
  | _, _ => pure ()

/-- Assert that a list is empty. -/
def shouldBeEmpty [Repr α] (actual : List α) : IO Unit := do
  if !actual.isEmpty then
    throw <| IO.userError s!"Expected empty list, got {repr actual}"

/-- Assert that a list is not empty. -/
def shouldNotBeEmpty [Repr α] (actual : List α) : IO Unit := do
  if actual.isEmpty then
    throw <| IO.userError "Expected non-empty list, got []"

/--
Assert that an Except value is Ok and return the success value.
Throws with the error message if it's an error.

**Example:**
```lean
test "API call succeeds" := do
  let result ← apiCall
  let value ← shouldBeOk result "API call"
  value ≡ expectedValue
```
-/
def shouldBeOk [ToString ε] (result : Except ε α) (context : String := "Operation") : IO α := do
  match result with
  | .ok a => return a
  | .error e => throw <| IO.userError s!"Expected {context} to succeed, got error: {e}"

/--
Assert that an Except value is an error.

**Example:**
```lean
test "invalid input returns error" := do
  let result := validate ""
  shouldBeErr result
```
-/
def shouldBeErr [Repr α] (result : Except ε α) : IO Unit := do
  match result with
  | .ok a => throw <| IO.userError s!"Expected error, got ok: {repr a}"
  | .error _ => pure ()

-- ============================================================================
-- Assertion context / custom messages
-- ============================================================================

/--
Add context to an assertion for better error messages.

**Example:**
```lean
test "user validation" := do
  (user.age ≡ 25) |> withContext "checking user age"
  (user.name ≡ "Alice") |> withContext "checking user name"
```
-/
def withContext (assertion : IO Unit) (context : String) : IO Unit := do
  try
    assertion
  catch e =>
    throw <| IO.userError s!"[{context}] {e}"

/--
Run an assertion with a custom failure message.

**Example:**
```lean
test "complex validation" := do
  withMessage "User should be an adult" do
    ensure (user.age >= 18) "age check"
```
-/
def withMessage (msg : String) (assertion : IO Unit) : IO Unit := do
  try
    assertion
  catch _ =>
    throw <| IO.userError msg

-- ============================================================================
-- Soft Assertions
-- ============================================================================

/--
Context for tracking soft assertion failures.

Soft assertions record failures but don't stop test execution,
allowing multiple checks per test and seeing all failures at once.
-/
structure SoftAssertContext where
  /-- Accumulated failure messages. -/
  failures : IO.Ref (Array String)

namespace SoftAssertContext

/-- Create a new soft assertion context. -/
def new : IO SoftAssertContext := do
  let failures ← IO.mkRef #[]
  return { failures }

/-- Record a failure message. -/
def addFailure (ctx : SoftAssertContext) (msg : String) : IO Unit :=
  ctx.failures.modify (·.push msg)

/-- Get all recorded failures. -/
def getFailures (ctx : SoftAssertContext) : IO (Array String) :=
  ctx.failures.get

/-- Check if any failures were recorded. -/
def hasFailures (ctx : SoftAssertContext) : IO Bool := do
  let failures ← ctx.failures.get
  return !failures.isEmpty

/-- Get the number of recorded failures. -/
def failureCount (ctx : SoftAssertContext) : IO Nat := do
  let failures ← ctx.failures.get
  return failures.size

end SoftAssertContext

/--
Run a test block with soft assertions enabled.

All soft assertions within the block will be collected, and if any fail,
the test will fail at the end with a summary of all failures.

**Example:**
```lean
test "multiple validations" := withSoftAsserts fun soft => do
  soft.ensure (user.age >= 0) "age should be non-negative"
  soft.ensure (user.name.length > 0) "name should not be empty"
  soft.shouldBe user.status "active"
  -- Test continues even if some assertions fail
  -- All failures are reported at the end
```
-/
def withSoftAsserts (block : SoftAssertContext → IO Unit) : IO Unit := do
  let ctx ← SoftAssertContext.new
  block ctx
  let failures ← ctx.getFailures
  if !failures.isEmpty then
    let failureList := failures.toList.mapIdx fun i msg => s!"  {i + 1}. {msg}"
    let summary := s!"Soft assertion failures ({failures.size} total):\n{"\n".intercalate failureList}"
    throw <| IO.userError summary

/-- Soft version of `ensure`: records failure but doesn't throw. -/
def SoftAssertContext.ensure (ctx : SoftAssertContext) (cond : Bool) (msg : String) : IO Unit := do
  if !cond then
    ctx.addFailure s!"Expected: {msg}"

/-- Soft version of `shouldBe`: records failure but doesn't throw. -/
def SoftAssertContext.shouldBe [BEq α] [Repr α] (ctx : SoftAssertContext) (actual : α) (expected : α) : IO Unit := do
  if actual != expected then
    ctx.addFailure s!"Expected {repr expected}, got {repr actual}"

/-- Soft version of `shouldBeNear`: records failure but doesn't throw. -/
def SoftAssertContext.shouldBeNear (ctx : SoftAssertContext) (actual expected : Float) (eps : Float := 0.0001) : IO Unit := do
  if !floatNear actual expected eps then
    ctx.addFailure s!"Expected {expected} (±{eps}), got {actual}"

/-- Soft version of `shouldBeSome`: records failure but doesn't throw. -/
def SoftAssertContext.shouldBeSome [BEq α] [Repr α] (ctx : SoftAssertContext) (actual : Option α) (expected : α) : IO Unit := do
  match actual with
  | some v =>
    if v != expected then
      ctx.addFailure s!"Expected some {repr expected}, got some {repr v}"
  | none =>
    ctx.addFailure s!"Expected some {repr expected}, got none"

/-- Soft version of `shouldBeNone`: records failure but doesn't throw. -/
def SoftAssertContext.shouldBeNone [Repr α] (ctx : SoftAssertContext) (actual : Option α) : IO Unit := do
  match actual with
  | some v => ctx.addFailure s!"Expected none, got some {repr v}"
  | none => pure ()

/-- Soft version of `shouldSatisfy`: records failure but doesn't throw. -/
def SoftAssertContext.shouldSatisfy (ctx : SoftAssertContext) (cond : Bool) (msg : String := "condition") : IO Unit := do
  if !cond then
    ctx.addFailure s!"Expected {msg} to be true"

/-- Soft version of `shouldMatch`: records failure but doesn't throw. -/
def SoftAssertContext.shouldMatch [Repr α] (ctx : SoftAssertContext) (actual : α) (pred : α → Bool) (desc : String := "predicate") : IO Unit := do
  if !pred actual then
    ctx.addFailure s!"Expected {repr actual} to satisfy {desc}"

/-- Soft version of `shouldHaveLength`: records failure but doesn't throw. -/
def SoftAssertContext.shouldHaveLength [Repr α] (ctx : SoftAssertContext) (actual : List α) (expected : Nat) : IO Unit := do
  if actual.length != expected then
    ctx.addFailure s!"Expected list of length {expected}, got length {actual.length}"

/-- Soft version of `shouldContain`: records failure but doesn't throw. -/
def SoftAssertContext.shouldContain [BEq α] [Repr α] (ctx : SoftAssertContext) (actual : List α) (expected : α) : IO Unit := do
  if !actual.contains expected then
    ctx.addFailure s!"Expected list to contain {repr expected}"

/-- Soft version of `shouldContainAll`: records failure but doesn't throw. -/
def SoftAssertContext.shouldContainAll [BEq α] [Repr α] (ctx : SoftAssertContext) (actual : List α) (expected : List α) : IO Unit := do
  for item in expected do
    if !actual.contains item then
      ctx.addFailure s!"Expected list to contain {repr item}, got {repr actual}"

/-- Soft version of `shouldStartWith`: records failure but doesn't throw. -/
def SoftAssertContext.shouldStartWith (ctx : SoftAssertContext) (actual : String) (expectedPrefix : String) : IO Unit := do
  if !actual.startsWith expectedPrefix then
    ctx.addFailure s!"Expected \"{actual}\" to start with \"{expectedPrefix}\""

/-- Soft version of `shouldEndWith`: records failure but doesn't throw. -/
def SoftAssertContext.shouldEndWith (ctx : SoftAssertContext) (actual : String) (suffix : String) : IO Unit := do
  if !actual.endsWith suffix then
    ctx.addFailure s!"Expected \"{actual}\" to end with \"{suffix}\""

/-- Soft version of `shouldContainSubstr`: records failure but doesn't throw. -/
def SoftAssertContext.shouldContainSubstr (ctx : SoftAssertContext) (actual : String) (substring : String) : IO Unit := do
  if !containsSubstr actual substring then
    ctx.addFailure s!"Expected \"{actual}\" to contain \"{substring}\""

/-- Soft version of `shouldBeBetween`: records failure but doesn't throw. -/
def SoftAssertContext.shouldBeBetween [Ord α] [Repr α] (ctx : SoftAssertContext) (actual : α) (min max : α) : IO Unit := do
  match compare actual min, compare actual max with
  | .lt, _ => ctx.addFailure s!"Expected {repr actual} to be >= {repr min}"
  | _, .gt => ctx.addFailure s!"Expected {repr actual} to be <= {repr max}"
  | _, _ => pure ()

/-- Soft version of `shouldBeEmpty`: records failure but doesn't throw. -/
def SoftAssertContext.shouldBeEmpty [Repr α] (ctx : SoftAssertContext) (actual : List α) : IO Unit := do
  if !actual.isEmpty then
    ctx.addFailure s!"Expected empty list, got {repr actual}"

/-- Soft version of `shouldNotBeEmpty`: records failure but doesn't throw. -/
def SoftAssertContext.shouldNotBeEmpty [Repr α] (ctx : SoftAssertContext) (actual : List α) : IO Unit := do
  if actual.isEmpty then
    ctx.addFailure "Expected non-empty list, got []"

/--
Infix notation for equality assertion.

**Usage:** `actual ≡ expected`

**Precedence:** 50 (lower than arithmetic, higher than logical operators)

**Typing:** Type `\equiv` or copy-paste the Unicode character ≡

**Equivalent to:** `shouldBe actual expected`

**Example:**
```lean
test "addition works" := do
  1 + 1 ≡ 2
  "hello".length ≡ 5
```
-/
scoped infix:50 " ≡ " => shouldBe

/--
Infix notation for Option equality assertion.

**Usage:** `optionValue ≡? expectedValue`

**Precedence:** 50 (lower than arithmetic, higher than logical operators)

**Typing:** Type `\equiv` then `?` or copy-paste ≡?

**Equivalent to:** `shouldBeSome optionValue expectedValue`

**Example:**
```lean
test "parsing works" := do
  String.toNat? "42" ≡? 42
  list.head? ≡? expectedFirst
```
-/
scoped infix:50 " ≡? " => shouldBeSome

/-- Result of running a single test. -/
inductive TestOutcome where
  | passed : TestOutcome
  | failed : TestOutcome
  | skipped : TestOutcome
  | xfailed : TestOutcome  -- expected failure that failed (good)
  | xpassed : TestOutcome  -- expected failure that passed (bad)
  deriving Repr, Inhabited, BEq

/-- Run a single test case. Uses dedicated threads to avoid thread pool exhaustion
    when tests contain blocking FFI calls or for-loops with I/O operations. -/
private def runWithTimeout (timeoutMs : Nat) (action : IO Unit) : IO Unit := do
  let doneRef ← IO.mkRef false
  let testTask ← IO.asTask (prio := .dedicated) (do
    try
      action
      return Sum.inl ()
    finally
      doneRef.set true
  )
  let timeoutTask ← IO.asTask (prio := .dedicated) (do
    let stepMs := 50
    let steps := (timeoutMs + stepMs - 1) / stepMs
    let mut remaining := timeoutMs
    for _ in [:steps] do
      if (← doneRef.get) then
        return Sum.inl ()
      let step := Nat.min stepMs remaining
      IO.sleep step.toUInt32
      remaining := remaining - step
    return Sum.inr ()
  )
  let result ← IO.waitAny [testTask, timeoutTask]
  match result with
  | .ok (.inl ()) =>
    IO.cancel timeoutTask
    match testTask.get with
    | .ok _ => pure ()
    | .error err => throw err
  | .ok (.inr ()) =>
    IO.cancel testTask
    throw <| IO.userError s!"Test timed out after {timeoutMs}ms"
  | .error err =>
    IO.cancel timeoutTask
    throw err

/-- Run a single test case with optional beforeEach/afterEach hooks. -/
def runTest (tc : TestCase) (defaultTimeoutMs : Option Nat := none)
    (defaultRetryCount : Option Nat := none)
    (beforeEach : Option (IO Unit) := none)
    (afterEach : Option (IO Unit) := none)
    (suiteName : String := "") : IO TestOutcome := do
  let startTime ← IO.monoMsNow
  let fullName := if suiteName.isEmpty then tc.name else s!"{suiteName} > {tc.name}"

  -- Handle skipped tests first (silent - no output)
  if let some _ := tc.skip then
    return .skipped

  try
    -- Run beforeEach hook if present
    if let some hook := beforeEach then
      hook

    let timeoutMs := tc.timeoutMs.orElse (fun _ => defaultTimeoutMs)
    let retryCount := tc.retryCount.orElse (fun _ => defaultRetryCount) |>.getD 0
    let rec attempt (n : Nat) : IO Unit := do
      try
        match timeoutMs with
        | some ms => runWithTimeout ms tc.run
        | none => tc.run
      catch e =>
        if n < retryCount then
          attempt (n + 1)
        else
          throw e
    let testPassed ← try
      attempt 0
      pure true
    catch e =>
      -- Run afterEach even on failure
      if let some hook := afterEach then
        try hook catch _ => pure ()
      let elapsed := (← IO.monoMsNow) - startTime
      -- For xfail tests, catching an exception is expected (silent)
      if tc.xfail then
        return .xfailed
      IO.println s!"{Output.failSymbol} {fullName} {Output.timing elapsed}\n  {e}"
      return .failed

    -- Run afterEach hook if present (on success)
    if let some hook := afterEach then
      hook

    let elapsed := (← IO.monoMsNow) - startTime

    -- Handle xfail tests that unexpectedly passed (this is a problem, so output it)
    if tc.xfail then
      let reason := tc.xfailReason.getD "expected failure"
      IO.println s!"{Output.xpassSymbol} {fullName} (XPASS - expected to fail: {reason}) {Output.timing elapsed}"
      return .xpassed

    -- Passed tests are silent
    return .passed
  catch e =>
    let elapsed := (← IO.monoMsNow) - startTime
    -- For xfail tests, any exception means expected failure (silent)
    if tc.xfail then
      return .xfailed
    IO.println s!"{Output.failSymbol} {fullName} {Output.timing elapsed}\n  {e}"
    return .failed

/-- Run a list of test cases and report results for a single suite. -/
def runTests (name : String) (cases : List TestCase)
    (defaultTimeoutMs : Option Nat := none)
    (defaultRetryCount : Option Nat := none)
    (fixture : Fixture := {}) : IO SuiteResult := do
  let suiteStartTime ← IO.monoMsNow

  -- Run beforeAll hook if present
  if let some hook := fixture.beforeAll then
    try
      hook
    catch e =>
      IO.println s!"  {Output.failSymbol} [beforeAll failed: {e}]"
      -- If beforeAll fails, skip all tests
      let elapsed := (← IO.monoMsNow) - suiteStartTime
      return { name, passed := 0, failed := cases.length, elapsedMs := elapsed }

  let mut passed := 0
  let mut failed := 0
  let mut skipped := 0
  let mut xfailed := 0
  let mut xpassed := 0

  for tc in cases do
    let outcome ← runTest tc defaultTimeoutMs defaultRetryCount fixture.beforeEach fixture.afterEach name
    match outcome with
    | .passed => passed := passed + 1
    | .failed => failed := failed + 1
    | .skipped => skipped := skipped + 1
    | .xfailed => xfailed := xfailed + 1
    | .xpassed => xpassed := xpassed + 1

  -- Run afterAll hook if present (always, even if tests failed)
  if let some hook := fixture.afterAll then
    try
      hook
    catch e =>
      IO.println s!"  {Output.failSymbol} [afterAll failed: {e}]"

  let elapsed := (← IO.monoMsNow) - suiteStartTime
  return { name, passed, failed, skipped, xfailed, xpassed, elapsedMs := elapsed }

/-- Run a list of test cases with filtering applied. -/
def runTestsFiltered (name : String) (cases : List TestCase) (filter : TestFilter)
    (defaultTimeoutMs : Option Nat := none)
    (defaultRetryCount : Option Nat := none)
    (fixture : Fixture := {}) : IO (Option SuiteResult) := do
  -- Skip suite entirely if filter doesn't match suite name
  if !filter.matchesSuite name then
    return none

  -- Filter test cases by name
  let filteredCases := if filter.testPatterns.isEmpty then cases
    else cases.filter (fun tc => filter.matchesTest tc.name)

  -- Skip if no tests match
  if filteredCases.isEmpty then
    return none

  -- Delegate to existing runTests
  some <$> runTests name filteredCases defaultTimeoutMs defaultRetryCount fixture

/-- Format a Float with a fixed number of decimal places. -/
private def formatFloat (f : Float) (decimals : Nat) : String :=
  let factor := Float.pow 10.0 decimals.toFloat
  let rounded := Float.round (f * factor) / factor
  let s := toString rounded
  -- Ensure we have the right number of decimal places
  match s.splitOn "." with
  | [whole] => s!"{whole}.{"0".intercalate (List.replicate decimals "")}"
  | [whole, frac] =>
    if frac.length < decimals then
      s!"{whole}.{frac}{"0".intercalate (List.replicate (decimals - frac.length) "")}"
    else
      s!"{whole}.{frac.take decimals}"
  | _ => s

/-- Print a summary of test results across all suites. -/
def printSummary (results : TestResults) : IO Unit := do
  let total := results.total
  let effectivePassed := results.passed + results.xfailed  -- xfailed counts as "good"
  let pct := if total > 0 then (effectivePassed.toFloat / total.toFloat) * 100.0 else 100.0
  let secs := results.totalElapsedMs.toFloat / 1000.0
  IO.println ""
  IO.println (Output.dim "────────────────────────────────────────")
  -- Build summary line with all non-zero counts (colored)
  let mut parts : List String := []
  parts := parts ++ [Output.boldGreen s!"{results.passed} passed"]
  parts := parts ++ [if results.failed > 0 then Output.boldRed s!"{results.failed} failed"
                     else Output.dim s!"{results.failed} failed"]
  if results.skipped > 0 then parts := parts ++ [Output.yellow s!"{results.skipped} skipped"]
  if results.xfailed > 0 then parts := parts ++ [Output.yellow s!"{results.xfailed} xfailed"]
  if results.xpassed > 0 then parts := parts ++ [Output.red s!"{results.xpassed} xpassed"]
  -- Color the percentage based on pass rate
  let pctStr := formatFloat pct 1
  let pctColored := if pct >= 100.0 then Output.boldGreen s!"{pctStr}%"
                    else if pct >= 80.0 then Output.green s!"{pctStr}%"
                    else if pct >= 50.0 then Output.yellow s!"{pctStr}%"
                    else Output.red s!"{pctStr}%"
  IO.println s!"Summary: {", ".intercalate parts} ({pctColored})"
  IO.println s!"         {results.suiteCount} suites, {total} tests run"
  if results.skipped > 0 then
    IO.println s!"         {Output.yellow s!"{results.skipped} tests skipped"}"
  IO.println s!"         Completed in {formatFloat secs 2}s"
  IO.println (Output.dim "────────────────────────────────────────")

/-- Print a summary of test results with filter information. -/
def printFilteredSummary (results : TestResults) (filter : TestFilter) : IO Unit := do
  printSummary results
  -- Print filter info if any filters were applied
  if !filter.matchesAll then
    IO.println ""
    if !filter.suitePatterns.isEmpty then
      IO.println s!"  Suite filter: {filter.suitePatterns}"
    if !filter.testPatterns.isEmpty then
      IO.println s!"  Test filter: {filter.testPatterns}"
    if filter.exactMatch then
      IO.println "  Match mode: exact"

/-! ## Automatic Suite Runner -/

open Lean Elab Term

/-- Run all registered test suites discovered via `testSuite`. Returns exit code (0 = pass, 1 = fail). -/
syntax (name := runAllSuitesTerm) "runAllSuites" : term

/-- Run all registered test suites and return structured TestResults. -/
syntax (name := runAllSuitesWithResultsTerm) "runAllSuitesWithResults" : term

/-- Run all registered test suites discovered via `testSuite` with a timeout. -/
syntax (name := runAllSuitesTermTimeout) "runAllSuites" "(" "timeout" ":=" term ")" : term

/-- Run all registered test suites with timeout and return structured TestResults. -/
syntax (name := runAllSuitesWithResultsTermTimeout) "runAllSuitesWithResults" "(" "timeout" ":=" term ")" : term

-- Core elaborator with returnResults flag for backwards compatibility
-- returnResults = false: return IO UInt32 (exit code) - backwards compatible
-- returnResults = true: return IO TestResults (structured results) - new API
private def elabRunAllSuitesCore (timeoutOpt : Option (TSyntax `term))
    (retryOpt : Option (TSyntax `term))
    (returnResults : Bool)
    (expectedType? : Option Expr) : TermElabM Expr := do
  let env ← getEnv
  let ref ← getRef
  -- Each entry is (name, cases, fixture)
  let mut suiteEntries : Array (TSyntax `term) := #[]

  for suite in SuiteRegistry.getAllSuites env do
    let suiteNameLit : TSyntax `term := ⟨Syntax.mkStrLit suite.suiteName⟩
    -- Query the test case extension directly for automatic discovery
    let suiteTests := SuiteRegistry.getTestsForSuite env suite.ns
    let casesTerm : TSyntax `term ←
      if suiteTests.isEmpty then
        `(term| ([] : List _root_.Crucible.TestCase))
      else
        let testIdents : Array (TSyntax `term) := suiteTests.map fun testName =>
          mkIdentFrom ref testName (canonical := true)
        `(term| [$[$testIdents],*])

    -- Check for fixture hooks
    let beforeAllName := suite.ns ++ `beforeAll
    let afterAllName := suite.ns ++ `afterAll
    let beforeEachName := suite.ns ++ `beforeEach
    let afterEachName := suite.ns ++ `afterEach

    let beforeAllTerm : TSyntax `term ←
      if env.contains beforeAllName then
        let ident := mkIdentFrom ref beforeAllName (canonical := true)
        `(term| some $ident)
      else
        `(term| none)

    let afterAllTerm : TSyntax `term ←
      if env.contains afterAllName then
        let ident := mkIdentFrom ref afterAllName (canonical := true)
        `(term| some $ident)
      else
        `(term| none)

    let beforeEachTerm : TSyntax `term ←
      if env.contains beforeEachName then
        let ident := mkIdentFrom ref beforeEachName (canonical := true)
        `(term| some $ident)
      else
        `(term| none)

    let afterEachTerm : TSyntax `term ←
      if env.contains afterEachName then
        let ident := mkIdentFrom ref afterEachName (canonical := true)
        `(term| some $ident)
      else
        `(term| none)

    let fixtureTerm : TSyntax `term ← `(term| ({
      beforeAll := $beforeAllTerm
      afterAll := $afterAllTerm
      beforeEach := $beforeEachTerm
      afterEach := $afterEachTerm
    } : _root_.Crucible.Fixture))

    let entry ← `(term| ($suiteNameLit, $casesTerm, $fixtureTerm))
    suiteEntries := suiteEntries.push entry

  let timeoutTerm : TSyntax `term ←
    match timeoutOpt with
    | some t => `(term| some $t)
    | none => `(term| none)

  let retryTerm : TSyntax `term ←
    match retryOpt with
    | some r => `(term| some $r)
    | none => `(term| none)

  let body : TSyntax `term ←
    if returnResults then
      `(term| do
        let startTime ← IO.monoMsNow
        let suites : Array (String × List _root_.Crucible.TestCase × _root_.Crucible.Fixture) := #[$[$suiteEntries],*]
        let mut results : _root_.Crucible.TestResults := {}
        for (name, cases, fixture) in suites do
          let suiteResult ← _root_.Crucible.runTests name cases $timeoutTerm $retryTerm fixture
          results := results.addSuite suiteResult
        let endTime ← IO.monoMsNow
        let finalResults := { results with totalElapsedMs := endTime - startTime }
        _root_.Crucible.printSummary finalResults
        return finalResults
      )
    else
      `(term| do
        let startTime ← IO.monoMsNow
        let suites : Array (String × List _root_.Crucible.TestCase × _root_.Crucible.Fixture) := #[$[$suiteEntries],*]
        let mut results : _root_.Crucible.TestResults := {}
        for (name, cases, fixture) in suites do
          let suiteResult ← _root_.Crucible.runTests name cases $timeoutTerm $retryTerm fixture
          results := results.addSuite suiteResult
        let endTime ← IO.monoMsNow
        let finalResults := { results with totalElapsedMs := endTime - startTime }
        _root_.Crucible.printSummary finalResults
        return finalResults.toExitCode
      )

  elabTerm body expectedType?

@[term_elab runAllSuitesTerm]
def elabRunAllSuites : TermElab := fun _stx expectedType? =>
  elabRunAllSuitesCore none none false expectedType?

@[term_elab runAllSuitesWithResultsTerm]
def elabRunAllSuitesWithResults : TermElab := fun _stx expectedType? =>
  elabRunAllSuitesCore none none true expectedType?

@[term_elab runAllSuitesTermTimeout]
def elabRunAllSuitesTimeout : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: _ :: _ :: timeoutStx :: _ =>
      elabRunAllSuitesCore (some ⟨timeoutStx⟩) none false expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[term_elab runAllSuitesWithResultsTermTimeout]
def elabRunAllSuitesWithResultsTimeout : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: _ :: _ :: timeoutStx :: _ =>
      elabRunAllSuitesCore (some ⟨timeoutStx⟩) none true expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-- Run all registered test suites discovered via `testSuite` with a retry count. -/
syntax (name := runAllSuitesTermRetry) "runAllSuites" "(" "retry" ":=" term ")" : term

@[term_elab runAllSuitesTermRetry]
def elabRunAllSuitesRetry : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: _ :: _ :: retryStx :: _ =>
      elabRunAllSuitesCore none (some ⟨retryStx⟩) false expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-- Run all registered test suites discovered via `testSuite` with timeout and retry. -/
syntax (name := runAllSuitesTermTimeoutRetry)
  "runAllSuites" "(" "timeout" ":=" term ")" "(" "retry" ":=" term ")" : term

@[term_elab runAllSuitesTermTimeoutRetry]
def elabRunAllSuitesTimeoutRetry : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: _ :: _ :: timeoutStx :: _ :: _ :: _ :: _ :: retryStx :: _ =>
      elabRunAllSuitesCore (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) false expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-- Run all registered test suites discovered via `testSuite` with retry and timeout. -/
syntax (name := runAllSuitesTermRetryTimeout)
  "runAllSuites" "(" "retry" ":=" term ")" "(" "timeout" ":=" term ")" : term

@[term_elab runAllSuitesTermRetryTimeout]
def elabRunAllSuitesRetryTimeout : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: _ :: _ :: retryStx :: _ :: _ :: _ :: _ :: timeoutStx :: _ =>
      elabRunAllSuitesCore (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) false expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-! ## Non-filtered Suite Runner (WithResults Variants) -/

/-- Run all suites with retry and return structured TestResults. -/
syntax (name := runAllSuitesWithResultsTermRetry)
  "runAllSuitesWithResults" "(" "retry" ":=" term ")" : term

@[term_elab runAllSuitesWithResultsTermRetry]
def elabRunAllSuitesWithResultsRetry : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: _ :: _ :: retryStx :: _ =>
      elabRunAllSuitesCore none (some ⟨retryStx⟩) true expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-- Run all suites with timeout, retry, and return structured TestResults. -/
syntax (name := runAllSuitesWithResultsTermTimeoutRetry)
  "runAllSuitesWithResults" "(" "timeout" ":=" term ")" "(" "retry" ":=" term ")" : term

@[term_elab runAllSuitesWithResultsTermTimeoutRetry]
def elabRunAllSuitesWithResultsTimeoutRetry : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: _ :: _ :: timeoutStx :: _ :: _ :: _ :: _ :: retryStx :: _ =>
      elabRunAllSuitesCore (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) true expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-- Run all suites with retry, timeout, and return structured TestResults. -/
syntax (name := runAllSuitesWithResultsTermRetryTimeout)
  "runAllSuitesWithResults" "(" "retry" ":=" term ")" "(" "timeout" ":=" term ")" : term

@[term_elab runAllSuitesWithResultsTermRetryTimeout]
def elabRunAllSuitesWithResultsRetryTimeout : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: _ :: _ :: retryStx :: _ :: _ :: _ :: _ :: timeoutStx :: _ =>
      elabRunAllSuitesCore (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) true expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-! ## Filtered Suite Runner -/

/-- Run all registered test suites with command-line filtering support.
    Use this instead of `runAllSuites` when you want to filter tests via CLI args.

    **Usage:**
    ```lean
    def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
    ```

    **Command-line options:**
    - `--test PATTERN` or `-t PATTERN`: Run only tests matching PATTERN
    - `--suite PATTERN` or `-s PATTERN`: Run only suites matching PATTERN
    - `--exact` or `-e`: Use exact match instead of substring
    - `--help` or `-h`: Show help message

    **Examples:**
    ```bash
    lake test -- --test parse           # Tests containing "parse"
    lake test -- --suite "HTTP Parser"  # Suites containing "HTTP Parser"
    lake test -- -t foo -t bar          # Tests matching "foo" OR "bar"
    ```
-/
syntax (name := runAllSuitesFilteredTerm) "runAllSuitesFiltered" term : term

/-- Run all suites with filtering and timeout. -/
syntax (name := runAllSuitesFilteredTimeoutTerm)
  "runAllSuitesFiltered" term "(" "timeout" ":=" term ")" : term

/-- Run all suites with filtering and retry. -/
syntax (name := runAllSuitesFilteredRetryTerm)
  "runAllSuitesFiltered" term "(" "retry" ":=" term ")" : term

/-- Run all suites with filtering, timeout, and retry. -/
syntax (name := runAllSuitesFilteredTimeoutRetryTerm)
  "runAllSuitesFiltered" term "(" "timeout" ":=" term ")" "(" "retry" ":=" term ")" : term

/-- Run all suites with filtering, retry, and timeout. -/
syntax (name := runAllSuitesFilteredRetryTimeoutTerm)
  "runAllSuitesFiltered" term "(" "retry" ":=" term ")" "(" "timeout" ":=" term ")" : term

-- Core elaborator with returnResults flag for backwards compatibility
-- returnResults = false: return IO UInt32 (exit code) - backwards compatible
-- returnResults = true: return IO TestResults (structured results) - new API
private def elabRunAllSuitesFilteredCore (argsStx : TSyntax `term)
    (timeoutOpt : Option (TSyntax `term))
    (retryOpt : Option (TSyntax `term))
    (returnResults : Bool)
    (expectedType? : Option Expr) : TermElabM Expr := do
  let env ← getEnv
  let ref ← getRef
  -- Each entry is (name, cases, fixture)
  let mut suiteEntries : Array (TSyntax `term) := #[]

  for suite in SuiteRegistry.getAllSuites env do
    let suiteNameLit : TSyntax `term := ⟨Syntax.mkStrLit suite.suiteName⟩
    -- Query the test case extension directly for automatic discovery
    let suiteTests := SuiteRegistry.getTestsForSuite env suite.ns
    let casesTerm : TSyntax `term ←
      if suiteTests.isEmpty then
        `(term| ([] : List _root_.Crucible.TestCase))
      else
        let testIdents : Array (TSyntax `term) := suiteTests.map fun testName =>
          mkIdentFrom ref testName (canonical := true)
        `(term| [$[$testIdents],*])

    -- Check for fixture hooks
    let beforeAllName := suite.ns ++ `beforeAll
    let afterAllName := suite.ns ++ `afterAll
    let beforeEachName := suite.ns ++ `beforeEach
    let afterEachName := suite.ns ++ `afterEach

    let beforeAllTerm : TSyntax `term ←
      if env.contains beforeAllName then
        let ident := mkIdentFrom ref beforeAllName (canonical := true)
        `(term| some $ident)
      else
        `(term| none)

    let afterAllTerm : TSyntax `term ←
      if env.contains afterAllName then
        let ident := mkIdentFrom ref afterAllName (canonical := true)
        `(term| some $ident)
      else
        `(term| none)

    let beforeEachTerm : TSyntax `term ←
      if env.contains beforeEachName then
        let ident := mkIdentFrom ref beforeEachName (canonical := true)
        `(term| some $ident)
      else
        `(term| none)

    let afterEachTerm : TSyntax `term ←
      if env.contains afterEachName then
        let ident := mkIdentFrom ref afterEachName (canonical := true)
        `(term| some $ident)
      else
        `(term| none)

    let fixtureTerm : TSyntax `term ← `(term| ({
      beforeAll := $beforeAllTerm
      afterAll := $afterAllTerm
      beforeEach := $beforeEachTerm
      afterEach := $afterEachTerm
    } : _root_.Crucible.Fixture))

    let entry ← `(term| ($suiteNameLit, $casesTerm, $fixtureTerm))
    suiteEntries := suiteEntries.push entry

  let timeoutTerm : TSyntax `term ←
    match timeoutOpt with
    | some t => `(term| some $t)
    | none => `(term| none)

  let retryTerm : TSyntax `term ←
    match retryOpt with
    | some r => `(term| some $r)
    | none => `(term| none)

  -- Generate body with filter support - args come from parameter
  let body : TSyntax `term ←
    if returnResults then
      `(term| do
        let args : List String := $argsStx
        -- Check for help first
        if _root_.Crucible.CLI.helpRequested args then
          _root_.Crucible.CLI.printHelp
          return {}  -- Return empty results on help
        let filter ← _root_.Crucible.CLI.parseArgs args
        let startTime ← IO.monoMsNow
        let suites : Array (String × List _root_.Crucible.TestCase × _root_.Crucible.Fixture) := #[$[$suiteEntries],*]
        let mut results : _root_.Crucible.TestResults := {}
        for (name, cases, fixture) in suites do
          if let some suiteResult ← _root_.Crucible.runTestsFiltered name cases filter $timeoutTerm $retryTerm fixture then
            results := results.addSuite suiteResult
        let endTime ← IO.monoMsNow
        let finalResults := { results with totalElapsedMs := endTime - startTime }
        _root_.Crucible.printFilteredSummary finalResults filter
        return finalResults
      )
    else
      `(term| do
        let args : List String := $argsStx
        -- Check for help first
        if _root_.Crucible.CLI.helpRequested args then
          _root_.Crucible.CLI.printHelp
          return 0  -- Return success on help
        let filter ← _root_.Crucible.CLI.parseArgs args
        let startTime ← IO.monoMsNow
        let suites : Array (String × List _root_.Crucible.TestCase × _root_.Crucible.Fixture) := #[$[$suiteEntries],*]
        let mut results : _root_.Crucible.TestResults := {}
        for (name, cases, fixture) in suites do
          if let some suiteResult ← _root_.Crucible.runTestsFiltered name cases filter $timeoutTerm $retryTerm fixture then
            results := results.addSuite suiteResult
        let endTime ← IO.monoMsNow
        let finalResults := { results with totalElapsedMs := endTime - startTime }
        _root_.Crucible.printFilteredSummary finalResults filter
        return finalResults.toExitCode
      )

  elabTerm body expectedType?

@[term_elab runAllSuitesFilteredTerm]
def elabRunAllSuitesFiltered : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: argsStx :: _ =>
      elabRunAllSuitesFilteredCore ⟨argsStx⟩ none none false expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[term_elab runAllSuitesFilteredTimeoutTerm]
def elabRunAllSuitesFilteredTimeout : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: argsStx :: _ :: _ :: _ :: timeoutStx :: _ =>
      elabRunAllSuitesFilteredCore ⟨argsStx⟩ (some ⟨timeoutStx⟩) none false expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[term_elab runAllSuitesFilteredRetryTerm]
def elabRunAllSuitesFilteredRetry : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: argsStx :: _ :: _ :: _ :: retryStx :: _ =>
      elabRunAllSuitesFilteredCore ⟨argsStx⟩ none (some ⟨retryStx⟩) false expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[term_elab runAllSuitesFilteredTimeoutRetryTerm]
def elabRunAllSuitesFilteredTimeoutRetry : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: argsStx :: _ :: _ :: _ :: timeoutStx :: _ :: _ :: _ :: _ :: retryStx :: _ =>
      elabRunAllSuitesFilteredCore ⟨argsStx⟩ (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) false expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[term_elab runAllSuitesFilteredRetryTimeoutTerm]
def elabRunAllSuitesFilteredRetryTimeout : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: argsStx :: _ :: _ :: _ :: retryStx :: _ :: _ :: _ :: _ :: timeoutStx :: _ =>
      elabRunAllSuitesFilteredCore ⟨argsStx⟩ (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) false expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-! ## Filtered Suite Runner (WithResults Variants) -/

/-- Run all suites with filtering and return structured TestResults. -/
syntax (name := runAllSuitesFilteredWithResultsTerm) "runAllSuitesFilteredWithResults" term : term

/-- Run all suites with filtering, timeout, and return structured TestResults. -/
syntax (name := runAllSuitesFilteredWithResultsTimeoutTerm)
  "runAllSuitesFilteredWithResults" term "(" "timeout" ":=" term ")" : term

/-- Run all suites with filtering, retry, and return structured TestResults. -/
syntax (name := runAllSuitesFilteredWithResultsRetryTerm)
  "runAllSuitesFilteredWithResults" term "(" "retry" ":=" term ")" : term

/-- Run all suites with filtering, timeout, retry, and return structured TestResults. -/
syntax (name := runAllSuitesFilteredWithResultsTimeoutRetryTerm)
  "runAllSuitesFilteredWithResults" term "(" "timeout" ":=" term ")" "(" "retry" ":=" term ")" : term

/-- Run all suites with filtering, retry, timeout, and return structured TestResults. -/
syntax (name := runAllSuitesFilteredWithResultsRetryTimeoutTerm)
  "runAllSuitesFilteredWithResults" term "(" "retry" ":=" term ")" "(" "timeout" ":=" term ")" : term

@[term_elab runAllSuitesFilteredWithResultsTerm]
def elabRunAllSuitesFilteredWithResults : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: argsStx :: _ =>
      elabRunAllSuitesFilteredCore ⟨argsStx⟩ none none true expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[term_elab runAllSuitesFilteredWithResultsTimeoutTerm]
def elabRunAllSuitesFilteredWithResultsTimeout : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: argsStx :: _ :: _ :: _ :: timeoutStx :: _ =>
      elabRunAllSuitesFilteredCore ⟨argsStx⟩ (some ⟨timeoutStx⟩) none true expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[term_elab runAllSuitesFilteredWithResultsRetryTerm]
def elabRunAllSuitesFilteredWithResultsRetry : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: argsStx :: _ :: _ :: _ :: retryStx :: _ =>
      elabRunAllSuitesFilteredCore ⟨argsStx⟩ none (some ⟨retryStx⟩) true expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[term_elab runAllSuitesFilteredWithResultsTimeoutRetryTerm]
def elabRunAllSuitesFilteredWithResultsTimeoutRetry : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: argsStx :: _ :: _ :: _ :: timeoutStx :: _ :: _ :: _ :: _ :: retryStx :: _ =>
      elabRunAllSuitesFilteredCore ⟨argsStx⟩ (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) true expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[term_elab runAllSuitesFilteredWithResultsRetryTimeoutTerm]
def elabRunAllSuitesFilteredWithResultsRetryTimeout : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: argsStx :: _ :: _ :: _ :: retryStx :: _ :: _ :: _ :: _ :: timeoutStx :: _ =>
      elabRunAllSuitesFilteredCore ⟨argsStx⟩ (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) true expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

end Crucible
