import Lean
import Crucible.SuiteRegistry

/-!
# Core Test Framework Types

Defines the fundamental TestCase structure and assertion functions.
-/

namespace Crucible

/-- Check if a string contains a substring. -/
private def containsSubstr (s : String) (sub : String) : Bool :=
  (s.splitOn sub).length > 1

/-- Helper to await a task and return its result. -/
def awaitTask (task : IO (Task α)) : IO α := do
  let t ← task
  return t.get

/-- A test case with a name and a monadic test action. -/
structure TestCase where
  name : String
  run : IO Unit
  timeoutMs : Option Nat := none
  retryCount : Option Nat := none

/-- Return a copy of the test case with a timeout configured (in milliseconds). -/
def TestCase.withTimeout (tc : TestCase) (timeoutMs : Nat) : TestCase :=
  { tc with timeoutMs := some timeoutMs }

/-- Return a copy of the test case with a retry count configured. -/
def TestCase.withRetry (tc : TestCase) (retryCount : Nat) : TestCase :=
  { tc with retryCount := some retryCount }

/-- Results from running a test suite. -/
structure TestResults where
  passed : Nat := 0
  failed : Nat := 0
  deriving Repr

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

/-- Total number of tests run. -/
def TestResults.total (r : TestResults) : Nat := r.passed + r.failed

/-- Merge two test results by summing their counts. -/
def TestResults.merge (a b : TestResults) : TestResults :=
  { passed := a.passed + b.passed, failed := a.failed + b.failed }

/-- Assert that a condition is true. -/
def ensure (cond : Bool) (msg : String) : IO Unit := do
  if !cond then
    throw <| IO.userError s!"Assertion failed: {msg}"

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
    throw <| IO.userError s!"Assertion failed: {msg}\n  expected: {repr expected}\n  actual:   {repr actual}"

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
    throw <| IO.userError s!"Expected list to contain {repr expected}, but it doesn't"

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
      throw <| IO.userError s!"Expected list to contain {repr item}, but it doesn't.\n  List: {repr actual}"

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
  | .error e => throw <| IO.userError s!"{context} failed: {e}"

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

/-- Run a single test case. Uses dedicated threads to avoid thread pool exhaustion
    when tests contain blocking FFI calls or for-loops with I/O operations. -/
private def runWithTimeout (timeoutMs : Nat) (action : IO Unit) : IO Unit := do
  let testTask ← IO.asTask (prio := .dedicated) (do
    action
    return Sum.inl ()
  )
  let timeoutTask ← IO.asTask (prio := .dedicated) (do
    IO.sleep (UInt32.ofNat timeoutMs)
    return Sum.inr ()
  )
  let result ← IO.waitAny [testTask, timeoutTask]
  match result with
  | .ok (.inl ()) =>
    IO.cancel timeoutTask
    pure ()
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
    (afterEach : Option (IO Unit) := none) : IO Bool := do
  IO.print s!"  {tc.name}... "
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
    let result ← try
      attempt 0
      pure true
    catch e =>
      -- Run afterEach even on failure
      if let some hook := afterEach then
        try hook catch _ => pure ()
      throw e

    -- Run afterEach hook if present (on success)
    if let some hook := afterEach then
      hook

    IO.println "✓"
    return true
  catch e =>
    IO.println s!"✗\n    {e}"
    return false

/-- Run a list of test cases and report results. -/
def runTests (name : String) (cases : List TestCase)
    (defaultTimeoutMs : Option Nat := none)
    (defaultRetryCount : Option Nat := none)
    (fixture : Fixture := {}) : IO TestResults := do
  IO.println s!"\n{name}"
  IO.println ("─".intercalate (List.replicate name.length ""))

  -- Run beforeAll hook if present
  if let some hook := fixture.beforeAll then
    try
      hook
    catch e =>
      IO.println s!"  [beforeAll failed: {e}]"
      -- If beforeAll fails, skip all tests
      return { passed := 0, failed := cases.length }

  let mut passed := 0
  let mut failed := 0

  for tc in cases do
    if ← runTest tc defaultTimeoutMs defaultRetryCount fixture.beforeEach fixture.afterEach then
      passed := passed + 1
    else
      failed := failed + 1

  -- Run afterAll hook if present (always, even if tests failed)
  if let some hook := fixture.afterAll then
    try
      hook
    catch e =>
      IO.println s!"  [afterAll failed: {e}]"

  IO.println s!"\nResults: {passed} passed, {failed} failed"
  return { passed, failed }

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
def printSummary (results : TestResults) (suiteCount : Nat) (elapsedMs : Nat) : IO Unit := do
  let total := results.total
  let pct := if total > 0 then (results.passed.toFloat / total.toFloat) * 100.0 else 100.0
  let secs := elapsedMs.toFloat / 1000.0
  IO.println ""
  IO.println "────────────────────────────────────────"
  IO.println s!"Summary: {results.passed} passed, {results.failed} failed ({formatFloat pct 1}%)"
  IO.println s!"         {suiteCount} suites, {total} tests total"
  IO.println s!"         Completed in {formatFloat secs 2}s"
  IO.println "────────────────────────────────────────"

/-! ## Automatic Suite Runner -/

open Lean Elab Term

/-- Run all registered test suites discovered via `testSuite`. -/
syntax (name := runAllSuitesTerm) "runAllSuites" : term

/-- Run all registered test suites discovered via `testSuite` with a timeout. -/
syntax (name := runAllSuitesTermTimeout) "runAllSuites" "(" "timeout" ":=" term ")" : term

-- Syntax for a timeout-enabled suite runner.

private def elabRunAllSuitesCore (timeoutOpt : Option (TSyntax `term))
    (retryOpt : Option (TSyntax `term))
    (expectedType? : Option Expr) : TermElabM Expr := do
  let env ← getEnv
  let ref ← getRef
  -- Each entry is (name, cases, fixture)
  let mut suiteEntries : Array (TSyntax `term) := #[]

  for suite in SuiteRegistry.getAllSuites env do
    let suiteNameLit : TSyntax `term := ⟨Syntax.mkStrLit suite.suiteName⟩
    let casesName := SuiteRegistry.suiteCasesName suite
    let casesIdent := mkIdentFrom ref casesName (canonical := true)
    let casesTerm : TSyntax `term ←
      if env.contains casesName then
        `(term| $casesIdent)
      else
        logWarning s!"No `cases` definition found for suite {suite.suiteName} ({suite.ns}). Did you forget #generate_tests?"
        `(term| ([] : List _root_.Crucible.TestCase))

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

  let body : TSyntax `term ← `(term| do
    let startTime ← IO.monoMsNow
    let suites : Array (String × List _root_.Crucible.TestCase × _root_.Crucible.Fixture) := #[$[$suiteEntries],*]
    let mut results : _root_.Crucible.TestResults := { passed := 0, failed := 0 }
    for (name, cases, fixture) in suites do
      let r ← _root_.Crucible.runTests name cases $timeoutTerm $retryTerm fixture
      results := _root_.Crucible.TestResults.merge results r
    let endTime ← IO.monoMsNow
    _root_.Crucible.printSummary results suites.size (endTime - startTime)
    return if results.failed > 0 then 1 else 0
  )

  elabTerm body expectedType?

@[term_elab runAllSuitesTerm]
def elabRunAllSuites : TermElab := fun _stx expectedType? =>
  elabRunAllSuitesCore none none expectedType?

@[term_elab runAllSuitesTermTimeout]
def elabRunAllSuitesTimeout : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: _ :: _ :: timeoutStx :: _ =>
      elabRunAllSuitesCore (some ⟨timeoutStx⟩) none expectedType?
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
      elabRunAllSuitesCore none (some ⟨retryStx⟩) expectedType?
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
      elabRunAllSuitesCore (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) expectedType?
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
      elabRunAllSuitesCore (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

end Crucible
