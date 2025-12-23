import Lean
import Crucible.SuiteRegistry

/-!
# Core Test Framework Types

Defines the fundamental TestCase structure and assertion functions.
-/

namespace Crucible

/-- A test case with a name and a monadic test action. -/
structure TestCase where
  name : String
  run : IO Unit
  timeoutMs : Option Nat := none

/-- Return a copy of the test case with a timeout configured (in milliseconds). -/
def TestCase.withTimeout (tc : TestCase) (timeoutMs : Nat) : TestCase :=
  { tc with timeoutMs := some timeoutMs }

/-- Assert that a condition is true. -/
def ensure (cond : Bool) (msg : String) : IO Unit := do
  if !cond then
    throw <| IO.userError s!"Assertion failed: {msg}"

/-- Assert that two values are equal (legacy signature for backwards compatibility). -/
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

-- Infix notation for even cleaner test syntax
scoped infix:50 " ≡ " => shouldBe
scoped infix:50 " ≡? " => shouldBeSome

/-- Run a single test case. -/
private def runWithTimeout (timeoutMs : Nat) (action : IO Unit) : IO Unit := do
  let testTask ← IO.asTask (do
    action
    return Sum.inl ()
  )
  let timeoutTask ← IO.asTask (do
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

/-- Run a single test case. -/
def runTest (tc : TestCase) (defaultTimeoutMs : Option Nat := none) : IO Bool := do
  IO.print s!"  {tc.name}... "
  try
    let timeoutMs := tc.timeoutMs.orElse (fun _ => defaultTimeoutMs)
    match timeoutMs with
    | some ms => runWithTimeout ms tc.run
    | none => tc.run
    IO.println "✓"
    return true
  catch e =>
    IO.println s!"✗\n    {e}"
    return false

/-- Run a list of test cases and report results. -/
def runTests (name : String) (cases : List TestCase)
    (defaultTimeoutMs : Option Nat := none) : IO UInt32 := do
  IO.println s!"\n{name}"
  IO.println ("─".intercalate (List.replicate name.length ""))
  let mut passed := 0
  let mut failed := 0
  for tc in cases do
    if ← runTest tc defaultTimeoutMs then
      passed := passed + 1
    else
      failed := failed + 1
  IO.println s!"\nResults: {passed} passed, {failed} failed"
  return if failed > 0 then 1 else 0

/-! ## Automatic Suite Runner -/

open Lean Elab Term

/-- Run all registered test suites discovered via `testSuite`. -/
syntax (name := runAllSuitesTerm) "runAllSuites" : term

/-- Run all registered test suites discovered via `testSuite` with a timeout. -/
syntax (name := runAllSuitesTermTimeout) "runAllSuites" "(" "timeout" ":=" term ")" : term

-- Syntax for a timeout-enabled suite runner.

private def elabRunAllSuitesCore (timeoutOpt : Option (TSyntax `term))
    (expectedType? : Option Expr) : TermElabM Expr := do
  let env ← getEnv
  let ref ← getRef
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

    let entry ← `(term| ($suiteNameLit, $casesTerm))
    suiteEntries := suiteEntries.push entry

  let timeoutTerm : TSyntax `term ←
    match timeoutOpt with
    | some t => `(term| some $t)
    | none => `(term| none)

  let body : TSyntax `term ← `(term| do
    let suites : Array (String × List _root_.Crucible.TestCase) := #[$[$suiteEntries],*]
    let mut exitCode : UInt32 := 0
    for (name, cases) in suites do
      exitCode := exitCode + (← _root_.Crucible.runTests name cases $timeoutTerm)
    return exitCode
  )

  elabTerm body expectedType?

@[term_elab runAllSuitesTerm]
def elabRunAllSuites : TermElab := fun _stx expectedType? =>
  elabRunAllSuitesCore none expectedType?

@[term_elab runAllSuitesTermTimeout]
def elabRunAllSuitesTimeout : TermElab := fun stx expectedType? => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: _ :: _ :: timeoutStx :: _ =>
      elabRunAllSuitesCore (some ⟨timeoutStx⟩) expectedType?
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

end Crucible
