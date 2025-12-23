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
def runTest (tc : TestCase) : IO Bool := do
  IO.print s!"  {tc.name}... "
  try
    tc.run
    IO.println "✓"
    return true
  catch e =>
    IO.println s!"✗\n    {e}"
    return false

/-- Run a list of test cases and report results. -/
def runTests (name : String) (cases : List TestCase) : IO UInt32 := do
  IO.println s!"\n{name}"
  IO.println ("─".intercalate (List.replicate name.length ""))
  let mut passed := 0
  let mut failed := 0
  for tc in cases do
    if ← runTest tc then
      passed := passed + 1
    else
      failed := failed + 1
  IO.println s!"\nResults: {passed} passed, {failed} failed"
  return if failed > 0 then 1 else 0

/-! ## Automatic Suite Runner -/

open Lean Elab Term

/-- Run all registered test suites discovered via `testSuite`. -/
syntax (name := runAllSuitesTerm) "runAllSuites" : term

@[term_elab runAllSuitesTerm]
def elabRunAllSuites : TermElab := fun _stx expectedType? => do
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

  let body : TSyntax `term ← `(term| do
    let suites : Array (String × List _root_.Crucible.TestCase) := #[$[$suiteEntries],*]
    let mut exitCode : UInt32 := 0
    for (name, cases) in suites do
      exitCode := exitCode + (← _root_.Crucible.runTests name cases)
    return exitCode
  )

  elabTerm body expectedType?

end Crucible
