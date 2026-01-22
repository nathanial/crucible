import Lean
import Crucible.Core

/-!
# Test Definition Macros

This module provides the DSL for defining tests:

## Test Syntax

```lean
-- Basic test
test "description" := do
  assertion1
  assertion2

-- With timeout (milliseconds)
test "slow test" (timeout := 5000) := do
  ...

-- With retry count
test "flaky test" (retry := 3) := do
  ...

-- Both timeout and retry
test "network test" (timeout := 10000) (retry := 2) := do
  ...

-- Skipped test (won't run)
test "not ready" (skip := "waiting on feature X") := do
  ...

-- Expected failure (should fail, passing is error)
test "known bug" (xfail := "issue #42") := do
  ...
```

## Fixture Hooks

Define hooks in your test namespace:

```lean
beforeAll := do
  -- Run once before all tests in suite

afterAll := do
  -- Run once after all tests (even if tests fail)

beforeEach := do
  -- Run before each test

afterEach := do
  -- Run after each test (even if test fails)
```

## How It Works

1. Each `test "name" := do body` creates a `TestCase` definition
2. Test names are registered in `testCaseExtension` (in SuiteRegistry)
3. `runAllSuites` discovers suites via `testSuite` and queries the extension for tests

Test discovery is fully automatic.
-/

namespace Crucible.Macros

open Lean Elab Command Meta
open Crucible.SuiteRegistry (testCaseExtension)

/-! ## Test Syntax -/

/-- Syntax for defining a test case: `test "description" := do body` -/
syntax (name := testDecl) "test " str " := " doSeq : command

/-- Syntax for defining a test case with timeout: `test "description" (timeout := 5000) := do body` -/
syntax (name := testDeclTimeout) "test " str "(" "timeout" ":=" term ")" " := " doSeq : command

/-- Syntax for defining a test case with retry: `test "description" (retry := 3) := do body` -/
syntax (name := testDeclRetry) "test " str "(" "retry" ":=" term ")" " := " doSeq : command

/-- Syntax for defining a test case with timeout and retry. -/
syntax (name := testDeclTimeoutRetry)
  "test " str "(" "timeout" ":=" term ")" "(" "retry" ":=" term ")" " := " doSeq : command

/-- Syntax for defining a test case with retry and timeout. -/
syntax (name := testDeclRetryTimeout)
  "test " str "(" "retry" ":=" term ")" "(" "timeout" ":=" term ")" " := " doSeq : command

/-! ## Skip and Expected Failure Syntax -/

/-- Syntax for a skipped test: `test "description" (skip := "reason") := do body` -/
syntax (name := testDeclSkip) "test " str "(" "skip" ":=" str ")" " := " doSeq : command

/-- Syntax for an expected failure test: `test "description" (xfail := "reason") := do body` -/
syntax (name := testDeclXfail) "test " str "(" "xfail" ":=" str ")" " := " doSeq : command

/-- Syntax for a skipped test with boolean: `test "description" (skip) := do body` -/
syntax (name := testDeclSkipBool) "test " str "(" "skip" ")" " := " doSeq : command

/-- Syntax for an expected failure test with boolean: `test "description" (xfail) := do body` -/
syntax (name := testDeclXfailBool) "test " str "(" "xfail" ")" " := " doSeq : command

/-! ## Fixture Hook Syntax -/

/-- Syntax for defining a beforeAll hook: `beforeAll := do body` -/
syntax (name := beforeAllDecl) "beforeAll" " := " doSeq : command

/-- Syntax for defining an afterAll hook: `afterAll := do body` -/
syntax (name := afterAllDecl) "afterAll" " := " doSeq : command

/-- Syntax for defining a beforeEach hook: `beforeEach := do body` -/
syntax (name := beforeEachDecl) "beforeEach" " := " doSeq : command

/-- Syntax for defining an afterEach hook: `afterEach := do body` -/
syntax (name := afterEachDecl) "afterEach" " := " doSeq : command

@[command_elab beforeAllDecl]
def elabBeforeAll : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: body :: _ =>
      let ref ← getRef
      let defId := mkIdentFrom ref `beforeAll (canonical := true)
      let cmd ← `(command| def $defId : IO Unit := do $(⟨body⟩):doSeq)
      elabCommand cmd
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab afterAllDecl]
def elabAfterAll : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: body :: _ =>
      let ref ← getRef
      let defId := mkIdentFrom ref `afterAll (canonical := true)
      let cmd ← `(command| def $defId : IO Unit := do $(⟨body⟩):doSeq)
      elabCommand cmd
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab beforeEachDecl]
def elabBeforeEach : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: body :: _ =>
      let ref ← getRef
      let defId := mkIdentFrom ref `beforeEach (canonical := true)
      let cmd ← `(command| def $defId : IO Unit := do $(⟨body⟩):doSeq)
      elabCommand cmd
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab afterEachDecl]
def elabAfterEach : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: _ :: body :: _ =>
      let ref ← getRef
      let defId := mkIdentFrom ref `afterEach (canonical := true)
      let cmd ← `(command| def $defId : IO Unit := do $(⟨body⟩):doSeq)
      elabCommand cmd
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-! ## Helper Functions -/

/-- Convert a test description string into a valid Lean identifier.
    Replaces non-alphanumeric characters with underscores. -/
private def sanitizeName (s : String) : String :=
  let chars := s.toList.map fun c =>
    if c.isAlphanum then c
    else '_'
  -- Remove leading/trailing underscores and collapse multiple underscores
  let result := String.ofList chars
  result.splitOn "_" |>.filter (· ≠ "") |>.intersperse "_" |> String.join

/-- Generate a unique test definition name from a description.
    Emits a warning if a duplicate test name is detected. -/
private def mkTestName (desc : String) (ns : Name) : CommandElabM Name := do
  let base := sanitizeName desc
  let baseName := Name.mkSimple s!"test_{base}"
  -- Check if name already exists and add suffix if needed
  let env ← getEnv
  let fullName := ns ++ baseName
  if env.contains fullName then
    -- Emit a warning about the duplicate name
    logWarning s!"Duplicate test name \"{desc}\" detected. Consider using unique test descriptions for clarity. The generated identifier will include a numeric suffix."
    -- Add a counter suffix
    let mut counter := 2
    let mut candidateName := ns ++ Name.mkSimple s!"test_{base}_{counter}"
    while env.contains candidateName do
      counter := counter + 1
      candidateName := ns ++ Name.mkSimple s!"test_{base}_{counter}"
    return candidateName.componentsRev.head!
  else
    return baseName

/-! ## Test Elaborator -/

private def elabTestCore (desc : TSyntax `str) (body : TSyntax `Lean.Parser.Term.doSeq)
    (timeoutOpt : Option (TSyntax `term)) (retryOpt : Option (TSyntax `term))
    (skipReason : Option String) (xfailReason : Option String) : CommandElabM Unit := do
  let descStr := desc.getString
  let ns ← getCurrNamespace
  let defName ← mkTestName descStr ns
  let defId := mkIdent defName

  -- Generate TestCase based on which options are set
  let cmd ← match timeoutOpt, retryOpt, skipReason, xfailReason with
    | some t, some r, none, none =>
      `(command|
        def $defId : TestCase := {
          name := $desc
          run := do $body
          timeoutMs := some $t
          retryCount := some $r
        }
      )
    | some t, none, none, none =>
      `(command|
        def $defId : TestCase := {
          name := $desc
          run := do $body
          timeoutMs := some $t
        }
      )
    | none, some r, none, none =>
      `(command|
        def $defId : TestCase := {
          name := $desc
          run := do $body
          retryCount := some $r
        }
      )
    | none, none, some skipStr, none =>
      let skipLit : TSyntax `str := ⟨Syntax.mkStrLit skipStr⟩
      `(command|
        def $defId : TestCase := {
          name := $desc
          run := do $body
          «skip» := some (SkipReason.unconditional $skipLit)
        }
      )
    | none, none, none, some xfailStr =>
      let xfailLit : TSyntax `str := ⟨Syntax.mkStrLit xfailStr⟩
      `(command|
        def $defId : TestCase := {
          name := $desc
          run := do $body
          «xfail» := true
          xfailReason := some $xfailLit
        }
      )
    | _, _, _, _ =>
      -- Default case: no special options
      `(command|
        def $defId : TestCase := {
          name := $desc
          run := do $body
        }
      )
  elabCommand cmd

  -- Register the full name in the environment extension
  let fullName := ns ++ defName
  modifyEnv fun env => testCaseExtension.addEntry env fullName

@[command_elab testDecl]
def elabTest : CommandElab := fun stx => do
  match stx with
  | `(command| test $desc:str := $body:doSeq) =>
    elabTestCore desc body none none none none
  | _ => throwUnsupportedSyntax

@[command_elab testDeclTimeout]
def elabTestTimeout : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: timeoutStx :: _ :: _ :: body :: _ =>
      elabTestCore ⟨desc⟩ ⟨body⟩ (some ⟨timeoutStx⟩) none none none
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab testDeclRetry]
def elabTestRetry : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: retryStx :: _ :: _ :: body :: _ =>
      elabTestCore ⟨desc⟩ ⟨body⟩ none (some ⟨retryStx⟩) none none
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab testDeclTimeoutRetry]
def elabTestTimeoutRetry : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: timeoutStx :: _ :: _ :: _ :: _ :: _ :: retryStx :: _ :: _ :: body :: _ =>
      elabTestCore ⟨desc⟩ ⟨body⟩ (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) none none
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab testDeclRetryTimeout]
def elabTestRetryTimeout : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: retryStx :: _ :: _ :: _ :: _ :: _ :: timeoutStx :: _ :: _ :: body :: _ =>
      elabTestCore ⟨desc⟩ ⟨body⟩ (some ⟨timeoutStx⟩) (some ⟨retryStx⟩) none none
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-! ## Skip/Xfail Elaborators -/

@[command_elab testDeclSkip]
def elabTestSkip : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: reasonStx :: _ :: _ :: body :: _ =>
      let reason := (⟨reasonStx⟩ : TSyntax `str).getString
      elabTestCore ⟨desc⟩ ⟨body⟩ none none (some reason) none
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab testDeclXfail]
def elabTestXfail : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: reasonStx :: _ :: _ :: body :: _ =>
      let reason := (⟨reasonStx⟩ : TSyntax `str).getString
      elabTestCore ⟨desc⟩ ⟨body⟩ none none none (some reason)
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab testDeclSkipBool]
def elabTestSkipBool : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: _ :: body :: _ =>
      elabTestCore ⟨desc⟩ ⟨body⟩ none none (some "skipped") none
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab testDeclXfailBool]
def elabTestXfailBool : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: _ :: body :: _ =>
      elabTestCore ⟨desc⟩ ⟨body⟩ none none none (some "expected failure")
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

end Crucible.Macros
