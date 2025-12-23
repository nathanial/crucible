import Lean
import Crucible.Core

/-!
# Test Framework Macros

Provides the `test` macro for defining tests with minimal boilerplate,
and `#generate_tests` for auto-collecting tests into a `cases` list.

## Usage

```lean
import Crucible

test "My test description" := do
  someValue ≡ expectedValue
  IO.println "Test passed"

test "Another test" := do
  anotherCheck

#generate_tests  -- Creates: def cases : List TestCase := [...]
```
-/

namespace Crucible.Macros

open Lean Elab Command Meta

/-! ## Environment Extension for Test Collection -/

/-- Environment extension that collects test case definition names within a module. -/
initialize testCaseExtension : SimplePersistentEnvExtension Name (Array Name) ←
  registerSimplePersistentEnvExtension {
    name := `crucibleTestCaseRegistry
    addImportedFn := fun arrays => arrays.foldl Array.append #[]
    addEntryFn := Array.push
  }

/-- Get all registered test case names from the environment. -/
def getRegisteredTests (env : Environment) : Array Name :=
  testCaseExtension.getState env

/-! ## Test Syntax -/

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

/-- Generate a unique test definition name from a description. -/
private def mkTestName (desc : String) (ns : Name) : CommandElabM Name := do
  let base := sanitizeName desc
  let baseName := Name.mkSimple s!"test_{base}"
  -- Check if name already exists and add suffix if needed
  let env ← getEnv
  let fullName := ns ++ baseName
  if env.contains fullName then
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
    (timeoutOpt : Option (TSyntax `term)) (retryOpt : Option (TSyntax `term)) : CommandElabM Unit := do
  let descStr := desc.getString
  let ns ← getCurrNamespace
  let defName ← mkTestName descStr ns
  let defId := mkIdent defName

  -- Generate the TestCase definition
  let cmd ←
    match timeoutOpt, retryOpt with
    | some timeoutVal, some retryVal =>
      `(command|
        private def $defId : TestCase := {
          name := $desc
          run := do $body
          timeoutMs := some $timeoutVal:term
          retryCount := some $retryVal:term
        }
      )
    | some timeoutVal, none =>
      `(command|
        private def $defId : TestCase := {
          name := $desc
          run := do $body
          timeoutMs := some $timeoutVal:term
        }
      )
    | none, some retryVal =>
      `(command|
        private def $defId : TestCase := {
          name := $desc
          run := do $body
          retryCount := some $retryVal:term
        }
      )
    | none, none =>
      `(command|
        private def $defId : TestCase := {
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
    elabTestCore desc body none none
  | _ => throwUnsupportedSyntax

@[command_elab testDeclTimeout]
def elabTestTimeout : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: timeoutStx :: _ :: _ :: body :: _ =>
      elabTestCore ⟨desc⟩ ⟨body⟩ (some ⟨timeoutStx⟩) none
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab testDeclRetry]
def elabTestRetry : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: retryStx :: _ :: _ :: body :: _ =>
      elabTestCore ⟨desc⟩ ⟨body⟩ none (some ⟨retryStx⟩)
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab testDeclTimeoutRetry]
def elabTestTimeoutRetry : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: timeoutStx :: _ :: _ :: _ :: _ :: _ :: retryStx :: _ :: _ :: body :: _ =>
      elabTestCore ⟨desc⟩ ⟨body⟩ (some ⟨timeoutStx⟩) (some ⟨retryStx⟩)
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

@[command_elab testDeclRetryTimeout]
def elabTestRetryTimeout : CommandElab := fun stx => do
  match stx with
  | Syntax.node _ _ args =>
    match args.toList with
    | _ :: desc :: _ :: _ :: _ :: retryStx :: _ :: _ :: _ :: _ :: _ :: timeoutStx :: _ :: _ :: body :: _ =>
      elabTestCore ⟨desc⟩ ⟨body⟩ (some ⟨timeoutStx⟩) (some ⟨retryStx⟩)
    | _ => throwUnsupportedSyntax
  | _ => throwUnsupportedSyntax

/-! ## Generate Tests Command -/

/-- Syntax for auto-generating the `cases` list: `#generate_tests` -/
syntax (name := generateTestsCmd) "#generate_tests" : command

@[command_elab generateTestsCmd]
def elabGenerateTests : CommandElab := fun _stx => do
  let env ← getEnv
  let ns ← getCurrNamespace
  let allTests := getRegisteredTests env

  -- Filter to tests in the current namespace
  let moduleTests := allTests.filter fun name =>
    -- Check if the test name starts with the current namespace
    name.getPrefix == ns || ns.isPrefixOf name

  -- Use a non-hygienic identifier so `cases` is accessible from outside
  let ref ← getRef
  let casesId := mkIdentFrom ref `cases (canonical := true)
  let testCaseId := mkIdentFrom ref `TestCase (canonical := true)

  if moduleTests.isEmpty then
    logWarning s!"No tests found in namespace {ns}. Make sure to define tests before #generate_tests."
    -- Generate empty cases list
    let cmd ← `(command| def $casesId : List $testCaseId := [])
    elabCommand cmd
  else
    -- Generate the cases definition with all test names
    let nameIds : Array (TSyntax `term) := moduleTests.map fun n =>
      -- Use the simple name if in current namespace, otherwise full path
      let ident := if n.getPrefix == ns then
        mkIdentFrom ref n.componentsRev.head! (canonical := true)
      else
        mkIdentFrom ref n (canonical := true)
      ident
    let cmd ← `(command| def $casesId : List $testCaseId := [$[$nameIds],*])
    elabCommand cmd

end Crucible.Macros
