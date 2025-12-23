import Lean

/-!
# Test Suite Registry

Provides auto-discovery of test suites across modules using the `testSuite` command.

## Usage

In each test module:
```lean
import Crucible

namespace MyTests

testSuite "My Test Suite"

test "test 1" := do ...
test "test 2" := do ...

#generate_tests

end MyTests
```

In Main.lean, use `getAllSuites` to get all registered suites.
-/

namespace Crucible.SuiteRegistry

open Lean Elab Command

/-! ## Suite Info Structure -/

/-- Information about a registered test suite. -/
structure SuiteInfo where
  /-- Human-readable name for the suite (shown in test output) -/
  suiteName : String
  /-- The namespace containing the tests -/
  ns : Name
  deriving Inhabited, BEq

instance : ToString SuiteInfo where
  toString s := s!"{s.suiteName} ({s.ns})"

/-! ## Environment Extension for Suite Collection -/

/-- Environment extension that collects test suite registrations. -/
initialize suiteExtension : SimplePersistentEnvExtension SuiteInfo (Array SuiteInfo) ←
  registerSimplePersistentEnvExtension {
    name := `crucibleTestSuiteRegistry
    addImportedFn := fun arrays => arrays.foldl Array.append #[]
    addEntryFn := Array.push
  }

/-- Get all registered test suites from the environment. -/
def getAllSuites (env : Environment) : Array SuiteInfo :=
  suiteExtension.getState env

/-- The name of the `cases` definition associated with a suite's namespace. -/
def suiteCasesName (suite : SuiteInfo) : Name :=
  suite.ns ++ `cases

/-- Iterate over all registered suites in the environment. -/
def forAllSuites [Monad m] (env : Environment) (f : SuiteInfo → m Unit) : m Unit := do
  for suite in getAllSuites env do
    f suite

/-! ## Test Suite Syntax -/

/-- Syntax for registering a test suite: `testSuite "Suite Name"` -/
syntax (name := testSuiteCmd) "testSuite " str : command

@[command_elab testSuiteCmd]
def elabTestSuite : CommandElab := fun stx => do
  match stx with
  | `(testSuite $name:str) =>
    let suiteName := name.getString
    let currNs ← getCurrNamespace
    let info : SuiteInfo := { suiteName, ns := currNs }

    -- Only register the first suite per namespace; subsequent calls are for grouping
    let env ← getEnv
    let existing := getAllSuites env
    unless existing.any (fun s => s.ns == currNs) do
      modifyEnv fun env => suiteExtension.addEntry env info

  | _ => throwUnsupportedSyntax

end Crucible.SuiteRegistry
