import Lean

/-!
# Test Suite Registry

Enables automatic discovery of test suites across modules.

## How Suite Discovery Works

1. Each test file calls `testSuite "Suite Name"` to register itself
2. The suite info (name + namespace) is stored in an environment extension
3. Each `test` macro registers the test case name in `testCaseExtension`
4. At runtime, `runAllSuites` iterates over all registered suites
5. For each suite, it queries `testCaseExtension` for tests in that namespace

## Required Structure

Each test module must follow this pattern:

```lean
import Crucible

namespace MyProject.FeatureTests  -- Namespace is required

open Crucible

testSuite "Feature Tests"  -- Registers this namespace as a suite

test "test 1" := do
  1 + 1 ≡ 2

test "test 2" := do
  "hello".length ≡ 5

end MyProject.FeatureTests
```

## Multiple Suites Per File

You can define multiple suites in one file using separate namespaces:

```lean
namespace Tests.Unit
testSuite "Unit Tests"
test "..." := do ...
end Tests.Unit

namespace Tests.Integration
testSuite "Integration Tests"
test "..." := do ...
end Tests.Integration
```

## Key Functions

- `getAllSuites env` - Get all registered `SuiteInfo` from environment
- `getTestsForSuite env suiteNs` - Get test case names for a specific suite
- `getRegisteredTests env` - Get all registered test case names
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

/-- The name of the `cases` definition associated with a suite's namespace.
    **Deprecated**: Test discovery is now automatic via `testCaseExtension`. -/
def suiteCasesName (suite : SuiteInfo) : Name :=
  suite.ns ++ `cases

/-! ## Environment Extension for Test Case Collection -/

/-- Environment extension that collects test case definition names within a module.
    This enables automatic test discovery without needing `#generate_tests`. -/
initialize testCaseExtension : SimplePersistentEnvExtension Name (Array Name) ←
  registerSimplePersistentEnvExtension {
    name := `crucibleTestCaseRegistry
    addImportedFn := fun arrays => arrays.foldl Array.append #[]
    addEntryFn := Array.push
  }

/-- Get all registered test case names from the environment. -/
def getRegisteredTests (env : Environment) : Array Name :=
  testCaseExtension.getState env

/-- Get test cases for a specific suite namespace.
    Returns all test names where the test is defined directly in the namespace
    or in a sub-namespace of the suite. -/
def getTestsForSuite (env : Environment) (suiteNs : Name) : Array Name :=
  (getRegisteredTests env).filter fun name =>
    name.getPrefix == suiteNs || suiteNs.isPrefixOf name

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
