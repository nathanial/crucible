import Lean
import Crucible.Core
import Crucible.Macros
import Crucible.Property.Core

/-!
# Property Test Syntax

Provides the `proptest` command for defining property-based tests
that integrate with Crucible's test runner.
-/

namespace Crucible.Property

open Lean Elab Command Meta
open Crucible
open Crucible.SuiteRegistry (testCaseExtension)

/-! ## Proptest Syntax -/

/-- Syntax for defining a property test: `proptest "description" := property` -/
syntax (name := proptestDecl) "proptest " str " := " term : command

/-- Syntax with test count: `proptest "description" (tests := 1000) := property` -/
syntax (name := proptestDeclTests)
  "proptest " str "(" "tests" ":=" term ")" " := " term : command

/-- Syntax with seed: `proptest "description" (seed := 42) := property` -/
syntax (name := proptestDeclSeed)
  "proptest " str "(" "seed" ":=" term ")" " := " term : command


/-! ## Elaboration Helpers -/

/-- Sanitize a test description to create a valid Lean identifier. -/
private def sanitizeName (s : String) : String :=
  let chars := s.toList.map fun c =>
    if c.isAlphanum then c
    else '_'
  let result := String.ofList chars
  result.splitOn "_" |>.filter (· ≠ "") |>.intersperse "_" |> String.join

/-- Generate a unique test definition name from a description. -/
private def mkPropTestName (desc : String) (ns : Name) : CommandElabM Name := do
  let base := sanitizeName desc
  let baseName := Name.mkSimple s!"proptest_{base}"
  let env ← getEnv
  let fullName := ns ++ baseName
  if env.contains fullName then
    logWarning s!"Duplicate proptest name \"{desc}\" detected."
    let mut counter := 2
    let mut candidateName := ns ++ Name.mkSimple s!"proptest_{base}_{counter}"
    while env.contains candidateName do
      counter := counter + 1
      candidateName := ns ++ Name.mkSimple s!"proptest_{base}_{counter}"
    return candidateName.componentsRev.head!
  else
    return baseName


/-! ## Core Elaborator -/

private def elabProptestCore (desc : TSyntax `str) (propTerm : TSyntax `term)
    (testsOpt : Option (TSyntax `term)) (seedOpt : Option (TSyntax `term)) : CommandElabM Unit := do
  let descStr := desc.getString
  let ns ← getCurrNamespace
  let defName ← mkPropTestName descStr ns
  let defId := mkIdent defName

  -- Build PropConfig based on options
  let configExpr : TSyntax `term ← match testsOpt, seedOpt with
    | some numTests, some seedVal =>
      `({ numTests := $numTests, «seed» := some $seedVal : PropConfig })
    | some numTests, none =>
      `({ numTests := $numTests : PropConfig })
    | none, some seedVal =>
      `({ «seed» := some $seedVal : PropConfig })
    | none, none =>
      `(({} : PropConfig))

  -- Generate TestCase definition
  let cmd ← `(command|
    def $defId : TestCase := {
      name := $desc
      run := Property.runOrFail $propTerm $configExpr
    }
  )
  elabCommand cmd

  -- Register the full name in the environment extension
  let fullName := ns ++ defName
  modifyEnv fun env => testCaseExtension.addEntry env fullName


/-! ## Elaborators -/

@[command_elab proptestDecl]
def elabProptest : CommandElab := fun stx => do
  -- proptest "desc" := prop
  let desc := stx[1]
  let prop := stx[3]
  elabProptestCore ⟨desc⟩ ⟨prop⟩ none none

@[command_elab proptestDeclTests]
def elabProptestTests : CommandElab := fun stx => do
  -- proptest "desc" ( tests := term ) := prop
  -- indices: 0=proptest, 1=str, 2=(, 3=tests, 4=:=, 5=term, 6=), 7=:=, 8=term
  let desc := stx[1]
  let numTests := stx[5]
  let prop := stx[8]
  elabProptestCore ⟨desc⟩ ⟨prop⟩ (some ⟨numTests⟩) none

@[command_elab proptestDeclSeed]
def elabProptestSeed : CommandElab := fun stx => do
  -- proptest "desc" ( seed := term ) := prop
  -- indices: 0=proptest, 1=str, 2=(, 3=seed, 4=:=, 5=term, 6=), 7=:=, 8=term
  let desc := stx[1]
  let seedVal := stx[5]
  let prop := stx[8]
  elabProptestCore ⟨desc⟩ ⟨prop⟩ none (some ⟨seedVal⟩)

end Crucible.Property
