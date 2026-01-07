import Crucible
import Tests.Fixtures

open Crucible

def main (args : List String) : IO UInt32 := do
  let results ← runAllSuitesFiltered args
  return results.toExitCode
