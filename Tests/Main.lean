import Crucible
import Tests.Fixtures

open Crucible

def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
