import Crucible
import Tests.Fixtures
import Tests.PropertyTests

open Crucible

def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
