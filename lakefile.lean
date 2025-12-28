import Lake
open Lake DSL

package crucible where
  version := v!"0.1.0"

@[default_target]
lean_lib Crucible where
  globs := #[.andSubmodules `Crucible]

lean_lib Tests where
  globs := #[.andSubmodules `Tests]

lean_exe crucible_tests where
  root := `Tests.Main

@[test_driver]
lean_exe test where
  root := `Tests.Main
