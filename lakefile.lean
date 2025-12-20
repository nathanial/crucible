import Lake
open Lake DSL

package crucible where
  version := v!"0.1.0"

@[default_target]
lean_lib Crucible where
  globs := #[.andSubmodules `Crucible]
