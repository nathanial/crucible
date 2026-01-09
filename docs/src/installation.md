# Installation

Add Crucible to your Lean 4 project by adding it as a dependency in your `lakefile.lean`.

## Adding the Dependency

Add the following to your `lakefile.lean`:

```lean
require crucible from git "https://github.com/nathanial/crucible" @ "v0.1.0"
```

Or track the latest changes (not recommended for production):

```lean
require crucible from git "https://github.com/nathanial/crucible" @ "master"
```

## Updating Dependencies

After adding the dependency, run:

```bash
lake update
lake build
```

## Project Structure

A typical project structure with Crucible tests:

```
my-project/
├── lakefile.lean
├── lean-toolchain
├── MyProject/
│   └── ...
└── Tests/
    ├── Main.lean      # Test runner entry point
    └── MyTests.lean   # Your test suites
```

## Test Runner Setup

Create a test runner in `Tests/Main.lean`:

```lean
import Crucible
import Tests.MyTests  -- Import your test modules

open Crucible

def main : IO UInt32 := do
  runAllSuites
```

## Lakefile Configuration

Add a test executable to your `lakefile.lean`:

```lean
lean_lib Tests where
  globs := #[.andSubmodules `Tests]

@[test_driver]
lean_exe test where
  root := `Tests.Main
```

The `@[test_driver]` attribute allows running tests with `lake test`.

## Running Tests

Once configured, run your tests with:

```bash
lake test
```

Or build and run directly:

```bash
lake build test
lake exe test
```

## Next Steps

Now that Crucible is installed, head to the [Quick Start](./quick-start.md) guide to write your first test suite.
