# Test Suites

Test suites group related tests together and provide organization for your test output.

## Declaring a Suite

Use `testSuite` to declare a named suite:

```lean
namespace MyTests
open Crucible

testSuite "HTTP Client"

test "sends GET request" := do ...
test "sends POST request" := do ...

#generate_tests

end MyTests
```

## Suite Names

Suite names appear in test output and can be used for filtering:

```
HTTP Client
───────────
[1/2]  sends GET request... ✓ (5ms)
[2/2]  sends POST request... ✓ (3ms)
```

Choose descriptive names that indicate the component being tested.

## Multiple Suites

Define multiple suites in different namespaces:

```lean
namespace Unit.Parser
open Crucible
testSuite "Parser Unit Tests"
-- tests...
#generate_tests
end Unit.Parser

namespace Integration.API
open Crucible
testSuite "API Integration Tests"
-- tests...
#generate_tests
end Integration.API
```

## Suite Organization Patterns

### By Component

```lean
namespace Auth.Tests
testSuite "Authentication"
end Auth.Tests

namespace Database.Tests
testSuite "Database"
end Database.Tests
```

### By Test Type

```lean
namespace Unit
testSuite "Unit Tests"
end Unit

namespace Integration
testSuite "Integration Tests"
end Integration
```

### By Feature

```lean
namespace Feature.UserManagement
testSuite "User Management"
end Feature.UserManagement

namespace Feature.Billing
testSuite "Billing"
end Feature.Billing
```

## Running Specific Suites

Use CLI filtering to run specific suites:

```bash
# Run suites containing "Parser"
lake test -- --suite Parser

# Run suites matching exactly
lake test -- --suite "HTTP Client" --exact
```

See [CLI Reference](./cli.md) for more filtering options.

## Suite-Level Configuration

Configure defaults for all tests in a suite:

```lean
def main : IO UInt32 := do
  runAllSuites (timeout := 5000)  -- 5 second default
```

Individual tests can override these defaults:

```lean
test "slow operation" (timeout := 10000) := do
  -- This test has a 10 second timeout
```

## Fixtures

Suites can have setup/teardown hooks. See [Fixtures](./fixtures.md) for details.

```lean
namespace DatabaseTests
open Crucible

testSuite "Database"

def beforeAll : IO Unit := do
  -- Run once before all tests
  Database.connect

def afterAll : IO Unit := do
  -- Run once after all tests
  Database.disconnect

test "can query" := do ...

#generate_tests

end DatabaseTests
```

## Test Discovery

The `runAllSuites` macro automatically discovers all registered suites:

```lean
def main : IO UInt32 := do
  runAllSuites  -- Finds and runs all suites
```

Suites are registered when:
1. `testSuite "name"` is called in a namespace
2. `#generate_tests` is called to collect tests

## Importing Test Modules

Ensure all test modules are imported in your test runner:

```lean
import Crucible
import Tests.ParserTests
import Tests.DatabaseTests
import Tests.APITests

def main : IO UInt32 := do
  runAllSuites
```

If a suite isn't running, check that its module is imported.
