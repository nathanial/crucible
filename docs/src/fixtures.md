# Fixtures

Fixtures provide setup and teardown hooks for your test suites.

## Overview

Crucible supports four fixture hooks:

| Hook | When it runs |
|------|--------------|
| `beforeAll` | Once before all tests in the suite |
| `afterAll` | Once after all tests (even if tests fail) |
| `beforeEach` | Before each individual test |
| `afterEach` | After each individual test (even if test fails) |

## Defining Fixtures

Define fixtures as `IO Unit` functions in your test namespace:

```lean
namespace DatabaseTests
open Crucible

testSuite "Database"

def beforeAll : IO Unit := do
  IO.println "Setting up database connection..."
  Database.connect

def afterAll : IO Unit := do
  IO.println "Closing database connection..."
  Database.disconnect

def beforeEach : IO Unit := do
  Database.beginTransaction

def afterEach : IO Unit := do
  Database.rollback

test "can insert record" := do
  let id ← Database.insert { name := "Alice" }
  id > 0 |> ensure "should return valid id"

test "can query records" := do
  let records ← Database.query "SELECT * FROM users"
  records.length ≡ 0  -- Rolled back each time

#generate_tests

end DatabaseTests
```

## Fixture Execution Order

For a suite with two tests, the execution order is:

```
beforeAll
  beforeEach
    test 1
  afterEach
  beforeEach
    test 2
  afterEach
afterAll
```

## Error Handling

### beforeAll Failure

If `beforeAll` fails, all tests in the suite are marked as failed:

```lean
def beforeAll : IO Unit := do
  if !configExists then
    throw (IO.userError "Configuration not found")
```

### afterAll Always Runs

`afterAll` runs even if tests fail, ensuring cleanup happens:

```lean
def afterAll : IO Unit := do
  -- Always close connections, delete temp files, etc.
  tempFile.delete
```

### beforeEach/afterEach Failure

If `beforeEach` fails, the test fails. `afterEach` runs even if the test fails.

## Common Patterns

### Temporary Files

```lean
namespace FileTests
open Crucible

testSuite "File Operations"

def tempPath : System.FilePath := "/tmp/test-file.txt"

def beforeEach : IO Unit := do
  IO.FS.writeFile tempPath "test content"

def afterEach : IO Unit := do
  if ← tempPath.pathExists then
    IO.FS.removeFile tempPath

test "can read file" := do
  let content ← IO.FS.readFile tempPath
  content ≡ "test content"

#generate_tests

end FileTests
```

### Database Transactions

```lean
def beforeEach : IO Unit := do
  Database.beginTransaction

def afterEach : IO Unit := do
  Database.rollback  -- Ensures test isolation
```

### Environment Setup

```lean
def beforeAll : IO Unit := do
  -- Set up test environment
  IO.setEnv "TEST_MODE" "true"
  TestServer.start

def afterAll : IO Unit := do
  TestServer.stop
```

### Shared State with IO.Ref

```lean
namespace CounterTests
open Crucible

testSuite "Counter"

def counter : IO (IO.Ref Nat) := IO.mkRef 0

def beforeAll : IO Unit := do
  let c ← counter
  c.set 0

def beforeEach : IO Unit := do
  let c ← counter
  c.modify (· + 1)

test "first test" := do
  let c ← counter
  let n ← c.get
  n ≡ 1

test "second test" := do
  let c ← counter
  let n ← c.get
  n ≡ 2

#generate_tests

end CounterTests
```

## Fixtures with Filtering

When using [CLI filtering](./cli.md), fixtures still run for filtered tests:

```bash
lake test -- --test "can insert"
```

This runs:
1. `beforeAll`
2. `beforeEach`
3. "can insert record" test
4. `afterEach`
5. `afterAll`

## Tips

1. **Keep fixtures simple**: Complex setup can mask test failures
2. **Use beforeEach for isolation**: Ensure tests don't affect each other
3. **Always clean up in afterAll/afterEach**: Don't leave resources dangling
4. **Log setup steps**: Add `IO.println` for debugging fixture issues
5. **Consider test order**: Tests within a suite run in definition order
