# Fixtures

Tests often need shared setup: a database connection, a temporary file, an HTTP server, or some initial state. Rather than duplicating this setup in every test, fixtures let you define it once and have Crucible run it at the right time.

Fixtures also handle teardown—cleaning up resources after tests complete. This is particularly important because tests can fail unexpectedly, and you need cleanup to happen regardless of whether the test passed. Without proper teardown, you end up with orphaned database connections, lingering temp files, or other resource leaks that make subsequent test runs unpredictable.

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

Several patterns appear repeatedly when working with fixtures. Understanding when to use each one helps you structure your test suites effectively.

### Temporary Files

File-based tests face a common challenge: you need a file to exist before the test runs, but you don't want leftover files polluting your filesystem or interfering with other tests. The solution is to create the file in `beforeEach` and delete it in `afterEach`, giving each test a clean slate.

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

Notice that `afterEach` checks whether the file exists before deleting it. This guards against tests that delete the file themselves—without this check, the cleanup would throw an error on a non-existent file.

### Database Transactions

When testing database code, you typically want tests to be isolated from each other—changes made in one test shouldn't affect another. The transaction-rollback pattern achieves this elegantly: start a transaction before each test, then roll it back afterward. The test can insert, update, and delete freely, knowing everything will be undone.

```lean
def beforeEach : IO Unit := do
  Database.beginTransaction

def afterEach : IO Unit := do
  Database.rollback  -- Ensures test isolation
```

This pattern is fast because rollback is cheaper than recreating the database, and it's safe because even failed tests get cleaned up properly.

### Environment Setup

Some tests require infrastructure that's expensive to create—a test server, a connection pool, or cached configuration. For these, use `beforeAll` to set up once before any tests run, then `afterAll` to tear down after all tests complete.

```lean
def beforeAll : IO Unit := do
  -- Set up test environment
  IO.setEnv "TEST_MODE" "true"
  TestServer.start

def afterAll : IO Unit := do
  TestServer.stop
```

The tradeoff here is that tests share this infrastructure, so they need to be careful about not interfering with each other. Use `beforeAll`/`afterAll` for genuinely expensive setup, and prefer `beforeEach`/`afterEach` when isolation matters more than performance.

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

Fixtures should be simple and focused. When a fixture grows complex—doing database migrations, starting multiple services, configuring intricate state—it becomes a source of failure itself. If your tests fail because the fixture failed, you spend time debugging infrastructure instead of the code you're actually testing. Keep fixtures minimal, doing just enough to let tests run.

Test isolation is worth prioritizing. When tests share state without proper cleanup, they become order-dependent: test A passes alone but fails when run after test B. These intermittent failures are notoriously hard to debug. Using `beforeEach` to reset state before each test prevents this class of bug entirely.

Resource cleanup belongs in teardown hooks, not in tests. A test might fail before reaching its cleanup code, but `afterEach` and `afterAll` run regardless of test outcome. This guarantees that connections close, files delete, and servers stop—even when things go wrong.

When fixtures misbehave, add logging. A well-placed `IO.println "Connecting to database..."` in your `beforeAll` tells you whether setup even ran. This is especially helpful in CI environments where you can't attach a debugger.

Finally, remember that tests run in definition order within a suite. While you shouldn't rely on this order—tests should be independent—knowing it exists helps when you're reading test output or debugging a sequence of failures.
