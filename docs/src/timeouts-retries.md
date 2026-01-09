# Timeouts & Retries

Control test execution timing and handle flaky tests.

## Timeouts

### Per-Test Timeout

Set a timeout in milliseconds:

```lean
test "network call" (timeout := 5000) := do
  let response ← httpGet url
  response.status ≡ 200
```

If the test exceeds the timeout, it fails with a timeout error.

### Suite-Wide Default

Set a default timeout for all tests:

```lean
def main : IO UInt32 := do
  runAllSuites (timeout := 10000)  -- 10 seconds
```

Individual tests can override:

```lean
test "quick test" (timeout := 1000) := do
  -- 1 second timeout (overrides suite default)
```

```lean
test "slow test" (timeout := 30000) := do
  -- 30 second timeout
```

### Timeout Behavior

When a timeout occurs:
1. The test is cancelled
2. A timeout error is reported
3. The next test runs

```
[1/3]  network call... ✗ (5001ms)
    Test timed out after 5000ms
```

## Retries

### Per-Test Retry

Automatically retry failing tests:

```lean
test "flaky operation" (retry := 3) := do
  let result ← unreliableService
  result.success ≡ true
```

This runs the test up to 3 additional times if it fails.

### Suite-Wide Default

Set a default retry count:

```lean
def main : IO UInt32 := do
  runAllSuites (retry := 2)  -- Retry all tests up to 2 times
```

### Retry Behavior

When a test fails with retries:
1. The test is re-run
2. This repeats until success or retry count exhausted
3. Only the final result is reported

If a test passes on retry, it's marked as passed (no indication of retries needed).

## Combining Timeout and Retry

Both can be used together:

```lean
test "unreliable network" (timeout := 5000) (retry := 2) := do
  let response ← fetchData
  response.valid ≡ true
```

Each retry attempt has the same timeout.

### Order Independence

Parameters can be in any order:

```lean
test "option A" (timeout := 5000) (retry := 2) := do ...
test "option B" (retry := 2) (timeout := 5000) := do ...  -- Same effect
```

### Suite-Wide Both

```lean
def main : IO UInt32 := do
  runAllSuites (timeout := 5000) (retry := 1)
```

## Use Cases

### Network Tests

```lean
test "API health check" (timeout := 3000) := do
  let response ← healthCheck apiEndpoint
  response.status ≡ 200
```

### Race Conditions

```lean
test "concurrent access" (retry := 3) := do
  -- May fail due to timing
  let result ← concurrentOperation
  result ≡ expected
```

### Slow Operations

```lean
test "file processing" (timeout := 60000) := do
  -- May take up to 60 seconds
  processLargeFile inputPath
  outputPath.pathExists |> ensure "output should exist"
```

### External Services

```lean
test "database query" (timeout := 10000) (retry := 2) := do
  -- Database might be slow or temporarily unavailable
  let rows ← Database.query "SELECT COUNT(*) FROM users"
  rows.length > 0 |> ensure "should have rows"
```

## Best Practices

### Timeouts

1. **Set reasonable defaults**: Choose a timeout that allows normal execution but catches hangs
2. **Increase for integration tests**: External services need more time
3. **Be generous**: Slow CI environments may need longer timeouts

### Retries

1. **Fix flaky tests**: Retries are a workaround, not a solution
2. **Use sparingly**: Retries slow down test runs
3. **Document why**: Comment why a test needs retries
4. **Track retry usage**: High retry counts indicate problems

```lean
-- TODO: Fix race condition in #456
-- Retry as workaround until fixed
test "flaky concurrent" (retry := 2) := do
  ...
```

## Filtering with Timeouts

When using CLI filtering, timeouts still apply:

```bash
lake test -- --test "network"
```

Each matched test uses its configured timeout.

## Structured Results

When using `runAllSuitesWithResults`, timing information is included:

```lean
def main : IO UInt32 := do
  let results ← runAllSuitesWithResults (timeout := 5000)
  IO.println s!"Total time: {results.totalElapsedMs}ms"
  for suite in results.suites do
    IO.println s!"{suite.name}: {suite.elapsedMs}ms"
  return results.toExitCode
```
