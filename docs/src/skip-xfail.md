# Skipping & Expected Failures

Mark tests as skipped or expected to fail.

## Skipping Tests

### Basic Skip

Skip a test unconditionally:

```lean
test "work in progress" (skip) := do
  -- This test won't run
  unimplementedFeature
```

### Skip with Reason

Provide a reason for skipping:

```lean
test "requires GPU" (skip := "GPU not available in CI") := do
  runGpuComputation
```

The reason appears in test output:

```
[1/3]  requires GPU... ○ (skipped: GPU not available in CI)
```

### Conditional Skipping

Skip based on conditions (at definition time):

```lean
test "linux only" (skip := if System.Platform.isWindows then "windows" else "") := do
  linuxSpecificTest
```

## Expected Failures (xfail)

### Basic xfail

Mark a test as expected to fail:

```lean
test "known bug" (xfail := "issue #123") := do
  buggyFunction ≡ expected  -- This will fail, as expected
```

### xfail Outcomes

| Test Result | xfail Outcome | Meaning |
|-------------|---------------|---------|
| Test fails | XFAIL (good) | Expected behavior |
| Test passes | XPASS (bad) | Bug was fixed, remove xfail! |

### xfail Output

```
[1/2]  known bug... ⊘ (xfail: issue #123) (2ms)
[2/2]  accidentally fixed... ⊕ (XPASS - expected to fail: should fail) (1ms)
```

- `⊘` (XFAIL): Test failed as expected - counts as pass
- `⊕` (XPASS): Test passed unexpectedly - counts as failure!

### Why XPASS is a Failure

An XPASS indicates the test was marked as expected to fail, but it passed. This means either:
1. The bug was fixed - remove the `xfail` marker
2. The test is wrong - fix the test

Either way, action is needed.

## Use Cases

### Skipping

#### Platform-Specific Tests

```lean
test "windows registry" (skip := "not on macOS") := do
  readWindowsRegistry key
```

#### Missing Dependencies

```lean
test "database integration" (skip := "requires PostgreSQL") := do
  connectToPostgres
```

#### Incomplete Features

```lean
test "new feature" (skip := "not implemented yet") := do
  newFeature ≡ expected
```

### Expected Failures

#### Known Bugs

```lean
test "parser edge case" (xfail := "bug #456 - crashes on empty input") := do
  parse "" ≡ emptyResult
```

#### Unimplemented Features

```lean
test "async support" (xfail := "async not implemented") := do
  asyncOperation.await
```

#### Documenting Incorrect Behavior

```lean
test "should return error" (xfail := "currently returns wrong value") := do
  brokenFunction ≡ correctResult
```

## Results Summary

Skip and xfail tests are tracked separately:

```
────────────────────────────────────────
Summary: 5 passed, 0 failed, 2 skipped, 1 xfailed (100.0%)
         1 suites, 8 tests run
         Completed in 0.05s
────────────────────────────────────────
```

- **skipped**: Tests that didn't run
- **xfailed**: Expected failures that failed (good)
- **xpassed**: Would appear if an xfail test passed (bad)

## Best Practices

### Skip

1. **Always provide a reason**: Helps future maintainers understand why
2. **Use for external dependencies**: Platform, hardware, network requirements
3. **Remove when addressed**: Don't let skipped tests accumulate
4. **Consider conditional logic**: Skip at runtime only when needed

### xfail

1. **Link to issues**: Reference bug numbers or tickets
2. **Check regularly**: Remove xfail when bugs are fixed
3. **Don't abuse**: Don't use xfail to hide real test failures
4. **Pay attention to XPASS**: An XPASS is a signal to update tests

### Choosing Between Skip and xfail

| Use Skip | Use xfail |
|----------|-----------|
| Test can't run (missing dependencies) | Test runs but fails due to known bug |
| Platform-specific tests | Documenting expected behavior |
| Work in progress | Tracking known issues |
| Resource not available | Bug being worked on |

## Filtering

Skipped and xfail tests are included in filter matches:

```bash
lake test -- --test "known bug"
```

If "known bug" matches, it will show as skipped or xfailed appropriately.
