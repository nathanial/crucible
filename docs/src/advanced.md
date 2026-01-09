# Advanced Features

Crucible provides several advanced features for more sophisticated testing needs.

## Overview

This section covers:

- [Timeouts & Retries](./timeouts-retries.md) - Handle slow or flaky tests
- [Skipping & Expected Failures](./skip-xfail.md) - Mark tests as skipped or expected to fail
- [Soft Assertions](./soft-assertions.md) - Collect multiple failures per test
- [Property Testing](./property-testing.md) - QuickCheck-style testing

## When to Use Advanced Features

### Timeouts

Use timeouts for:
- Tests involving network calls
- Tests with potentially infinite loops
- Integration tests that might hang

```lean
test "network request" (timeout := 5000) := do
  let response ← httpGet "https://api.example.com"
  response.status ≡ 200
```

### Retries

Use retries for:
- Flaky tests (while you fix the root cause)
- Tests with race conditions
- Tests depending on external services

```lean
test "eventually consistent" (retry := 3) := do
  let value ← checkEventualValue
  value ≡ expected
```

### Skip

Use skip for:
- Platform-specific tests
- Tests requiring unavailable resources
- Work-in-progress tests

```lean
test "windows only" (skip := "not on windows") := do
  -- Windows-specific test
```

### Expected Failures (xfail)

Use xfail for:
- Known bugs awaiting fixes
- Tests for unimplemented features
- Documenting expected behavior that currently fails

```lean
test "known bug #123" (xfail := "see issue #123") := do
  buggyFunction ≡ expected  -- Expected to fail
```

### Soft Assertions

Use soft assertions when:
- Testing multiple independent properties
- You want to see all failures at once
- Validating complex objects

```lean
test "user validation" := withSoftAsserts fun soft => do
  soft.shouldBe user.name "Alice"
  soft.shouldBe user.age 25
  soft.shouldBe user.email "alice@example.com"
  -- All failures reported together
```

### Property Testing

Use property testing for:
- Testing invariants over many inputs
- Finding edge cases automatically
- Validating mathematical properties

```lean
proptest "addition is commutative" :=
  forAll' fun (a, b) : (Int × Int) =>
    a + b == b + a
```

## Combining Features

Features can be combined:

```lean
test "complex test" (timeout := 10000) (retry := 2) := do
  -- Has both timeout and retry
```

```lean
test "skipped with reason" (skip := "database not available") := do
  -- Won't run, shows skip reason
```

## Best Practices

1. **Use timeouts for external dependencies**: Prevent tests from hanging indefinitely

2. **Retry sparingly**: Fix flaky tests rather than relying on retries

3. **Document xfail reasons**: Link to issues or explain why failure is expected

4. **Prefer hard assertions**: Use soft assertions only when seeing all failures matters

5. **Use property tests for invariants**: They're excellent for finding edge cases

## Next Steps

Explore each feature in detail:

- [Timeouts & Retries](./timeouts-retries.md)
- [Skipping & Expected Failures](./skip-xfail.md)
- [Soft Assertions](./soft-assertions.md)
- [Property Testing](./property-testing.md)
