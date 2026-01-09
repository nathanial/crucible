# Writing Tests

This page covers the fundamentals of writing tests with Crucible.

## Test Syntax

The basic test syntax is:

```lean
test "description" := do
  -- test body
```

Tests run in `IO`, so you can use any `IO` operations:

```lean
test "can read environment" := do
  let path ← IO.getEnv "PATH"
  shouldBeSome path "PATH"
```

## Test Names

Test names are strings that describe what the test verifies:

```lean
test "addition of positive numbers" := do
  (1 + 2) ≡ 3

test "empty list has length zero" := do
  [].length ≡ 0
```

Good test names:
- Describe the expected behavior
- Are readable as sentences
- Help identify failures quickly

## Organizing Tests

Group related tests in namespaces with `testSuite`:

```lean
namespace ParserTests
open Crucible

testSuite "JSON Parser"

test "parses integers" := do ...
test "parses strings" := do ...
test "parses arrays" := do ...

#generate_tests

end ParserTests
```

## The `#generate_tests` Command

The `#generate_tests` command collects all tests defined in the current namespace:

```lean
namespace MyTests
open Crucible

testSuite "My Suite"

test "first" := do ...
test "second" := do ...

#generate_tests  -- Collects "first" and "second"

end MyTests
```

This must be called after all tests in the namespace are defined.

## Multiple Assertions Per Test

Tests can contain multiple assertions:

```lean
test "user validation" := do
  let user := createUser "Alice" 25
  user.name ≡ "Alice"
  user.age ≡ 25
  shouldSatisfy (user.age >= 18) "user should be adult"
```

If any assertion fails, the test stops at that point.

## Testing Exceptions

Use `shouldThrow` and related assertions for exception testing:

```lean
test "division by zero throws" := do
  shouldThrow (IO.ofExcept (divide 1 0))

test "invalid input error message" := do
  shouldThrowWith (parseNumber "abc") "invalid"
```

## Helper Functions

Extract common logic into helper functions:

```lean
def createTestUser (name : String) (age : Nat) : IO User := do
  let user := User.mk name age
  user.validate
  pure user

test "valid user creation" := do
  let user ← createTestUser "Bob" 30
  user.name ≡ "Bob"
```

## Conditional Logic in Tests

Tests can use standard Lean control flow:

```lean
test "handles both cases" := do
  let result ← fetchData
  match result with
  | .ok data => data.length > 0 |> ensure "should have data"
  | .error e => throw (IO.userError s!"unexpected error: {e}")
```

## Test Isolation

Each test runs independently. Don't rely on state from other tests:

```lean
-- Bad: relies on shared mutable state
test "first" := do
  globalCounter.set 1

test "second" := do
  -- Don't assume globalCounter is 1!
```

Use [fixtures](./fixtures.md) for shared setup/teardown logic.

## Next Steps

- [Test Suites](./test-suites.md) - Suite organization and naming
- [Assertions](./assertions.md) - All available assertions
- [Fixtures](./fixtures.md) - Setup and teardown hooks
