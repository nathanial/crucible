# Quick Start

This guide walks you through writing your first test suite with Crucible. By the end, you'll have a working test file, a test runner, and an understanding of how Crucible organizes and executes tests.

## Your First Test

Create a test file `Tests/MyTests.lean`:

```lean
import Crucible

namespace MyTests
open Crucible

testSuite "My First Suite"

test "addition works" := do
  (1 + 1) ≡ 2

test "strings can be compared" := do
  "hello".length ≡ 5

#generate_tests

end MyTests
```

Every Crucible test file follows the same structure. You start by importing Crucible to bring the test framework into scope. The `namespace` declaration groups your tests together and gives them a unique identity—this becomes important when you have many test files, since each namespace can contain its own suite without conflicting with others.

The `open Crucible` statement brings the framework's definitions into scope so you can use `test`, `testSuite`, and the assertion operators without qualification. Without this line, you'd need to write `Crucible.test` and `Crucible.testSuite` everywhere.

The `testSuite "My First Suite"` declaration names the suite. This name appears in the test output and helps you identify which group of tests is running. Think of it as a chapter heading for this set of related tests.

Each test is defined with `test "description" := do` followed by the test body. The description is what you'll see in the output, so make it clear and specific. Inside the test body, you write assertions. The `≡` operator (typed with `\equiv`) checks that two values are equal and throws a descriptive error if they're not.

Finally, `#generate_tests` scans the namespace and collects all the tests you've defined. This command must come after your test definitions and before the `end` that closes the namespace. Without it, Crucible won't know your tests exist.

## The Test Runner

Tests don't run themselves—you need an entry point that knows about all your test files. Create `Tests/Main.lean` to serve as this central runner:

```lean
import Crucible
import Tests.MyTests

open Crucible

def main : IO UInt32 := do
  runAllSuites
```

The key insight here is that importing a test file causes its `#generate_tests` command to register the suite with Crucible's global registry. When you call `runAllSuites`, it iterates through every registered suite and executes their tests. This means adding a new test file is just a matter of importing it here—no other configuration needed.

The function returns `IO UInt32` because it's designed to work with shell conventions: it returns `0` when all tests pass and `1` when any test fails. This makes it easy to use Crucible in CI pipelines or scripts that check the exit code.

## Running Tests

With everything in place, run your tests using Lake's test command:

```bash
lake test
```

You'll see output like:

```
My First Suite
──────────────
[1/2]  addition works... ✓ (1ms)
[2/2]  strings can be compared... ✓ (0ms)

Results: 2 passed

────────────────────────────────────────
Summary: 2 passed, 0 failed (100.0%)
         1 suites, 2 tests run
         Completed in 0.01s
────────────────────────────────────────
```

## Multiple Test Suites

As your project grows, you'll want to organize tests by area of functionality. Each suite lives in its own namespace, which keeps tests isolated and makes the output easier to navigate. Here's an example with two suites in the same file:

```lean
import Crucible

namespace ArithmeticTests
open Crucible

testSuite "Arithmetic"

test "addition" := do (1 + 1) ≡ 2
test "subtraction" := do (5 - 3) ≡ 2

#generate_tests

end ArithmeticTests

namespace StringTests
open Crucible

testSuite "Strings"

test "length" := do "hello".length ≡ 5
test "append" := do ("a" ++ "b") ≡ "ab"

#generate_tests

end StringTests
```

In practice, you'd often put each suite in its own file—`Tests/ArithmeticTests.lean` and `Tests/StringTests.lean`—and import both in your test runner. The pattern stays the same: each file is a self-contained suite with its own namespace, `testSuite` declaration, tests, and `#generate_tests` call.

## Testing with IO

Because tests run in the `IO` monad, you can test code that reads files, makes network requests, accesses databases, or performs any other side effect. This makes Crucible suitable for integration testing, not just unit testing.

```lean
test "file operations" := do
  let content ← IO.FS.readFile "test-data.txt"
  content.length > 0 |> ensure "file should not be empty"
```

When testing effectful code, keep in mind that tests run sequentially within a suite and each test starts with a fresh state. If you need shared setup—like creating a test file before running tests that read it—use fixtures, which are covered in a later guide.

## Using Assertions

Crucible provides several assertion styles, each suited to different situations:

```lean
-- Equality (recommended)
(actual) ≡ expected

-- Option unwrapping
String.toNat? "42" ≡? 42

-- Boolean conditions
ensure (value > 0) "value should be positive"

-- Named assertions
shouldBe actual expected
shouldBeSome option expected
shouldBeNone option
```

See the [Assertions](./assertions.md) page for the complete list.

## Next Steps

- [Writing Tests](./writing-tests.md) - Learn more about test syntax
- [Test Suites](./test-suites.md) - Organize tests into suites
- [Assertions](./assertions.md) - All available assertions
- [Fixtures](./fixtures.md) - Setup and teardown hooks
