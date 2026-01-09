# Common Mistakes

Every testing framework has its sharp edges—places where the syntax looks right but something subtle is wrong. This page collects the mistakes that trip up new Crucible users most often, along with explanations of why they happen and how to avoid them.

## Forgetting `#generate_tests`

This is the most common mistake, and it's frustrating because everything compiles successfully. Your test file builds without errors, you run `lake test`, and... nothing happens. No output, no failures, just silence. Your tests exist in the source code but Crucible doesn't know about them.

The issue is that defining a test with `test "name" := do ...` only creates the test—it doesn't register it anywhere. The `#generate_tests` command scans the current namespace, finds all the test definitions, and adds them to Crucible's internal registry. Without this command, the tests are orphaned.

```lean
-- ❌ Tests defined but never collected
namespace MyTests
open Crucible

testSuite "My Suite"

test "one" := do 1 ≡ 1
test "two" := do 2 ≡ 2

end MyTests  -- Missing #generate_tests!
```

```lean
-- ✓ Tests collected and will run
namespace MyTests
open Crucible

testSuite "My Suite"

test "one" := do 1 ≡ 1
test "two" := do 2 ≡ 2

#generate_tests  -- Required!

end MyTests
```

The `#generate_tests` command must come after all test definitions and before `end`. If you add it too early, tests defined after it won't be collected. The pattern is always: namespace, then tests, then `#generate_tests`, then end.

## Forgetting `open Crucible`

When you see "unknown identifier 'testSuite'" or "unknown identifier 'test'", the most likely cause is a missing `open Crucible` statement. The Crucible library defines these commands, but they're not visible until you bring them into scope.

```lean
-- ❌ Unknown identifier 'testSuite'
namespace MyTests

testSuite "My Suite"  -- Error!

test "one" := do 1 ≡ 1  -- Error!
```

```lean
-- ✓ Crucible opened
namespace MyTests
open Crucible

testSuite "My Suite"

test "one" := do 1 ≡ 1
```

You could also use qualified names (`Crucible.testSuite`, `Crucible.test`), but that gets verbose quickly. The `open` statement is idiomatic and keeps your tests readable.

## Forgetting to Import Test Modules

You've written tests, added `#generate_tests`, and everything compiles. But when you run `lake test`, only some suites appear—or none at all. The issue is in your test runner: it's not importing all your test files.

When you import a file containing tests, Lean evaluates that file, which runs the `#generate_tests` command and registers the suite. Files that aren't imported are never evaluated, so their tests never register. This is why every test file needs to be listed in your `Tests/Main.lean` imports.

```lean
-- ❌ Tests in MyTests.lean won't run
import Crucible

def main : IO UInt32 := runAllSuites
```

```lean
-- ✓ Import all test modules
import Crucible
import Tests.MyTests
import Tests.OtherTests

def main : IO UInt32 := runAllSuites
```

Each file containing tests must be imported in your `Tests/Main.lean`. When you add a new test file, you need to add its import here too—it's easy to forget this step when you're focused on writing the tests.

## Confusing `≡` with `==`

This mistake compiles and runs but doesn't actually test anything. The difference between `≡` and `==` is subtle but crucial: `≡` is an assertion that throws when the values differ, while `==` is a boolean comparison that returns `true` or `false`.

When you write `(1 + 1) == 2` inside a test, you get `true`—and then nothing happens. The boolean value is computed and discarded. No assertion runs, no error is thrown, and the test passes regardless of what the comparison returns.

```lean
-- ❌ This compiles but doesn't assert anything!
test "wrong" := do
  let _ := (1 + 1) == 2  -- Just returns true, discarded
  pure ()

-- ✓ This actually asserts equality
test "correct" := do
  (1 + 1) ≡ 2  -- Throws if not equal
```

The fix is simple: use `≡` (typed with `\equiv` in your editor) for assertions. When you want to check that two values are equal and fail if they're not, reach for `≡`. Save `==` for when you genuinely need a boolean value—like in an if-condition or as part of a more complex expression.

## Using `≡` Outside a Test

If you try to use `≡` in a regular function, you'll get a type error. The `≡` operator has type `IO Unit`, meaning it's an IO action that either succeeds (returning `()`) or throws an error. You can't use it where a pure value is expected.

```lean
-- ❌ Can't use ≡ at top level
def checkValue : IO Unit := do
  1 ≡ 1  -- Error: not in test context
```

```lean
-- ✓ Use inside a test
test "value check" := do
  1 ≡ 1
```

For helper functions, use `ensure` or return a boolean:

```lean
def isValid (x : Nat) : IO Bool := pure (x > 0)

test "validation" := do
  let valid ← isValid 42
  ensure valid "should be valid"
```

This pattern—helper functions that return results, tests that assert on those results—keeps your assertions where they belong while letting you factor out common logic.

## Wrong Namespace Structure

Crucible expects a specific structure: a namespace containing a `testSuite` declaration, some tests, and a `#generate_tests` command. If you skip the `testSuite` declaration, the tests have no suite to belong to, and registration fails silently.

```lean
-- ❌ Missing testSuite declaration
namespace Tests
open Crucible

test "one" := do 1 ≡ 1

#generate_tests
end Tests
```

```lean
-- ✓ Proper structure
namespace Tests
open Crucible

testSuite "My Tests"  -- Required!

test "one" := do 1 ≡ 1

#generate_tests
end Tests
```

The mental model is that `testSuite` creates a container, tests fill it, and `#generate_tests` seals it and registers it. Without the container, there's nothing to register.

## Multiple Suites in One Namespace

It might seem natural to define two suites in one namespace—just call `testSuite` twice, right? But this doesn't work as expected. Each `testSuite` declaration sets the current suite name, so the second one overwrites the first. Tests defined between them get associated with whichever suite is active at the time.

```lean
-- ❌ Two suites in one namespace causes confusion
namespace Tests
open Crucible

testSuite "Suite A"
test "a1" := do 1 ≡ 1

testSuite "Suite B"  -- This replaces Suite A!
test "b1" := do 2 ≡ 2

#generate_tests
end Tests
```

```lean
-- ✓ Separate namespaces
namespace Tests.SuiteA
open Crucible
testSuite "Suite A"
test "a1" := do 1 ≡ 1
#generate_tests
end Tests.SuiteA

namespace Tests.SuiteB
open Crucible
testSuite "Suite B"
test "b1" := do 2 ≡ 2
#generate_tests
end Tests.SuiteB
```

The rule is one suite per namespace. This constraint keeps the model simple and makes it clear which tests belong to which suite.

## Using `runAllSuites` Without `(args)`

Crucible supports filtering tests from the command line with flags like `--test` and `--suite`. But this only works if you pass the command-line arguments to the runner. The basic `runAllSuites` ignores arguments; you need `runAllSuitesFiltered args` to enable filtering.

```lean
-- ❌ CLI filtering won't work
def main : IO UInt32 := runAllSuites

-- ✓ Enables --test, --suite filtering
def main (args : List String) : IO UInt32 :=
  runAllSuitesFiltered args
```

If you don't need filtering, `runAllSuites` is fine. But if you ever want to run just one test or one suite, switch to `runAllSuitesFiltered` and pass `args` from your `main` function.

## Property Test Without Opening Property Module

Property testing lives in its own module, `Crucible.Property`, which you need to open separately. The `proptest` command and `forAll` combinators aren't available with just `open Crucible`—you need both.

```lean
-- ❌ Unknown 'forAll''
test "commutative" := do
  forAll' fun (a, b) : (Int × Int) => a + b == b + a  -- Error!
```

```lean
-- ✓ Open Property module
open Crucible.Property

proptest "commutative" :=
  forAll' fun (a, b) : (Int × Int) => a + b == b + a
```

Notice that property tests use `proptest` instead of `test`. This is intentional—property tests have different semantics (running many times with different inputs), so they need their own command.

## Assertions in Pure Functions

Crucible's assertions are `IO` actions because they need to throw errors when assertions fail. This means they can only appear in `IO` contexts—you can't use them in pure functions that return non-`IO` types.

```lean
-- ❌ Can't use IO assertions in pure function
def checkPure (x : Nat) : Bool :=
  x ≡ 42  -- Error: expected Bool, got IO Unit
```

```lean
-- ✓ Use pure comparison
def checkPure (x : Nat) : Bool := x == 42

-- ✓ Or make it IO
def checkIO (x : Nat) : IO Unit := do
  x ≡ 42
```

## Tips for Avoiding Mistakes

Starting from a working example is the best way to avoid these issues. Copy an existing test file that works, rename it, and modify the tests. This gives you the correct structure—imports, opens, namespace, testSuite, tests, #generate_tests—without having to remember each piece.

Run `lake test` after adding each test, not after writing a whole file. This way, if something's wrong, you know it's in what you just added. Waiting until you've written twenty tests and then discovering none of them run is frustrating; catching the issue after the first test is easy to fix.

When things don't work, check the basics: is Crucible imported? Is the namespace open? Is `testSuite` declared? Is `#generate_tests` present and in the right place? Is the test file imported in Main.lean? Most issues trace back to one of these being missing.

The canonical pattern is: `namespace` → `open Crucible` → `testSuite "Name"` → tests → `#generate_tests` → `end`. Every working test file follows this structure. If yours looks different, that's where to start investigating.
