# Assertions

Assertions are the core of testing—they express what you expect to be true and fail loudly when reality disagrees. Crucible provides over 30 built-in assertions covering equality, options, collections, strings, exceptions, and more.

Each assertion follows the same pattern: check a condition, and if it fails, throw an error with a descriptive message. This means tests stop at the first failure by default, which helps you focus on one problem at a time. For cases where you want to check multiple conditions and see all failures, see [Soft Assertions](./soft-assertions.md).

## Equality Assertions

Most tests ultimately check that two values are equal. Crucible's equality assertions are the workhorses you'll use constantly—they compare values, show the difference when they don't match, and work with any type that has a `BEq` instance.

### The `≡` Operator

The primary assertion operator. Type `\equiv` in your editor or copy the Unicode character directly. This is the assertion you'll reach for most often.

```lean
test "equality" := do
  (1 + 1) ≡ 2
  "hello" ≡ "hello"
  [1, 2, 3] ≡ [1, 2, 3]
```

### `shouldBe`

Function form of equality assertion:

```lean
test "shouldBe" := do
  shouldBe (1 + 1) 2
  shouldBe actual expected
```

## Option Assertions

Working with `Option` values is common in Lean, and unwrapping them manually in every test adds noise. Option assertions handle the unwrapping for you: they check that the option is `some` and that the contained value matches your expectation, or they verify that an option is `none`.

### The `≡?` Operator

Unwraps an `Option` and checks equality in one step. Type `\equiv?` in your editor. If the option is `none`, the assertion fails with a clear message about expecting `some`.

```lean
test "option equality" := do
  String.toNat? "42" ≡? 42
  [1, 2, 3].head? ≡? 1
```

### `shouldBeSome`

Function form of Option assertion:

```lean
test "shouldBeSome" := do
  shouldBeSome (some 42) 42
  shouldBeSome list.head? expectedFirst
```

### `shouldBeNone`

Assert that an Option is `none`:

```lean
test "shouldBeNone" := do
  shouldBeNone (none : Option Nat)
  shouldBeNone (String.toNat? "abc")
```

## Boolean Assertions

Sometimes you need to check a condition that doesn't fit neatly into equality. Is this number positive? Is this list non-empty? Does this string match a pattern? Boolean assertions let you express arbitrary conditions and provide custom messages when they fail.

### `ensure`

The most direct boolean assertion. Give it a condition and a message describing what should be true:

```lean
test "ensure" := do
  ensure (value > 0) "value should be positive"
  ensure list.length > 0 "list should not be empty"
```

### `shouldSatisfy`

Assert a condition is true:

```lean
test "shouldSatisfy" := do
  shouldSatisfy (x > 0) "x should be positive"
```

### `shouldMatch`

Assert a value satisfies a predicate:

```lean
test "shouldMatch" := do
  shouldMatch 42 (· > 0) "should be positive"
  shouldMatch "hello" (·.length < 10) "should be short"
```

## Numeric Assertions

Floating-point arithmetic is notoriously imprecise—`0.1 + 0.2` doesn't exactly equal `0.3`. Numeric assertions handle this by comparing within a tolerance, so you can test floating-point code without fighting rounding errors.

### `shouldBeNear`

Assert that two floats are approximately equal, within an epsilon tolerance. The default epsilon is 0.0001, but you can specify a custom value for tighter or looser comparisons:

```lean
test "floating point" := do
  shouldBeNear 0.1 + 0.2 0.3  -- default epsilon: 0.0001
  shouldBeNear result expected 0.001  -- custom epsilon
```

### `shouldBeApprox`

Alias for `shouldBeNear`:

```lean
test "approximate" := do
  shouldBeApprox calculated expected 0.01
```

### `shouldBeBetween`

Assert value is in range (inclusive):

```lean
test "range" := do
  shouldBeBetween 5 1 10     -- 5 is between 1 and 10
  shouldBeBetween x min max
```

## Collection Assertions

Tests often work with lists and arrays, and you frequently need to verify their contents. Rather than converting to specific values and comparing, collection assertions let you express what you care about directly: length, membership, containment.

### `shouldHaveLength`

Assert that a list has a specific length. This is often the first check you make when testing functions that produce collections:

```lean
test "length" := do
  shouldHaveLength [1, 2, 3] 3
  shouldHaveLength [] 0
```

### `shouldContain`

Assert list contains element:

```lean
test "contains" := do
  shouldContain [1, 2, 3] 2
  shouldContain names "Alice"
```

### `shouldContainAll`

Assert list contains all elements (order independent):

```lean
test "containsAll" := do
  shouldContainAll [1, 2, 3, 4] [2, 4]
  shouldContainAll result expectedItems
```

### `shouldBeEmpty`

Assert list is empty:

```lean
test "empty" := do
  shouldBeEmpty []
  shouldBeEmpty filteredList
```

### `shouldNotBeEmpty`

Assert list is not empty:

```lean
test "notEmpty" := do
  shouldNotBeEmpty [1, 2, 3]
  shouldNotBeEmpty results
```

## String Assertions

Strings have their own patterns: checking prefixes for URLs or file paths, checking suffixes for extensions, searching for substrings in output or logs. String assertions handle these common cases with readable syntax.

### `shouldStartWith`

Assert that a string begins with a specific prefix. Useful for URLs, paths, or any string with a predictable start:

```lean
test "startsWith" := do
  shouldStartWith "hello world" "hello"
  shouldStartWith url "https://"
```

### `shouldEndWith`

Assert string suffix:

```lean
test "endsWith" := do
  shouldEndWith "hello.txt" ".txt"
  shouldEndWith path "/"
```

### `shouldContainSubstr`

Assert string contains substring:

```lean
test "containsSubstr" := do
  shouldContainSubstr "hello world" "world"
  shouldContainSubstr errorMsg "not found"
```

## Exception Assertions

Good code throws exceptions for invalid inputs, failed operations, and error conditions. Testing this behavior requires assertions that expect failures—ones that pass when an exception occurs and fail when the code completes normally.

### `shouldThrow`

Assert that an action throws any exception. The test passes if an exception is raised, regardless of the exception message:

```lean
test "throws" := do
  shouldThrow (divide 1 0)
  shouldThrow (parseStrict "invalid")
```

### `shouldThrowWith`

Assert exception message contains substring:

```lean
test "throwsWith" := do
  shouldThrowWith (validate "") "empty"
  shouldThrowWith (connect badUrl) "connection"
```

### `shouldThrowMatching`

Assert exception matches predicate:

```lean
test "throwMatching" := do
  shouldThrowMatching (riskyOp) (·.startsWith "Error:")
```

### `shouldNotThrow`

Assert that an action completes without throwing:

```lean
test "noThrow" := do
  shouldNotThrow (safeOperation)
```

## Except Assertions

The `Except` type represents computations that might fail, returning either `ok value` or `error e`. These assertions check which case you got and let you work with the value or error.

### `shouldBeOk`

Assert that an `Except` is `ok` and extract the value for further testing. This is useful when you want to verify both that the operation succeeded and that the result has the right properties:

```lean
test "ok" := do
  let value ← shouldBeOk (parseConfig input) "parsing config"
  value.setting ≡ expected
```

### `shouldBeErr`

Assert `Except` is an error:

```lean
test "error" := do
  shouldBeErr (validate "")
```

## Context and Messages

When a test fails, the assertion message shows what was expected versus what was received. But sometimes you need more context: which iteration of a loop failed? Which field of a structure was wrong? Context and message helpers let you annotate assertions with additional information.

### `withContext`

Add explanatory context that appears when an assertion fails. The context is appended to the failure message, helping you understand which part of a complex test failed:

```lean
test "with context" := do
  (user.age ≡ 25) |> withContext "checking user age"
  (user.name ≡ "Alice") |> withContext "checking user name"
```

### `withMessage`

Replace failure message entirely:

```lean
test "custom message" := do
  withMessage "User should be an adult" do
    ensure (user.age >= 18) "age check"
```

## Quick Reference

| Assertion | Description |
|-----------|-------------|
| `a ≡ b` | Assert equality |
| `opt ≡? val` | Assert Option contains value |
| `shouldBe a b` | Assert equality (function) |
| `shouldBeSome opt val` | Assert Option contains value |
| `shouldBeNone opt` | Assert Option is none |
| `ensure cond msg` | Assert condition is true |
| `shouldSatisfy cond msg` | Assert condition is true |
| `shouldMatch val pred` | Assert value matches predicate |
| `shouldBeNear a b eps` | Assert floats approximately equal |
| `shouldBeBetween val min max` | Assert value in range |
| `shouldHaveLength list n` | Assert list length |
| `shouldContain list elem` | Assert list contains element |
| `shouldContainAll list elems` | Assert list contains all |
| `shouldBeEmpty list` | Assert list is empty |
| `shouldNotBeEmpty list` | Assert list is not empty |
| `shouldStartWith str prefix` | Assert string prefix |
| `shouldEndWith str suffix` | Assert string suffix |
| `shouldContainSubstr str sub` | Assert substring |
| `shouldThrow action` | Assert action throws |
| `shouldThrowWith action msg` | Assert exception contains message |
| `shouldNotThrow action` | Assert action doesn't throw |
| `shouldBeOk result ctx` | Assert Except is Ok |
| `shouldBeErr result` | Assert Except is error |
