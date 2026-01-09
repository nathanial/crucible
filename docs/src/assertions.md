# Assertions

Crucible provides a comprehensive set of assertions for testing your code.

## Equality Assertions

### The `≡` Operator

The primary assertion operator. Type `\equiv` or copy the Unicode character.

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

### The `≡?` Operator

Unwraps an `Option` and checks equality. Type `\equiv?`.

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

### `ensure`

Assert a condition with a message:

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

### `shouldBeNear`

Assert floats are approximately equal:

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

### `shouldHaveLength`

Assert list length:

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

### `shouldStartWith`

Assert string prefix:

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

### `shouldThrow`

Assert that an action throws:

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

### `shouldBeOk`

Assert `Except` is `Ok` and return the value:

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

### `withContext`

Add context to assertion failures:

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
