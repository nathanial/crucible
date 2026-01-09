# Soft Assertions

Collect multiple failures in a single test instead of stopping at the first failure.

## Overview

Normal assertions stop test execution on first failure:

```lean
test "normal assertions" := do
  user.name ≡ "Alice"  -- If this fails...
  user.age ≡ 25        -- ...this never runs
  user.email ≡ "alice@example.com"
```

Soft assertions continue and report all failures:

```lean
test "soft assertions" := withSoftAsserts fun soft => do
  soft.shouldBe user.name "Alice"      -- Checked
  soft.shouldBe user.age 25            -- Checked even if above failed
  soft.shouldBe user.email "alice@ex"  -- Checked even if above failed
  -- All failures reported at end
```

## Basic Usage

Use `withSoftAsserts` to create a soft assertion context:

```lean
test "multiple checks" := withSoftAsserts fun soft => do
  soft.shouldBe actual1 expected1
  soft.shouldBe actual2 expected2
  soft.ensure condition "message"
```

## Available Soft Assertions

All standard assertions have soft versions:

| Standard | Soft Version |
|----------|--------------|
| `shouldBe` | `soft.shouldBe` |
| `shouldBeSome` | `soft.shouldBeSome` |
| `shouldBeNone` | `soft.shouldBeNone` |
| `shouldBeNear` | `soft.shouldBeNear` |
| `ensure` | `soft.ensure` |
| `shouldSatisfy` | `soft.shouldSatisfy` |
| `shouldMatch` | `soft.shouldMatch` |
| `shouldHaveLength` | `soft.shouldHaveLength` |
| `shouldContain` | `soft.shouldContain` |
| `shouldContainAll` | `soft.shouldContainAll` |
| `shouldStartWith` | `soft.shouldStartWith` |
| `shouldEndWith` | `soft.shouldEndWith` |
| `shouldContainSubstr` | `soft.shouldContainSubstr` |
| `shouldBeBetween` | `soft.shouldBeBetween` |
| `shouldBeEmpty` | `soft.shouldBeEmpty` |
| `shouldNotBeEmpty` | `soft.shouldNotBeEmpty` |

## Output Format

When soft assertions fail, all failures are listed:

```
[1/1]  user validation... ✗ (3ms)
    Soft assertion failures (3 total):
      1. Expected "Alice", got "Bob"
      2. Expected 25, got 30
      3. Expected "alice@example.com", got "bob@example.com"
```

## Examples

### Validating Complex Objects

```lean
test "user record" := withSoftAsserts fun soft => do
  let user ← fetchUser 123

  soft.shouldBe user.name "Alice"
  soft.shouldBe user.age 25
  soft.shouldBe user.email "alice@example.com"
  soft.shouldBe user.role "admin"
  soft.ensure user.isActive "user should be active"
```

### Checking Multiple Conditions

```lean
test "response validation" := withSoftAsserts fun soft => do
  let response ← apiCall

  soft.shouldBe response.status 200
  soft.shouldContainSubstr response.contentType "json"
  soft.shouldSatisfy (response.body.length > 0) "body not empty"
  soft.shouldBe response.headers.get? "X-Request-Id" requestId
```

### Testing Collections

```lean
test "list properties" := withSoftAsserts fun soft => do
  let items ← fetchItems

  soft.shouldNotBeEmpty items
  soft.shouldHaveLength items 5
  soft.shouldContain items "required-item"
  soft.shouldContainAll items ["a", "b", "c"]
```

## Mixing Hard and Soft Assertions

You can mix both in the same test:

```lean
test "mixed assertions" := withSoftAsserts fun soft => do
  -- Hard assertion: stops if this fails
  let user ← fetchUser userId

  -- Soft assertions: collect all failures
  soft.shouldBe user.name expected.name
  soft.shouldBe user.email expected.email
```

This is useful when:
1. Setup must succeed (hard assertion)
2. Multiple properties should be checked (soft assertions)

## Use Cases

### Testing API Responses

```lean
test "API response format" := withSoftAsserts fun soft => do
  let resp ← callAPI

  soft.shouldBe resp.status 200
  soft.shouldStartWith resp.contentType "application/json"
  soft.shouldContainSubstr resp.body "success"
  soft.ensure (resp.latencyMs < 1000) "should respond quickly"
```

### Configuration Validation

```lean
test "config file" := withSoftAsserts fun soft => do
  let config ← loadConfig

  soft.shouldBe config.version "2.0"
  soft.ensure config.debug == false "debug should be off"
  soft.shouldHaveLength config.servers 3
  soft.shouldContain config.features "authentication"
```

### Data Migration Tests

```lean
test "migrated data" := withSoftAsserts fun soft => do
  let old ← fetchOldRecord
  let new ← fetchNewRecord

  soft.shouldBe new.id old.id
  soft.shouldBe new.name old.name
  soft.shouldBe new.createdAt old.createdAt
  soft.ensure (new.version > old.version) "version should increase"
```

## Best Practices

1. **Use for independent checks**: Soft assertions shine when checks are independent

2. **Don't overuse**: If failures are related, hard assertions give clearer output

3. **Combine with hard assertions**: Use hard assertions for setup, soft for validation

4. **Group logically**: Put related soft assertions together in one test

5. **Check the context**: If one failure implies others, hard assertions are clearer

## When to Use

| Use Soft Assertions | Use Hard Assertions |
|---------------------|---------------------|
| Checking multiple independent fields | Setup that must succeed |
| Seeing all failures at once matters | Failures are dependent |
| Validating complex objects | Sequential operations |
| Comparing entire records | Early exit on failure is helpful |
