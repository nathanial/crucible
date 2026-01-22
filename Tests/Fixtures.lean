/-
  Tests for fixture/hook functionality
-/
import Crucible

namespace Tests.Fixtures

open Crucible

-- Use an IORef to track hook execution order
def hookLog : IO (IO.Ref (List String)) := IO.mkRef []

testSuite "Fixture Hooks"

-- Define the fixture hooks
beforeAll := do
  let log ← hookLog
  log.modify (· ++ ["beforeAll"])

afterAll := do
  let log ← hookLog
  log.modify (· ++ ["afterAll"])

beforeEach := do
  let log ← hookLog
  log.modify (· ++ ["beforeEach"])

afterEach := do
  let log ← hookLog
  log.modify (· ++ ["afterEach"])

test "first test" := do
  let log ← hookLog
  log.modify (· ++ ["test1"])
  ensure true "first test runs"

test "second test" := do
  let log ← hookLog
  log.modify (· ++ ["test2"])
  ensure true "second test runs"

end Tests.Fixtures

namespace Tests.NoFixtures

open Crucible

testSuite "No Fixtures"

test "test without fixtures" := do
  1 + 1 ≡ 2

test "another test without fixtures" := do
  "hello".length ≡ 5

end Tests.NoFixtures

namespace Tests.SkipXfail

open Crucible

testSuite "Skip and Xfail Tests"

test "normal passing test" := do
  1 + 1 ≡ 2

test "skipped test with reason" (skip := "not implemented yet") := do
  -- This test body won't be executed
  throw <| IO.userError "This should not run"

test "skipped test" (skip) := do
  -- This test body won't be executed
  throw <| IO.userError "This should not run"

test "expected failure that fails" (xfail := "known bug #123") := do
  -- This test is expected to fail
  throw <| IO.userError "Expected error"

test "expected failure simple" (xfail) := do
  -- This test is expected to fail
  ensure false "This is expected to fail"

end Tests.SkipXfail

namespace Tests.SoftAsserts

open Crucible

testSuite "Soft Assertions"

test "soft assertions collect all failures" := do
  -- This test should fail with multiple failures listed
  let ctx ← SoftAssertContext.new
  ctx.ensure false "first check"
  ctx.ensure false "second check"
  ctx.ensure true "third check (passes)"
  ctx.shouldBe 1 2
  let failures ← ctx.getFailures
  failures.size ≡ 3

test "soft assertions pass when all succeed" := withSoftAsserts fun soft => do
  soft.ensure true "first check"
  soft.ensure true "second check"
  soft.shouldBe 42 42
  soft.shouldBe "hello" "hello"

test "withSoftAsserts fails with summary" (xfail := "demonstrates soft assertion failure format") := withSoftAsserts fun soft => do
  soft.ensure false "age should be positive"
  soft.shouldBe "actual" "expected"
  soft.shouldContain [1, 2, 3] 4

test "soft shouldBeSome works" := withSoftAsserts fun soft => do
  soft.shouldBeSome (some 42) 42
  soft.shouldBeNone (none : Option Int)

test "soft shouldHaveLength works" := withSoftAsserts fun soft => do
  soft.shouldHaveLength [1, 2, 3] 3
  soft.shouldBeEmpty ([] : List Int)
  soft.shouldNotBeEmpty [1]

test "soft string assertions work" := withSoftAsserts fun soft => do
  soft.shouldStartWith "hello world" "hello"
  soft.shouldEndWith "hello world" "world"
  soft.shouldContainSubstr "hello world" "lo wo"

test "soft shouldBeBetween works" := withSoftAsserts fun soft => do
  soft.shouldBeBetween 5 1 10
  soft.shouldBeBetween 1 1 10
  soft.shouldBeBetween 10 1 10

test "soft shouldMatch works" := withSoftAsserts fun soft => do
  soft.shouldMatch 42 (· > 0) "positive"
  soft.shouldSatisfy true "always true"

test "failure count tracking" := do
  let ctx ← SoftAssertContext.new
  let count1 ← ctx.failureCount
  count1 ≡ 0
  ctx.ensure false "fail 1"
  ctx.ensure false "fail 2"
  let count2 ← ctx.failureCount
  count2 ≡ 2
  let hasFailures ← ctx.hasFailures
  ensure hasFailures "should have failures"

end Tests.SoftAsserts
