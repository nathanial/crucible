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

#generate_tests

end Tests.Fixtures

namespace Tests.NoFixtures

open Crucible

testSuite "No Fixtures"

test "test without fixtures" := do
  1 + 1 ≡ 2

test "another test without fixtures" := do
  "hello".length ≡ 5

#generate_tests

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

#generate_tests

end Tests.SkipXfail
