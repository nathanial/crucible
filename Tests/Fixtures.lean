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
