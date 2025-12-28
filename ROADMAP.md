# Crucible Roadmap

This document outlines potential improvements, new features, and cleanup opportunities for the Crucible test framework.

---

## Feature Proposals

### [Priority: Medium] Expected Failure / Skip Test Support

**Description:** Add ability to mark tests as expected to fail (`xfail`) or to skip them conditionally.

**Rationale:** Common test framework feature for:
- Documenting known bugs without breaking CI
- Skipping platform-specific tests
- Skipping tests that require external resources

**Affected Files:**
- `Crucible/Core.lean` - Add `TestCase.xfail`, `TestCase.skip`, `TestCase.skipIf`
- `Crucible/Macros.lean` - Add syntax like `test "name" (skip := true) := do`

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Parallel Test Execution

**Description:** Add option to run tests in parallel using Lean's task system.

**Rationale:** Large test suites (like ledger with 80+ tests, collimator with 200+ tests) would benefit from parallel execution. Currently all tests run sequentially.

**Affected Files:**
- `Crucible/Core.lean` - Add `runTestsParallel` with configurable concurrency

**Estimated Effort:** Medium

**Dependencies:** May need fixture support to handle shared resource initialization

---

### [Priority: Medium] Test Filtering / Selection

**Description:** Add command-line arguments or configuration for running specific tests or suites.

**Rationale:** During development, it's useful to run only specific tests rather than the entire suite. Common patterns include name matching, tag filtering, or suite selection.

**Affected Files:**
- `Crucible/Core.lean` - Add `runTestsFiltered` with name pattern matching
- New file `Crucible/CLI.lean` - Command-line argument parsing

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Better Test Output Formatting

**Description:** Improve test output with colors, progress indicators, and summary statistics.

**Rationale:** Current output is minimal. Enhanced formatting could include:
- Colored pass/fail indicators (green checkmark, red X)
- Progress bar for long test runs
- Timing information per test
- Summary with total time, pass/fail counts by suite

**Affected Files:**
- `Crucible/Core.lean` - Enhance `runTest` and `runTests` output
- New file `Crucible/Output.lean` - Formatting utilities

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Low] Property-Based Testing Integration

**Description:** Add hooks or utilities for integrating with property-based testing libraries like `plausible`.

**Rationale:** Some projects already use plausible (tincture, chroma). The collimator tests at `/Users/Shared/Projects/lean-workspace/collimator/CollimatorTests/LensTests.lean` lines 334-426 show manual property testing patterns that could be formalized.

**Affected Files:**
- New file `Crucible/Property.lean` - Property test helpers and assertions

**Estimated Effort:** Medium

**Dependencies:** Optional dependency on plausible

---

### [Priority: Low] Structured Test Data Capture

**Description:** Add ability to capture and export test results in structured formats (JSON, JUnit XML).

**Rationale:** CI/CD integration often requires structured test output for reporting tools, dashboards, and trend analysis.

**Affected Files:**
- New file `Crucible/Export.lean` - Test result serialization

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Low] Snapshot Testing

**Description:** Add support for snapshot testing where expected output is stored in files and compared against actual output.

**Rationale:** Useful for testing complex output (rendered text, serialized data) without writing complex equality checks.

**Affected Files:**
- New file `Crucible/Snapshot.lean` - Snapshot management and comparison

**Estimated Effort:** Large

**Dependencies:** None

---

## Code Improvements

### [Priority: Low] Structured Test Results Type

**Current State:** The `runAllSuites` function now handles automatic suite discovery and aggregation, returning `IO UInt32`. Projects no longer need to manually accumulate exit codes.

**Proposed Enhancement:** Return a structured `TestResults` type with pass/fail counts per suite, timing information, and provide a standard `toExitCode` method for richer reporting.

**Benefits:** Enables richer reporting, timing analysis, and programmatic access to results.

**Affected Files:**
- `Crucible/Core.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] Type-Safe Test Tags

**Current State:** No tagging system exists for categorizing tests.

**Proposed Change:** Add a tag system using a custom type or string array, enabling test filtering by category (unit, integration, slow, etc.).

**Benefits:** Better organization, selective test runs.

**Affected Files:**
- `Crucible/Core.lean` - Add tags to `TestCase`
- `Crucible/Macros.lean` - Add tag syntax

**Estimated Effort:** Medium

---

### [Priority: Low] Lazy Test Case Evaluation

**Current State:** All `TestCase` structures are created at module load time.

**Proposed Change:** Use thunks for the `run` field to defer test body construction until execution.

**Benefits:** Potentially faster startup for large test suites.

**Affected Files:**
- `Crucible/Core.lean`

**Estimated Effort:** Small

---

## Code Cleanup

### [Priority: Medium] Add Module-Level Documentation

**Issue:** While there are file-level doc comments, the overall architecture and usage patterns are not well documented.

**Location:** All source files

**Action Required:**
- Add comprehensive module docstrings
- Document the relationship between Core, Macros, and SuiteRegistry
- Add usage examples in doc comments

**Estimated Effort:** Small

---

### [Priority: Low] Consolidate Error Message Format

**Issue:** Error messages across different assertions have slightly different formats. Some prefix with "Expected", others with "Assertion failed:".

**Location:** `Crucible/Core.lean` lines 40-105

**Action Required:** Standardize error message format across all assertions for consistent output.

**Estimated Effort:** Small

---

## API Enhancements

### [Priority: Low] Soft Assertions

**Description:** Add assertions that record failures but don't stop test execution, allowing multiple checks per test.

**Rationale:** Sometimes useful to check multiple conditions and see all failures at once.

**Affected Files:**
- `Crucible/Core.lean` - Add `softAssert` family of functions
- Possibly need a test context monad to track soft failures

**Estimated Effort:** Medium

---

## Notes

### Dependencies on This Project

The following projects depend on crucible:
- afferent
- arbor
- chroma
- collimator
- enchiridion
- ledger
- legate
- protolean
- terminus
- tincture
- trellis
- wisp

Any breaking changes should be carefully considered and communicated.

### Lean Version

Currently targeting Lean 4.26.0. Feature additions should be compatible with this version.
