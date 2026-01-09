# CLI Reference

Run and filter tests from the command line.

## Basic Usage

Run all tests:

```bash
lake test
```

Run with arguments (note the `--` separator):

```bash
lake test -- [options]
```

## Options

| Option | Short | Description |
|--------|-------|-------------|
| `--test PATTERN` | `-t` | Run tests matching PATTERN |
| `--suite PATTERN` | `-s` | Run suites matching PATTERN |
| `--exact` | `-e` | Use exact match (not substring) |
| `--help` | `-h` | Show help message |

## Filtering Tests

### By Test Name

Run tests containing a substring:

```bash
lake test -- --test parse
lake test -- -t parse
```

This matches tests like:
- "parse header"
- "can parse JSON"
- "parser handles empty input"

### By Suite Name

Run suites containing a substring:

```bash
lake test -- --suite HTTP
lake test -- -s HTTP
```

This matches suites like:
- "HTTP Client"
- "HTTP Parser"

### Combining Filters

Filter by both suite and test:

```bash
lake test -- --suite Parser --test header
```

This runs tests containing "header" in suites containing "Parser".

### Multiple Patterns (OR Logic)

Multiple patterns of the same type use OR logic:

```bash
lake test -- -t parse -t validate
```

Runs tests matching "parse" OR "validate".

### Exact Match

Use exact matching instead of substring:

```bash
lake test -- --exact -t "parse header"
```

Only runs tests named exactly "parse header".

### Equals Syntax

Alternative syntax using `=`:

```bash
lake test -- --test=parse
lake test -- -t=parse
```

## Setup

To enable CLI filtering, use `runAllSuitesFiltered` in your test runner:

```lean
import Crucible

def main (args : List String) : IO UInt32 := do
  runAllSuitesFiltered args
```

The `args` parameter passes command-line arguments to the filter.

## Examples

### Run Specific Tests

```bash
# Tests containing "authentication"
lake test -- -t authentication

# Tests in "User" suites
lake test -- -s User

# Exact match for a specific test
lake test -- --exact -t "validates email format"
```

### Development Workflow

```bash
# Run only the test you're working on
lake test -- -t "my new feature"

# Run a single suite
lake test -- -s "Database"

# Run related tests
lake test -- -t database -t query
```

### CI/CD Usage

```bash
# Run all tests (no filter)
lake test

# Run integration tests only
lake test -- -s Integration

# Run smoke tests
lake test -- -t smoke
```

## Output

### Normal Output

```
Database
────────
[1/3]  can connect... ✓ (5ms)
[2/3]  can query... ✓ (10ms)
[3/3]  can insert... ✓ (8ms)

Results: 3 passed

────────────────────────────────────────
Summary: 3 passed, 0 failed (100.0%)
         1 suites, 3 tests run
         Completed in 0.03s
────────────────────────────────────────
```

### Filtered Output

When filters are applied, they're shown in the summary:

```
────────────────────────────────────────
Summary: 2 passed, 0 failed (100.0%)
         1 suites, 2 tests run
         Completed in 0.02s
────────────────────────────────────────

  Test filter: ["query"]
```

## Help

Show available options:

```bash
lake test -- --help
```

Output:

```
Test Filter Options:
  -t, --test PATTERN    Run tests matching PATTERN (substring)
  -s, --suite PATTERN   Run suites matching PATTERN (substring)
  -e, --exact           Use exact match instead of substring
  -h, --help            Show this help message

Multiple patterns can be specified (OR logic):
  lake test -- --test 'parse' --test 'validate'

Examples:
  lake test -- --test parse           # Tests containing 'parse'
  lake test -- --suite 'HTTP Parser'  # Suites containing 'HTTP Parser'
  lake test -- --test foo --exact     # Test named exactly 'foo'
```

## Advanced Usage

### With Timeout

Combine filtering with suite-wide settings:

```lean
def main (args : List String) : IO UInt32 := do
  runAllSuitesFiltered args (timeout := 5000)
```

### With Retry

```lean
def main (args : List String) : IO UInt32 := do
  runAllSuitesFiltered args (retry := 2)
```

### Getting Structured Results

```lean
def main (args : List String) : IO UInt32 := do
  let results ← runAllSuitesFilteredWithResults args
  -- Custom result handling
  for suite in results.suites do
    IO.println s!"{suite.name}: {suite.passed}/{suite.total}"
  return results.toExitCode
```

## Tips

1. **Use quotes for patterns with spaces**: `lake test -- -t "my test"`

2. **Combine suite and test filters**: Be specific to run exactly what you need

3. **Start broad, narrow down**: Start with a suite filter, then add test filters

4. **Use exact match sparingly**: Substring matching is usually more convenient

5. **Check output for applied filters**: The summary shows what filters were used
