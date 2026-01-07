import Crucible.Filter

/-!
# CLI Argument Parsing

Parses command-line arguments into a `TestFilter` for selective test execution.

## Supported Options

| Option | Short | Description |
|--------|-------|-------------|
| `--test PATTERN` | `-t` | Run tests matching PATTERN |
| `--suite PATTERN` | `-s` | Run suites matching PATTERN |
| `--exact` | `-e` | Use exact match (not substring) |
| `--help` | `-h` | Show help message |

## Usage Examples

```bash
# Run tests containing "parse"
lake test -- --test parse

# Run tests in suites containing "HTTP"
lake test -- --suite HTTP

# Multiple patterns (OR logic)
lake test -- -t parse -t validate

# Exact match
lake test -- --exact -t "parse header"

# Combined
lake test -- --suite "HTTP Parser" --test request

# Show help
lake test -- --help
```

## Main Functions

- `parseArgs args` - Parse args into `TestFilter`, prints help on error
- `parseArgsCore args` - Pure parsing, returns `ParseResult`
- `helpRequested args` - Check if `--help` or `-h` present

## Integration

In your test Main.lean:

```lean
def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
```

The `runAllSuitesFiltered` macro handles argument parsing internally.
-/

namespace Crucible.CLI

/-- Print help message for test filtering options. -/
def printHelp : IO Unit := do
  IO.println "Test Filter Options:"
  IO.println "  -t, --test PATTERN    Run tests matching PATTERN (substring)"
  IO.println "  -s, --suite PATTERN   Run suites matching PATTERN (substring)"
  IO.println "  -e, --exact           Use exact match instead of substring"
  IO.println "  -h, --help            Show this help message"
  IO.println ""
  IO.println "Multiple patterns can be specified (OR logic):"
  IO.println "  lake test -- --test 'parse' --test 'validate'"
  IO.println ""
  IO.println "Examples:"
  IO.println "  lake test -- --test parse           # Tests containing 'parse'"
  IO.println "  lake test -- --suite 'HTTP Parser'  # Suites containing 'HTTP Parser'"
  IO.println "  lake test -- --test foo --exact     # Test named exactly 'foo'"

/-- Result of parsing arguments - either a filter or a help request. -/
inductive ParseResult
  | filter (f : TestFilter)
  | help
  | error (msg : String)
  deriving Repr

/-- Parse command-line arguments into a TestFilter. -/
def parseArgsCore (args : List String) : ParseResult :=
  let rec loop (args : List String) (filter : TestFilter) : ParseResult :=
    match args with
    | [] => .filter filter
    | "--help" :: _ => .help
    | "-h" :: _ => .help
    | "--exact" :: rest => loop rest { filter with exactMatch := true }
    | "-e" :: rest => loop rest { filter with exactMatch := true }
    | "--test" :: pattern :: rest =>
      loop rest { filter with testPatterns := filter.testPatterns ++ [pattern] }
    | "-t" :: pattern :: rest =>
      loop rest { filter with testPatterns := filter.testPatterns ++ [pattern] }
    | "--suite" :: pattern :: rest =>
      loop rest { filter with suitePatterns := filter.suitePatterns ++ [pattern] }
    | "-s" :: pattern :: rest =>
      loop rest { filter with suitePatterns := filter.suitePatterns ++ [pattern] }
    | arg :: rest =>
      if arg.startsWith "--test=" then
        loop rest { filter with testPatterns := filter.testPatterns ++ [arg.drop 7] }
      else if arg.startsWith "-t=" then
        loop rest { filter with testPatterns := filter.testPatterns ++ [arg.drop 3] }
      else if arg.startsWith "--suite=" then
        loop rest { filter with suitePatterns := filter.suitePatterns ++ [arg.drop 8] }
      else if arg.startsWith "-s=" then
        loop rest { filter with suitePatterns := filter.suitePatterns ++ [arg.drop 3] }
      else if arg == "--test" || arg == "-t" then
        .error "Missing value for --test"
      else if arg == "--suite" || arg == "-s" then
        .error "Missing value for --suite"
      else
        -- Skip unknown args (may be consumed by lake/lean)
        loop rest filter
  loop args {}

/-- Parse command-line arguments, printing help if requested.
    Returns empty filter on help (caller should check and exit). -/
def parseArgs (args : List String) : IO TestFilter := do
  match parseArgsCore args with
  | .filter f => return f
  | .help =>
    printHelp
    return {}
  | .error msg =>
    IO.eprintln s!"Error: {msg}"
    printHelp
    return {}

/-- Check if help was requested in the arguments. -/
def helpRequested (args : List String) : Bool :=
  args.contains "--help" || args.contains "-h"

end Crucible.CLI
