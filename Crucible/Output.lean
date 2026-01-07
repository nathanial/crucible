/-!
# Test Output Formatting

ANSI color utilities and symbols for test result display.

## Symbols

- `passSymbol` = ✓ (green) - Test passed
- `failSymbol` = ✗ (red) - Test failed
- `skipSymbol` = ⊘ (yellow) - Test skipped
- `xfailSymbol` = ✗ (yellow) - Expected failure (good)
- `xpassSymbol` = ✗ (red) - Unexpected pass (bad)

## Color Functions

All functions take a string and return it wrapped in ANSI codes:

- `green`, `red`, `yellow`, `cyan` - Foreground colors
- `bold`, `dim` - Text styles
- `boldGreen`, `boldRed`, `boldYellow` - Combined styles

## Display Helpers

- `progress current total` → `[1/10]` (dimmed)
- `timing ms` → `(42ms)` (dimmed)

## Example Output

```
HTTP Parser Tests
─────────────────
[1/3]  parse valid request... ✓ (2ms)
[2/3]  parse malformed request... ✗ (1ms)
    Expected status 400, got 200
[3/3]  not implemented... ⊘ (skipped: todo)

Results: 1 passed, 1 failed, 1 skipped
```
-/

namespace Crucible.Output

/-! ## ANSI Escape Primitives -/

/-- ANSI escape sequence prefix. -/
def esc : String := "\x1b["

/-- Reset all formatting. -/
def reset : String := esc ++ "0m"

/-- Build an SGR (Select Graphic Rendition) sequence from codes. -/
def sgr (codes : List String) : String :=
  if codes.isEmpty then ""
  else esc ++ ";".intercalate codes ++ "m"

/-! ## Modifier Codes -/

def codeBold : String := "1"
def codeDim : String := "2"

/-! ## Foreground Color Codes -/

def fgRed : String := "31"
def fgGreen : String := "32"
def fgYellow : String := "33"
def fgCyan : String := "36"

/-! ## Styling Functions -/

/-- Apply ANSI styling codes to text. -/
def styled (text : String) (codes : List String) : String :=
  if codes.isEmpty then text
  else sgr codes ++ text ++ reset

/-- Green text. -/
def green (text : String) : String := styled text [fgGreen]

/-- Red text. -/
def red (text : String) : String := styled text [fgRed]

/-- Yellow text. -/
def yellow (text : String) : String := styled text [fgYellow]

/-- Cyan text. -/
def cyan (text : String) : String := styled text [fgCyan]

/-- Dim (faded) text. -/
def dim (text : String) : String := styled text [codeDim]

/-- Bold text. -/
def bold (text : String) : String := styled text [codeBold]

/-- Bold green text. -/
def boldGreen (text : String) : String := styled text [codeBold, fgGreen]

/-- Bold red text. -/
def boldRed (text : String) : String := styled text [codeBold, fgRed]

/-- Bold yellow text. -/
def boldYellow (text : String) : String := styled text [codeBold, fgYellow]

/-! ## Test Result Symbols -/

/-- Green checkmark for passed tests. -/
def passSymbol : String := green "✓"

/-- Red X for failed tests. -/
def failSymbol : String := red "✗"

/-- Yellow circle-slash for skipped tests. -/
def skipSymbol : String := yellow "⊘"

/-- Yellow X for expected failures (xfail). -/
def xfailSymbol : String := yellow "✗"

/-- Red X for unexpected passes (xpass - this is bad). -/
def xpassSymbol : String := red "✗"

/-! ## Display Helpers -/

/-- Progress indicator showing current/total. -/
def progress (current total : Nat) : String :=
  dim s!"[{current}/{total}]"

/-- Timing display in milliseconds. -/
def timing (ms : Nat) : String :=
  dim s!"({ms}ms)"

end Crucible.Output
