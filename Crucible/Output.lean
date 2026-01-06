/-!
# Test Output Formatting

ANSI color utilities and styled output helpers for test results.
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
