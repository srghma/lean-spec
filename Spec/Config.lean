module
import Init.System.IO
public import Spec.Assert

@[expose] public section

namespace Spec

inductive Color where
  | Always
  | Never
  | Default
  deriving Inhabited, Repr, BEq

structure Config where
  /-- Substring filter (`--example`/`-e`). -/
  example? : Option String := none
  /-- Regex-ish filter (`--example-matches`/`-E`). We use substring matching of the
  pattern's literal characters to avoid a regex dependency; callers wanting real
  regexes can pre-filter. -/
  exampleMatches? : Option String := none
  failFast : Bool := false
  onlyFailures : Bool := false
  /-- Default timeout in milliseconds; `none` means no timeout. Per-test
  overrides can replace this value. -/
  timeoutMs : Option Nat := some 30000
  /-- Requested reporter names; empty means caller default. -/
  reporterNames : List String := []
  /-- ANSI color mode; `default` means auto-detect from stdout TTY. -/
  color : Color := .Default
  parallel : Bool := true
  deriving Inhabited

/-- File used to remember which tests failed last run (`--only-failures`). -/
def failuresFile : IO System.FilePath := do
  let base ← _root_.Spec.Assert.initialCwd.get
  return base / ".spec-failures"

def resolveColor (cfg : Config) : IO Bool := do
  match cfg.color with
  | .Always => pure true
  | .Never => pure false
  | .Default => do
    let out ← IO.getStdout
    return (← out.isTty)

end Spec
end
