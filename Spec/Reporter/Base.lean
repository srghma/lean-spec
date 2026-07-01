module
public import Spec.Events

@[expose] public section

namespace Spec.Reporter.Base

set_option autoImplicit false

/-! Minimal ANSI styling shared by reporters. -/

def esc : String := "\x1b["
def reset : String := esc ++ "0m"

def code (useColor : Bool) (n : String) (s : String) : String :=
  if useColor then esc ++ n ++ "m" ++ s ++ reset else s

def bold (useColor : Bool) (s : String) : String := code useColor "1" s
def dim (useColor : Bool) (s : String) : String := code useColor "2" s
def red (useColor : Bool) (s : String) : String := code useColor "31" s
def green (useColor : Bool) (s : String) : String := code useColor "32" s
def yellow (useColor : Bool) (s : String) : String := code useColor "33" s
def magenta (useColor : Bool) (s : String) : String := code useColor "35" s
def cyan (useColor : Bool) (s : String) : String := code useColor "36" s

def indent (depth : Nat) : String := String.ofList (List.replicate (depth * 2) ' ')

def pluralize (s : String) (n : Nat) : String := if n == 1 then s else s ++ "s"

structure Summary where
  passed : Nat := 0
  failed : Nat := 0
  pending : Nat := 0

def summarize (results : Array Spec.ItemResult) : Summary :=
  results.foldl (init := {}) fun acc r =>
    match r.outcome with
    | .success => { acc with passed := acc.passed + 1 }
    | .failure _ => { acc with failed := acc.failed + 1 }
    | .pending => { acc with pending := acc.pending + 1 }

/-- Default summary block reused by the console/spec reporters. -/
def defaultSummary (useColor : Bool) (results : Array Spec.ItemResult) : IO Unit := do
  let s := summarize results
  let total := s.passed + s.failed
  IO.println ""
  IO.println (bold useColor "Summary")
  let amount := s!"{s.passed}/{total} {pluralize "test" total} passed"
  IO.println (if s.failed > 0 then red useColor amount else dim useColor amount)
  if s.pending > 0 then
    IO.println (yellow useColor s!"{s.pending} {pluralize "test" s.pending} pending")
  IO.println ""

end Spec.Reporter.Base
