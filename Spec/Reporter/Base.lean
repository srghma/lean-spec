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
def blue (useColor : Bool) (s : String) : String := code useColor "34" s
def green (useColor : Bool) (s : String) : String := code useColor "32" s
def yellow (useColor : Bool) (s : String) : String := code useColor "33" s
def magenta (useColor : Bool) (s : String) : String := code useColor "35" s
def cyan (useColor : Bool) (s : String) : String := code useColor "36" s

def padThreeDigits (value : Nat) : String :=
  if value < 10 then "00" ++ toString value
  else if value < 100 then "0" ++ toString value
  else toString value

def formatDurationMsParts (milliseconds : Nat) : String × String :=
  let formatScaled (unitMs : Nat) (unit : String) : String × String :=
    let whole := milliseconds / unitMs
    let fraction := (milliseconds % unitMs) * 1000 / unitMs
    (s!"{whole}.{padThreeDigits fraction}", unit)
  if milliseconds < 1000 then
    (toString milliseconds, "ms")
  else if milliseconds < 60000 then
    formatScaled 1000 "s"
  else if milliseconds < 3600000 then
    formatScaled 60000 "min"
  else
    formatScaled 3600000 "h"

def formatDurationMs (milliseconds : Nat) : String :=
  let (value, unit) := formatDurationMsParts milliseconds
  value ++ unit

def duration (useColor : Bool) (res : Spec.ItemResult) : String :=
  let timing := formatDurationMs res.durationMs
  let queue := if res.nonParallel then
    s!"queue 1/{res.queueCount} for non-parallel tests"
  else
    s!"queue {res.queueIndex + 1}/{res.queueCount}"
  let text := s!" ({timing}, {queue})"
  match res.speed? with
  | some .fast => blue useColor text
  | some .medium => yellow useColor text
  | some .slow => red useColor text
  | none => dim useColor text

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
def defaultSummary (useColor : Bool) (results : Array Spec.ItemResult)
    (previousSuiteDurationMs? : Option Nat) (suiteDurationMs : Nat) : IO Unit := do
  let s := summarize results
  let total := s.passed + s.failed
  IO.println ""
  IO.println (bold useColor "Summary")
  let amount := s!"{s.passed}/{total} {pluralize "test" total} passed"
  IO.println (if s.failed > 0 then red useColor amount else dim useColor amount)
  if s.pending > 0 then
    IO.println (yellow useColor s!"{s.pending} {pluralize "test" s.pending} pending")
  let mut previous := #[]
  let mut current := #[]
  for result in results do
    match result.outcome with
    | .pending => pure ()
    | _ =>
      current := current.push result.durationMs
      if let some durationMs := result.previousDurationMs? then
        previous := previous.push durationMs
  unless current.isEmpty do
    IO.println ""
    IO.println (bold useColor "Timings")
    let colorCompared (render : Nat → String) (value other : Nat) : String :=
      if value < other then blue useColor (render value)
      else if value > other then red useColor (render value)
      else render value
    match Speed.historicalTimingStats? previous, Speed.historicalTimingStats? current with
    | some before, some now =>
      IO.println ("  before (" ++ colorCompared toString previous.size current.size ++ " tests)")
      let beforeTotal := match previousSuiteDurationMs? with
        | some total => colorCompared formatDurationMs total suiteDurationMs
        | none => "<no data>"
      IO.println ("    total: " ++ beforeTotal)
      IO.println ("    mean: " ++ colorCompared formatDurationMs before.meanMs now.meanMs)
      IO.println ("    variation: " ++ colorCompared formatDurationMs before.standardDeviationMs now.standardDeviationMs)
      IO.println ("  now (" ++ colorCompared toString current.size previous.size ++ " tests)")
      let nowTotal := match previousSuiteDurationMs? with
        | some total => colorCompared formatDurationMs suiteDurationMs total
        | none => formatDurationMs suiteDurationMs
      IO.println ("    total: " ++ nowTotal)
      IO.println ("    mean: " ++ colorCompared formatDurationMs now.meanMs before.meanMs)
      IO.println ("    variation: " ++ colorCompared formatDurationMs now.standardDeviationMs before.standardDeviationMs)
    | _, _ =>
      IO.println "  before: no matching history"
      IO.println (s!"  now ({current.size} tests)")
      IO.println (s!"    total: {formatDurationMs suiteDurationMs}")
  IO.println ""

end Spec.Reporter.Base
