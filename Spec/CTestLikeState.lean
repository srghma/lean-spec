module

public import Spec.Reporter.Base
public import Spec.Events
public import Std.Data.HashMap.Basic
public import Std.Data.HashSet.Basic

@[expose] public section

namespace Spec

open Spec.Reporter.Base

structure Timing where
  previousRuns : Nat
  costMs : Nat

abbrev Timings := Std.HashMap String Timing

structure LastRunState where
  failures : Std.HashSet String
  timings : Timings

def emptyLastRunState : LastRunState :=
  { failures := Std.HashSet.emptyWithCapacity, timings := Std.HashMap.emptyWithCapacity }

def lastRunStateFile : IO System.FilePath := do
  let base ← _root_.Spec.Assert.initialCwd.get
  return base / ".lean-spec-timings-and-last-failures"

def invalidStateFile (useColor : Bool) (file : System.FilePath) : IO α := do
  let message := s!"❌ Invalid test file {file}; it seems something went wrong. Remove that file first, then run the tests again."
  IO.eprintln (red useColor message)
  throw <| IO.userError message

def warnCollisions (useColor : Bool) (file : System.FilePath)
    (failureCollisions timingCollisions : Std.HashSet String) : IO Unit := do
  unless failureCollisions.isEmpty && timingCollisions.isEmpty do
    let failures := failureCollisions.toList.mergeSort (· < ·)
    let timings := timingCollisions.toList.mergeSort (· < ·)
    IO.eprintln <| yellow useColor s!"⚠️ There were collisions of test names in state file {file}; last value wins."
    unless failures.isEmpty do
      IO.eprintln <| yellow useColor s!"  In failures: {String.intercalate ", " failures}"
    unless timings.isEmpty do
      IO.eprintln <| yellow useColor s!"  In timings: {String.intercalate ", " timings}"

def parseCostMs (value : String) : Option Nat :=
  match value.splitOn "." with
  | [whole] => whole.toNat?
  | whole :: _ => whole.toNat?
  | _ => none

def parseTimingLine (line : String) : Option (String × Timing) :=
  let parts := line.splitOn " " |>.filter (!·.isEmpty) |>.toArray
  if parts.size < 3 then
    none
  else
    let cost := parts.back!
    let previousRuns := parts[parts.size - 2]!
    let name := String.intercalate " " (parts.toList.take (parts.size - 2))
    match previousRuns.toNat?, parseCostMs cost with
    | some previousRuns, some costMs => some (name, { previousRuns, costMs })
    | _, _ => none

def loadLastRunState (useColor : Bool) : IO LastRunState := do
  let file ← lastRunStateFile
  let content ← try IO.FS.readFile file catch _ => return emptyLastRunState
  if content.isEmpty then
    IO.eprintln (yellow useColor s!"⚠️ Test file {file} is present but empty; something seems to have gone wrong, but we will continue running tests.")
    return emptyLastRunState
  let mut state := emptyLastRunState
  let mut failureCollisions := Std.HashSet.emptyWithCapacity
  let mut timingCollisions := Std.HashSet.emptyWithCapacity
  let mut sawSeparator := false
  for line in content.splitOn "\n" do
    if line == "---" then
      if sawSeparator then invalidStateFile useColor file
      sawSeparator := true
    else unless line.isEmpty do
      if sawSeparator then
        if state.failures.contains line then
          failureCollisions := failureCollisions.insert line
        state := { state with failures := state.failures.insert line }
      else
        match parseTimingLine line with
        | some (name, timing) =>
          if state.timings.contains name then
            timingCollisions := timingCollisions.insert name
          state := { state with timings := state.timings.insert name timing }
        | none => invalidStateFile useColor file
  unless sawSeparator do invalidStateFile useColor file
  warnCollisions useColor file failureCollisions timingCollisions
  return state

def timingFor (timings : Timings) (name : String) : Option Timing :=
  timings[name]?

def failedFirst (failedNames : Std.HashSet String) (a b : Leaf Unit) : Bool :=
  let aFailed := failedNames.contains a.fullName
  let bFailed := failedNames.contains b.fullName
  aFailed && !bFailed

/-- CTest-style order: previous failures first, then descending historical cost. -/
def orderByTiming (failedNames : Std.HashSet String) (timings : Timings)
    (leaves : Array (Leaf Unit)) : Array (Leaf Unit) :=
  leaves.mergeSort fun a b =>
    if failedFirst failedNames a b then
      true
    else if failedFirst failedNames b a then
      false
    else
      match timingFor timings a.fullName, timingFor timings b.fullName with
      | some aTiming, some bTiming => aTiming.costMs > bTiming.costMs
      | some _, none => true
      | none, some _ => false
      | none, none => false

def sortedTimings (timings : Timings) : List (String × Timing) :=
  timings.toList.mergeSort (fun a b => a.1 < b.1)

def saveLastRunState (useColor : Bool) (previous : LastRunState) (results : Array ItemResult) : IO Unit := do
  let mut failed := Std.HashSet.emptyWithCapacity
  let mut timings := previous.timings
  let mut seenNames := Std.HashSet.emptyWithCapacity
  for result in results do
    match result.outcome with
    | .pending => pure ()
    | .success =>
      let name := String.intercalate " » " (result.path.toList ++ [result.name])
      unless seenNames.contains name do
        seenNames := seenNames.insert name
        match timingFor timings name with
        | some old =>
          let costMs := (old.previousRuns * old.costMs + result.durationMs) / (old.previousRuns + 1)
          timings := timings.insert name { old with previousRuns := old.previousRuns + 1, costMs }
        | none =>
          timings := timings.insert name { previousRuns := 1, costMs := result.durationMs }
    | .failure _ =>
      let name := String.intercalate " » " (result.path.toList ++ [result.name])
      unless seenNames.contains name do
        seenNames := seenNames.insert name
        failed := failed.insert name
  let timingLines := (sortedTimings timings).map
    fun (name, timing) => s!"{name} {timing.previousRuns} {timing.costMs}.000000"
  let failureLines := failed.toList.mergeSort (· < ·)
  let lines := if timingLines.isEmpty && failureLines.isEmpty then []
    else timingLines ++ ["---"] ++ failureLines
  let content := String.intercalate "\n" lines
  unless content.isEmpty do
    try
      let file ← lastRunStateFile
      IO.FS.writeFile file content
    catch error =>
      IO.eprintln <| yellow useColor
        s!"⚠️ Could not save .lean-spec-timings-and-last-failures: {error}. Continuing without updating it."

end Spec
end
