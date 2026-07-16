module

public import Spec.StateFile.CommonUtils

@[expose] public section

namespace Spec

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

def withoutLineEnding (line : String) : String :=
  let line := if line.back == '\n' then line.dropEnd 1 |>.copy else line
  if line.back == '\r' then line.dropEnd 1 |>.copy else line

def loadLastRunState (useColor : Bool) : IO LastRunState := do
  let file ← lastRunStateFile
  let handle ← try IO.FS.Handle.mk file .read catch _ => return emptyLastRunState
  let mut state := emptyLastRunState
  let mut failureCollisions := Std.HashSet.emptyWithCapacity
  let mut timingCollisions := Std.HashSet.emptyWithCapacity
  let mut phase := 0
  let mut readAny := false
  let mut reachedEof := false
  while !reachedEof do
    let rawLine ← try handle.getLine catch _ => return emptyLastRunState
    if rawLine.isEmpty then
      reachedEof := true
      continue
    readAny := true
    let line := withoutLineEnding rawLine
    if line == "---" then
      if phase == 2 then invalidStateFile useColor file
      phase := phase + 1
    else unless line.isEmpty do
      if phase == 1 then
        if state.failures.contains line then
          failureCollisions := failureCollisions.insert line
        state := { state with failures := state.failures.insert line }
      else if phase == 0 then
        match parseTimingLine line with
        | some (name, timing) =>
          if state.timings.contains name then
            timingCollisions := timingCollisions.insert name
          state := { state with timings := state.timings.insert name timing }
        | none => invalidStateFile useColor file
      else
        match parseTimingLine line with
        | some ("total", timing) => state := { state with suiteTiming? := some timing }
        | _ => invalidStateFile useColor file
  if !readAny then
    IO.eprintln (Reporter.Base.yellow useColor s!"⚠️ Test file {file} is present but empty; something seems to have gone wrong, but we will continue running tests.")
    return emptyLastRunState
  unless phase > 0 do invalidStateFile useColor file
  warnStateFileCollisions useColor file failureCollisions timingCollisions
  return state

end Spec
end
