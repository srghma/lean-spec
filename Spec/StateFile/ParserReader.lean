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

def loadLastRunState (useColor : Bool) : IO LastRunState := do
  let file ← lastRunStateFile
  let content ← try IO.FS.readFile file catch _ => return emptyLastRunState
  if content.isEmpty then
    IO.eprintln (Reporter.Base.yellow useColor s!"⚠️ Test file {file} is present but empty; something seems to have gone wrong, but we will continue running tests.")
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
  warnStateFileCollisions useColor file failureCollisions timingCollisions
  return state

end Spec
end
