module

public import Spec.StateFile.CommonUtils

@[expose] public section

namespace Spec

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
      let name := formatSpecName result.path result.name
      unless seenNames.contains name do
        seenNames := seenNames.insert name
        match timingFor timings name with
        | some old =>
          let costMs := (old.previousRuns * old.costMs + result.durationMs) / (old.previousRuns + 1)
          timings := timings.insert name { old with previousRuns := old.previousRuns + 1, costMs }
        | none =>
          timings := timings.insert name { previousRuns := 1, costMs := result.durationMs }
    | .failure _ =>
      let name := formatSpecName result.path result.name
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
      IO.eprintln <| Reporter.Base.yellow useColor
        s!"⚠️ Could not save .lean-spec-timings-and-last-failures: {error}. Continuing without updating it."

end Spec
end
