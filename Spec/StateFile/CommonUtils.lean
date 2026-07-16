module

public import Spec.Reporter.Base
public import Spec.StateFile.Types

@[expose] public section

namespace Spec

open Spec.Reporter.Base

def lastRunStateFile : IO System.FilePath := do
  let base ← _root_.Spec.Assert.initialCwd.get
  return base / ".lean-spec-timings-and-last-failures"

def invalidStateFile (useColor : Bool) (file : System.FilePath) : IO α := do
  let message := s!"❌ Invalid test file {file}; it seems something went wrong. Remove that file first, then run the tests again."
  IO.eprintln (red useColor message)
  throw <| IO.userError message

def warnStateFileCollisions (useColor : Bool) (file : System.FilePath)
    (failureCollisions timingCollisions : Std.HashSet String) : IO Unit := do
  unless failureCollisions.isEmpty && timingCollisions.isEmpty do
    let failures := failureCollisions.toList.mergeSort (· < ·)
    let timings := timingCollisions.toList.mergeSort (· < ·)
    IO.eprintln <| yellow useColor s!"⚠️ There were collisions of test names in state file {file}; last value wins."
    unless failures.isEmpty do
      IO.eprintln <| yellow useColor s!"  In failures: {String.intercalate ", " failures}"
    unless timings.isEmpty do
      IO.eprintln <| yellow useColor s!"  In timings: {String.intercalate ", " timings}"

end Spec
end
