module
public import Spec.Reporter.Base
public import Spec.Events

@[expose] public section

namespace Spec.Reporter.Spec

open Spec.Reporter.Base

/-- Spec reporter: indented suite tree with a checkmark / numbered failure / dash
per item, plus speed annotation in ms for slow tests. -/
def specReporter : ReporterBuilder := fun useColor => do
  let numFailures ← IO.mkRef (0 : Nat)
  let lastPath ← IO.mkRef (none : Option (Array String))
  let printSuitesIfNeeded (path : Array String) : IO Unit := do
    let prev ← lastPath.get
    unless prev == some path do
      -- print each suite level (only the parts that changed for tidiness we just
      -- reprint the full path; minimal implementation).
      for i in [0:path.size] do
        IO.println (indent i ++ path[i]!)
      lastPath.set (some path)
  pure
    { reportItem := fun res => do
        printSuitesIfNeeded res.path
        let depth := res.path.size
        match res.outcome with
        | .success =>
          IO.println (indent depth ++ green useColor "✓ " ++ dim useColor res.name ++ duration useColor res)
        | .failure _ =>
          let n ← numFailures.modifyGet fun n => (n + 1, n + 1)
          IO.println (indent depth ++ red useColor s!"{n}) {res.name}" ++ duration useColor res)
        | .pending =>
          IO.println (indent depth ++ cyan useColor ("- " ++ res.name))
    , reportSummary := defaultSummary useColor }

end Spec.Reporter.Spec
