module
public import Spec.Reporter.Base
public import Spec.Events

@[expose] public section

namespace Spec.Reporter.Console

open Spec.Reporter.Base

/-- Console reporter: prints each suite path once (when it changes), then a line
per test under it. Per-item output is already serialized by the runner, so this
stays readable under parallel execution. -/
def consoleReporter : ReporterBuilder := fun useColor => do
  let lastPath ← IO.mkRef (none : Option (Array String))
  let printHeaderIfNeeded (path : Array String) : IO Unit := do
    let prev ← lastPath.get
    unless prev == some path do
      IO.println (bold useColor (magenta useColor (String.intercalate " » " path.toList)))
      lastPath.set (some path)
  pure
    { reportItem := fun res => do
        printHeaderIfNeeded res.path
        match res.outcome with
        | .success =>
          IO.println ("  " ++ green useColor "✓ " ++ dim useColor res.name ++ duration useColor res)
        | .failure err =>
          IO.println ("  " ++ red useColor ("✗ " ++ res.name) ++ duration useColor res ++ ":")
          IO.println ""
          IO.println ("  " ++ red useColor err)
        | .pending =>
          IO.println ("  " ++ cyan useColor ("~ " ++ res.name))
    , reportSummary := defaultSummary useColor }
