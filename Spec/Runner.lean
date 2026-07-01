module
import Init.System.IO
public import Std.Sync.Mutex
public import Spec.Config
public import Spec.ArgsParser
public import Spec.Events

open IO

@[expose] public section

namespace Spec

/-- Run an action with an optional timeout (ms), returning its outcome and
duration. Uses a task + polling so we don't depend on a particular async API. -/
def runLeaf (cfg : Config) (l : Leaf Unit) : IO ItemResult := do
  match l.kind with
  | .inr () =>
    return { path := l.path, name := l.name, outcome := .pending, durationMs := 0 }
  | .inl action =>
    let start ← IO.monoMsNow
    let task ← IO.asTask (prio := .dedicated) do
      try action (); pure (Outcome.success)
      catch e => pure (Outcome.failure (toString e))
    let outcome ← match cfg.timeoutMs with
      | none =>
        match (← IO.wait task) with
        | .ok o => pure o
        | .error e => pure (Outcome.failure (toString e))
      | some ms =>
        let deadline := start + ms
        let mut res : Option Outcome := none
        while res.isNone do
          if (← IO.hasFinished task) then
            res := some (match task.get with
              | .ok o => o
              | .error e => Outcome.failure (toString e))
          else if (← IO.monoMsNow) > deadline then
            IO.cancel task
            res := some (Outcome.failure s!"timed out after {ms}ms")
          else
            IO.sleep 1
        pure res.get!
    let stop ← IO.monoMsNow
    return { path := l.path, name := l.name, outcome, durationMs := stop - start }

def isFailure : Outcome → Bool
  | .failure _ => true
  | _ => false

/-- Run a flattened, filtered list of leaves, dispatching to reporters.
`reporters` are already built (state allocated). Per-item reporting is
serialized through `lock` so parallel output is not interleaved. -/
def runLeaves (cfg : Config) (reporters : List Reporter) (leaves : Array (Leaf Unit)) :
    IO (Array ItemResult) := do
  for r in reporters do r.start leaves.size
  let lock ← Std.BaseMutex.new
  let report (res : ItemResult) : IO Unit := do
    lock.lock
    try
      for r in reporters do r.reportItem res
    finally
      lock.unlock
  let mut results : Array ItemResult := #[]
  if cfg.parallel && !cfg.failFast then
    let tasks ← leaves.mapM fun l => IO.asTask (prio := .dedicated) do
      let res ← runLeaf cfg l
      report res
      pure res
    for t in tasks do
      let res ← match (← IO.wait t) with
        | .ok r => pure r
        | .error e => pure { path := #[], name := "<task>", outcome := .failure (toString e), durationMs := 0 }
      results := results.push res
  else
    for l in leaves do
      let res ← runLeaf cfg l
      report res
      results := results.push res
      if cfg.failFast && isFailure res.outcome then
        break
  for r in reporters do r.reportSummary results
  return results

def saveFailures (results : Array ItemResult) : IO Unit := do
  let failed := results.filterMap fun r =>
    if isFailure r.outcome then some (String.intercalate " » " (r.path.toList ++ [r.name])) else none
  unless failed.isEmpty do
    let file ← failuresFile
    IO.FS.writeFile file (String.intercalate "\n" failed.toList)

def loadFailures : IO (Array String) := do
  try
    let file ← failuresFile
    let content ← IO.FS.readFile file
    return content.splitOn "\n" |>.filter (!·.isEmpty) |>.toArray
  catch _ => return #[]

def runSpecWith (cfg : Config) (reporters : List ReporterBuilder) (spec : Spec) : IO Bool := do
  let (_, trees) := spec.run #[]
  let globalHasOnly := trees.any SpecTree.hasOnly
  let leaves := trees.foldl (init := #[]) fun acc t =>
    acc ++ flatten globalHasOnly false #[] t
  let failedNames ← if cfg.onlyFailures then loadFailures else pure #[]
  let selected := leaves.filter (matchesFilters cfg failedNames)
  let useColor ← resolveColor cfg
  let built ← reporters.mapM (fun mk => mk useColor)
  let results ← runLeaves cfg built selected
  saveFailures results
  let anyFailed := results.any (isFailure ·.outcome)
  return anyFailed

def runSpec (args : List String) (reporters : List ReporterBuilder) (spec : Spec) : IO Bool := do
  let cfg := parseArgs args
  runSpecWith cfg reporters spec

def runSpecAndReturnExitCode (args : List String) (reporters : List ReporterBuilder) (spec : Spec) : IO UInt32 := do
  let failed ← runSpec args reporters spec
  return if failed then 1 else 0

end Spec
end
