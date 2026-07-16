module
import Init.System.IO
public import Std.Sync.Mutex
public import Spec.Config
public import Spec.ArgsParser
public import Spec.Events
public import Spec.CTestLikeState

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
    let timeoutMs? := l.timeoutMs?.orElse (fun _ => cfg.timeoutMs)
    let outcome ← match timeoutMs? with
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

def runSpecWith (cfg : Config) (reporters : List ReporterBuilder) (spec : Spec) : IO Bool := do
  let (_, trees) := spec.run #[]
  let globalHasOnly := trees.any SpecTree.hasOnly
  let leaves := trees.foldl (init := #[]) fun acc t =>
    acc ++ flatten globalHasOnly false cfg.timeoutMs #[] t
  let useColor ← resolveColor cfg
  let state ← loadLastRunState useColor
  let failedNames := if cfg.onlyFailures then state.failures else Std.HashSet.emptyWithCapacity
  let selected := orderByTiming failedNames state.timings (leaves.filter (matchesFilters cfg failedNames))
  let built ← reporters.mapM (fun mk => mk useColor)
  let results ← runLeaves cfg built selected
  saveLastRunState useColor state results
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
