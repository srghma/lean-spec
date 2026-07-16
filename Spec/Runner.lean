module
import Init.System.IO
public import Std.Sync.Mutex
public import Spec.Config
public import Spec.ArgsParser
public import Spec.Events
public import Spec.DuplicateNames
public import Spec.CTestLikeState
public import Spec.ParallelQueue

open IO

@[expose] public section

namespace Spec

/-- Run an action with an optional timeout (ms), returning its outcome and
duration. Uses a task + polling so we don't depend on a particular async API. -/
def runLeaf (cfg : Config) (timingStats? : Option Speed.HistoricalTimingStats)
    (previousTiming? : Option Timing) (queueIndex queueCount : Nat) (l : Leaf Unit) : IO ItemResult := do
  match l.kind with
  | .inr () =>
    return { path := l.path, name := l.name, outcome := .pending, durationMs := 0 }
  | .inl action =>
    let startMs ← IO.monoMsNow
    let task ← IO.asTask (prio := .dedicated) do
      let outcome ← try action (); pure Outcome.success
        catch e => pure (Outcome.failure (toString e))
      pure outcome
    let timeoutMs? := l.timeoutMs?.orElse (fun _ => cfg.timeoutMs)
    let outcome ← match timeoutMs? with
      | none =>
        match (← IO.wait task) with
        | .ok o => pure o
        | .error e => pure (Outcome.failure (toString e))
      | some ms =>
        let deadline := startMs + ms
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
    let stopMs ← IO.monoMsNow
    let durationMs := stopMs - startMs
    return {
      path := l.path
      name := l.name
      outcome
      durationMs
      previousDurationMs? := previousTiming?.map (·.costMs)
      speed? := some <| Speed.classify timingStats? (previousTiming?.map (·.costMs)) durationMs
      queueIndex
      queueCount
      nonParallel := !l.parallel }

/-- Run a flattened, filtered list of leaves, dispatching to reporters.
`reporters` are already built (state allocated). Per-item reporting is
serialized through `lock` so parallel output is not interleaved. -/
def runLeaves (cfg : Config) (reporters : List Reporter)
    (timingStats? : Option Speed.HistoricalTimingStats) (previousSuiteDurationMs? : Option Nat)
    (leaves : Array ScheduledLeaf) : IO (Array ItemResult × Nat) := do
  for r in reporters do r.start leaves.size
  let suiteStart ← IO.monoMsNow
  let lock ← Std.BaseMutex.new
  let report (res : ItemResult) : IO Unit := do
    lock.lock
    try
      for r in reporters do r.reportItem res
    finally
      lock.unlock
  let mut results : Array ItemResult := #[]
  if cfg.parallel && !cfg.failFast then
    let queue ← ParallelQueue.create leaves
    let workers ← (Array.range queue.queueCount).mapM fun queueIndex => IO.asTask (prio := .dedicated) do
      let mut queueResults := #[]
      let mut running := true
      while running do
        match ← queue.takeParallel? with
        | none => running := false
        | some scheduled =>
          let res ← runLeaf cfg timingStats? scheduled.timing? queueIndex queue.queueCount scheduled.leaf
          report res
          queueResults := queueResults.push res
      return queueResults
    for worker in workers do
      match (← IO.wait worker) with
      | .ok queueResults => results := results ++ queueResults
      | .error e =>
        results := results.push {
          path := #[]
          name := "<queue>"
          outcome := .failure (toString e)
          durationMs := 0 }
    for scheduled in queue.sequentialLeaves do
      let res ← runLeaf cfg timingStats? scheduled.timing? 0 queue.queueCount scheduled.leaf
      report res
      results := results.push res
  else
    for scheduled in leaves do
      let res ← runLeaf cfg timingStats? scheduled.timing? 0 1 scheduled.leaf
      report res
      results := results.push res
      if cfg.failFast && isFailure res.outcome then
        break
  let suiteDurationMs := (← IO.monoMsNow) - suiteStart
  for r in reporters do r.reportSummary results previousSuiteDurationMs? suiteDurationMs
  return (results, suiteDurationMs)

def runSpecWith (cfg : Config) (reporters : List ReporterBuilder) (spec : Spec) : IO Bool := do
  let (_, trees) := spec.run #[]
  let useColor ← resolveColor cfg
  if let some warning := SpecTree.duplicateNamesWarning trees then
    IO.eprintln (Reporter.Base.yellow useColor warning)
  let trees := trees.map SpecTree.cacheOnly
  let globalHasOnly := trees.any SpecTreeWithCachedOnly.hasOnly
  let leaves := trees.foldl (init := #[]) fun leaves tree =>
    flattenCachedOnlyInto globalHasOnly false cfg.timeoutMs #[] tree leaves
  let state ← loadLastRunState useColor
  let failedNames := if cfg.onlyFailures then state.failures else (∅ : Std.TreeSet String)
  let timingStats? := Speed.historicalTimingStats? <|
    state.timings.toList.toArray.map (·.2.costMs)
  let selected := orderScheduledByTiming <| leaves.filterMap fun leaf =>
    let fullName := formatSpecName leaf.path leaf.name
    if matchesFilters cfg failedNames fullName leaf then
      some { leaf, failed := failedNames.contains fullName, timing? := timingFor state.timings fullName }
    else none
  let built ← reporters.mapM (fun mk => mk useColor)
  let (results, suiteDurationMs) ← runLeaves cfg built timingStats?
    (state.suiteTiming?.map (·.costMs)) selected
  saveLastRunState useColor state results suiteDurationMs
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
