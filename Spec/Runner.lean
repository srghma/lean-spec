import Pipes
import Pipes.Core
import Spec.Event
import Spec.Config
import Spec.Tree
import Spec.Basic
import Spec.Result
import Spec.Summary
import Spec.Timeout
import Pipes.Concurrent.MergeProducers
import Pipes.Sequential.MergeProducers
import Std.Time

namespace Spec

-- Type aliases matching the PureScript definitions
abbrev TestEvents := Producer Spec.Event IO (Array (Spec.Tree Spec.TestLocator Empty Spec.Result))
abbrev Reporter := Pipe Spec.Event Spec.Event IO (Array (Spec.Tree Spec.TestLocator Empty Spec.Result)) -- TODO: multiple subscribers?

-- Data structure for test with path information
abbrev TestWithPath := Tree TestLocator (ActionWith IO Unit) (Item IO Unit)

def _run.executeExample (config : Spec.Config) (action : BaseIO (Except IO.Error Unit)) : BaseIO (Task Spec.Result) := do
  let t ← executeWithMTimeoutAndMeasureTime config.timeout action
  BaseIO.mapTask (t := t) fun
    | MaybeTimedOut.timedOut => return Spec.Result.failure_timeouted
    | MaybeTimedOut.notTimedOut (duration, r) => do
      let speed := Spec.Speed.speedOf_Int config.slow.toInt duration
      match r with
        | .error e => return Spec.Result.failure speed e
        | .ok _r => return Spec.Result.success speed duration

open Proxy

mutual
def _run.runItem
  (config : Spec.Config)
  (keepRunningRef : IO.Ref Bool)
  (testWithPath : TestWithPath) :
  TestEvents := do
  let keepRunning : Bool ← (keepRunningRef.get : IO Bool)
  let execution := if Spec.Tree.isAllParallelizable testWithPath then Spec.Execution.parallel else Spec.Execution.sequential
  match testWithPath with
  | Spec.Tree.leaf pathName (some item) => do
    if keepRunning then do
      Proxy.yield $ Spec.Event.test execution pathName
      let t ← executeExample config (item.example_ .unit).toBaseIO
      let result := t.get
      if config.failFast && result.isSuccess then
        (keepRunningRef.set false : IO Unit)
      Proxy.yield $ Spec.Event.testEnd pathName result
      pure #[Spec.Tree.leaf pathName (some result)]
    else
      pure #[Spec.Tree.leaf pathName none]

  | Spec.Tree.leaf pathName none => do
    if keepRunning then
      Proxy.yield $ Spec.Event.pending pathName
    pure #[Spec.Tree.leaf pathName none]

  | Spec.Tree.node (Sum.inr cleanup) children => do
    let results ← loop config keepRunningRef children
    cleanup ()
    pure results

  | Spec.Tree.node (Sum.inl pathName) children => do
    if keepRunning then do
      Proxy.yield $ Spec.Event.suite execution pathName
    let results ← loop config keepRunningRef children
    if keepRunning then
      Proxy.yield $ Spec.Event.suiteEnd pathName
    pure #[Spec.Tree.node (Sum.inl pathName) results]

def _run.loop (config : Spec.Config) (keepRunningRef : IO.Ref Bool) (tests : Array TestWithPath) : TestEvents := do
  -- Group tests by parallelizability
  let (isP, isNotP) := tests.partition Spec.Tree.isAllParallelizable

  let isNotP' <- do
    -- Run sequential tests
    let results ← isNotP.foldlM (fun acc test => do
      let result ← runItem config keepRunningRef test
      pure (acc ++ result)
    ) #[]
    pure results

  let isP' <- do
    -- Run parallel tests
    let producers := isP.map (fun x => Producer.EIOtoBaseIO $ runItem config keepRunningRef x)
    let results ← mergeProducers producers
    pure results

  pure results
end

-- Main runner implementation
def _run (config : Spec.Config) (spec : Spec.SpecT IO Unit Id Unit) : TestEvents := do
  let tests := (Spec.collect spec).run
  Proxy.yield (Spec.Event.start (Spec.Tree.countTests tests))
  let keepRunningRef ← IO.mkRef true
  -- let filteredTests: Array (SpecTree IO Unit) := config.filterTree tests
  let filteredTests: Array (SpecTree IO Unit) := config.filterTree tests
  let annotatedTests: Array TestWithPath := Spec.Tree.annotateWithPaths filteredTests
  let results ← _run.loop config keepRunningRef annotatedTests
  Proxy.yield (Spec.Event.end_ results)
  pure results

-- Evaluate spec tree and return action to run tests
def evalSpecT
  (config : Spec.Config)
  (reporters : Array Reporter)
  (spec : Spec.SpecT IO Unit Id Unit) :
  IO (Array (Spec.Tree TestLocator Empty Spec.Result)) := do
  let runner := _run config spec
  let events := reporters.foldl (· >-> ·) runner
  let events_withEracedE := events //> (fun (_event : Event) => Proxy.Pure (b := PEmpty) (b' := PUnit) ())
  let reportedEvents: IO (Array (Spec.Tree TestLocator Empty Spec.Result)) := Proxy.runEffect events_withEracedE

  if config.exit then do
    -- Handle exit logic - simplified for Lean
    try
      let results ← reportedEvents
      let exitCode := if Spec.Summary.successful? results then 0 else 1
      -- In a real implementation, you'd call System.exit here
      IO.println s!"Exit code: {exitCode}"
      pure results
    catch e =>
      IO.println s!"Error: {e}"
      throw e
  else
    reportedEvents

-- Simplified runner functions
def runSpecPure (reporters : Array Reporter) (spec : Spec Unit) : IO Unit := do
  let _action ← evalSpecT Spec.Config.default reporters spec
  pure ()

def runSpecPure' (config : Spec.Config) (reporters : Array Reporter) (spec : Spec Unit) : IO Unit := do
  let _action ← evalSpecT config reporters spec
  pure ()

-- Main run function
def run (reporters : Array Reporter) (spec : Spec Unit) : IO Unit :=
  runSpecPure' Spec.Config.default reporters spec

def runSpec (reporters : Array Reporter) (spec : Spec Unit) : IO Unit :=
  runSpecPure reporters spec

def runSpec' (config : Spec.Config) (reporters : Array Reporter) (spec : Spec Unit) : IO Unit :=
  runSpecPure' config reporters spec
