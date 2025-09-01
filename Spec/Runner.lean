import Pipes
import Spec.Event
import Spec.Config
import Spec.Tree
import Spec.Basic
import Spec.Result
import Spec.Summary
import Spec.Timeout
import Pipes.Concurrent.MergeProducers
import Std.Time

-- Type aliases matching the PureScript definitions
abbrev TestEvents := Producer Spec.Event IO (Array (Spec.Tree Spec.TestLocator PEmpty Spec.Result))
abbrev Reporter := Pipe Spec.Event Spec.Event IO (Array (Spec.Tree Spec.TestLocator PEmpty Spec.Result))

-- Data structure for test with path information
structure TestWithPath where
  test : Spec.SpecTree IO Unit
  path : Spec.Path

def _run.executeExample (config : Spec.Config) (action : BaseIO (Except IO.Error Unit)) : BaseIO (Task Spec.Result) := do
  let t ← executeWithMTimeoutAndMeasureTime config.timeout action
  BaseIO.mapTask (t := t) fun
    | MaybeTimedOut.timedOut => return Spec.Result.failure_timeouted
    | MaybeTimedOut.notTimedOut (duration, .error e) => do
      let speed := Spec.Speed.speedOf config.slow duration
      return (Spec.Result.failure e)
    | MaybeTimedOut.notTimedOut (duration, .ok result) => do
      let speed := Spec.Speed.speedOf config.slow duration
      pure (Spec.Result.success speed duration)

def _run.runItem
  (keepRunningRef : IO.Ref Bool)
  (testWithPath : TestWithPath) :
  Producer Spec.Event IO (Array (Spec.Tree Spec.TestLocator PEmpty Spec.Result)) := do
  let keepRunning : Bool ← keepRunningRef.get
  let path := testWithPath.path

  let execution := if Spec.Tree.isAllParallelizable testWithPath.test then Spec.Execution.parallel else Spec.Execution.sequential

  match testWithPath.test with
  | Spec.Tree.leaf pathName (some item) => do
    if keepRunning then do
      Proxy.yield $ Spec.Event.test execution (pathName: Spec.TestLocator)
      let result ← executeExample item.example
      match result with
      | Spec.Result.failure _ =>
          if config.failFast then keepRunningRef.set false else pure ()
      | _ => pure ()
      Proxy.yield $ Spec.Event.testEnd pathName result
      pure #[Spec.Tree.Leaf pathName.snd (some result)]
    else
      pure #[Spec.Tree.Leaf pathName.snd none]

  | Spec.Tree.leaf pathName none => do
    if keepRunning then
      Proxy.yield $ Spec.Event.pending pathName
    pure #[Spec.Tree.Leaf pathName.snd none]

  | Spec.Tree.node (Sum.inr cleanup) children => do
    let results ← loop keepRunningRef children
    cleanup ()
    pure results

  | Spec.Tree.node (Sum.inl pathName) children => do
    if keepRunning then do
      Proxy.yield $ Spec.Event.suite execution pathName
    let results ← loop keepRunningRef children
    if keepRunning then
      Proxy.yield $ Spec.Event.suiteEnd pathName
    pure #[Spec.Tree.Node (Sum.inl pathName.snd) results]

def _run.loop (keepRunningRef : IO.Ref Bool) (tests : Array TestWithPath) : Producer Spec.Event IO (Array (Spec.Tree String PEmpty Spec.Result)) := do
  -- Group tests by parallelizability
  let (isP, isNotP) := tests.partition (Spec.Tree.isAllParallelizable ·.test)

  let isP' <- do
    -- Run parallel tests
    let producers := isP.map (runItem keepRunningRef)
    results ← mergeProducers producers
    pure results.flatten

  let isNotP <- do
    -- Run sequential tests
    results ← group.foldlM (fun acc test => do
      result ← runItem keepRunningRef test
      pure (acc ++ result)
    ) #[]
    pure results

  pure results

-- Main runner implementation
def _run (config : Spec.Config) (spec : Spec.SpecT IO Unit IO Unit) : TestEvents := do
  let tests ← Spec.collect spec
  Proxy.yield (Spec.Event.start (Spec.Tree.countTests tests))
  let keepRunningRef ← IO.mkRef true
  let filteredTests := match config.filterTree with
    | some filter => filter tests
    | none => tests
  let results ← loop keepRunningRef (Spec.Tree.annotatedWithPaths filteredTests)
  Proxy.yield (Spec.Event.end results)
  pure results

-- Evaluate spec tree and return action to run tests
def evalSpecT (config : Spec.Config) (reporters : Array Reporter) (spec : Spec.SpecT IO Unit IO Unit) :
  IO (IO (Array (Spec.Tree String PEmpty Spec.Result))) := do
  let runner ← _run config spec
  pure $ do
    let events := reporters.foldl (· >-> ·) runner
    let reportedEvents := Pipes.runEffect $ events //> (fun _ => pure ())

    if config.exit then do
      -- Handle exit logic - simplified for Lean
      try
        let results ← reportedEvents
        let exitCode := if Spec.Summary.successful results then 0 else 1
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
  let action ← evalSpecT Spec.Config.default reporters spec
  _ ← action
  pure ()

def runSpecPure' (config : Spec.Config) (reporters : Array Reporter) (spec : Spec Unit) : IO Unit := do
  let action ← evalSpecT config reporters spec
  _ ← action
  pure ()

-- Main run function
def run (reporters : Array Reporter) (spec : Spec Unit) : IO Unit :=
  runSpecPure' Spec.Config.default reporters spec

def runSpec (reporters : Array Reporter) (spec : Spec Unit) : IO Unit :=
  runSpecPure reporters spec

def runSpec' (config : Spec.Config) (reporters : Array Reporter) (spec : Spec Unit) : IO Unit :=
  runSpecPure' config reporters spec
