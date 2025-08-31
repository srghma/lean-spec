import Spec.Tree
import Spec.Result
import Spec.Event
import Spec.Style
import Spec.Console
import Spec.Summary
import Std.Data.HashMap
import Pipes

namespace Spec.Reporter.Base

open Spec

-- Running item state for tracking test execution
inductive RunningItem where
  | runningTest : Option Result → RunningItem
  | runningPending : RunningItem
  | runningSuite : Bool → RunningItem
deriving Repr, BEq, Hashable

-- Reporter state type
structure ReporterState where
  runningItems : Std.HashMap TestLocator RunningItem
  output : String
deriving Repr

-- Reporter monad - simplified without pipes
abbrev ReporterM := StateM ReporterState

-- Helper to write output
def tellLn (s : String) : ReporterM Unit := do
  let state ← get
  set { state with output := state.output ++ s ++ "\n" }

-- Print test failures
partial def printFailures (results : Array (Tree (Name × Path) Empty Result)) : ReporterM Unit := do
  let mut failureCount := 0
  for tree in results do
    match tree with
    | Tree.node (Sum.inr v) _ => nomatch v
    | Tree.node (Sum.inl _) children => printFailures children
    | Tree.leaf (name, path) optRes =>
      match optRes with
      | some (.failure err) => do
          failureCount := failureCount + 1
          let mut label := Tree.parentSuiteName path ++ #[name]
          let label' := label.map Name.toString
          let label'' := String.intercalate " " label'.toList
          tellLn s!"{failureCount}) {label''}"
          tellLn $ Style.styled Style.red (Style.indent 2 ++ err.toString)
      | _ => pure ()

-- Default summary printing
def defaultSummary (results : Array (Tree (Name × Path) Empty Result)) : ReporterM Unit := do
  let summary := Summary.summarize results
  if summary.passed > 0 then tellLn $ Style.styled Style.green s!"{summary.passed} passing"
  if summary.pending > 0 then tellLn $ Style.styled Style.cyan s!"{summary.pending} pending"
  if summary.failed > 0 then tellLn $ Style.styled Style.red s!"{summary.failed} failed"
  tellLn ""
  printFailures results

-- Check if a running item is finished
def RunningItem.isFinished : RunningItem → Bool
  | .runningPending => true
  | .runningTest x => x.isSome
  | .runningSuite finished => finished

-- Base reporter configuration
structure ReporterConfig (s : Type) where
  getRunningItems : s → Std.HashMap TestLocator RunningItem
  putRunningItems : Std.HashMap TestLocator RunningItem → s → s
  printFinishedItem : TestLocator → RunningItem → StateT s (WriterT String IO) Unit
  update : Event → StateT s (WriterT String IO) Unit

-- needed to derive [Monad (WriterT String IO)]
local instance : EmptyCollection String where
  emptyCollection := ""

local instance : Append String where
  append a b := a ++ b

def ReporterConfig.printFinishedItems
  (self : ReporterConfig s)
  (items : Std.HashMap TestLocator RunningItem)
  : StateT s (WriterT String IO) PUnit :=
  items.forM fun loc item => self.printFinishedItem loc item

def modifyRunningItems
  (config : ReporterConfig s)
  (f : Std.HashMap TestLocator RunningItem → Std.HashMap TestLocator RunningItem) :
  StateT s (WriterT String IO) PUnit := do
    let state <- StateT.get
    let currentItems := config.getRunningItems state
    let nextItems := f currentItems
    let allFinished : Bool := nextItems.values.all RunningItem.isFinished

    let finalItems := if allFinished then Std.HashMap.emptyWithCapacity else nextItems
    StateT.set (config.putRunningItems finalItems state)

    if allFinished then
      ReporterConfig.printFinishedItems config nextItems

-- Default update function
def defaultUpdate (config : ReporterConfig s) (event : Event) :
    StateT s (WriterT String IO) PUnit := do
  -- Base update logic
  match event with
  | Event.suite Execution.sequential _ => pure ()
  | Event.suite Execution.parallel loc => do
    modifyRunningItems config (·.insert loc (RunningItem.runningSuite false))
  | Event.suiteEnd loc => do
    modifyRunningItems config (·.alter loc fun
      | some (RunningItem.runningSuite _) => some (RunningItem.runningSuite true)
      | other => other)
  | Event.test Execution.sequential _ => pure ()
  | Event.test Execution.parallel loc => do
    modifyRunningItems config (·.insert loc (RunningItem.runningTest none))
  | Event.testEnd loc result => do
    let state ← StateT.get
    let runningItems := config.getRunningItems state
    match runningItems.get? loc with
    | some (RunningItem.runningTest _) =>
      modifyRunningItems config (·.insert loc (RunningItem.runningTest (some result)))
    | _ => pure ()
  | Event.pending loc => do
    let state ← StateT.get
    let runningItems := config.getRunningItems state
    unless runningItems.isEmpty do
      modifyRunningItems config (·.insert loc RunningItem.runningPending)
  | Event.end _ => pure ()
  | Event.start _ => pure ()

  -- Call custom update
  config.update event

def defaultReporter
  (initialState : s)
  (onEvent : Event → StateT s (WriterT String Id) Unit)
  : Reporter :=
  fun events => do
    let (_, output) ← events.foldlM (init := (initialState, "")) fun (state, _) event => do
      let ((_, newState), log) := WriterT.run ((onEvent event).run state)
      pure (newState, log)
    pure output

end Spec.Reporter.Base
