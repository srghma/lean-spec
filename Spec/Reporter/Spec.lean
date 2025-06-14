-- Spec/Reporter/Spec.lean
import Spec.Reporter.Base
import Spec.Tree
import Spec.Result
import Spec.Event
import Spec.Style
import Spec.Speed
import Spec.Summary

namespace Spec.Reporter.Spec

-- Spec reporter state
structure SpecState where
  runningItems : HashMap TestLocator RunningItem
  numFailures : Nat
deriving Repr

-- Initial state
def initialState : SpecState := {
  runningItems := HashMap.empty,
  numFailures := 0
}

-- Print actions for spec reporter
inductive PrintAction where
  | PrintSuite : String → PrintAction
  | PrintTest : String → Result → PrintAction
  | PrintPending : String → PrintAction
deriving Repr

-- Print function with indentation based on path depth
def print (path : Path) (action : PrintAction) :
    StateT SpecState (WriterT String IO) Unit := do
  match action with
  | PrintAction.PrintSuite name => do
    tell $ indent path ++ name ++ "\n"
  | PrintAction.PrintTest name (Result.Success speed duration) => do
    let speedDetails := match speed with
      | Speed.Fast => ""
      | _ => Style.styled (Speed.toStyle speed) s!" ({duration.toInt}ms)"
    tell $ indent path ++
           Style.styled Style.green "✓ " ++
           Style.styled Style.dim name ++
           speedDetails ++ "\n"
  | PrintAction.PrintTest name (Result.Failure _) => do
    state ← get
    let newFailures := state.numFailures + 1
    set { state with numFailures := newFailures }
    tell $ indent path ++
           Style.styled Style.red s!"{newFailures}) {name}" ++ "\n"
  | PrintAction.PrintPending name => do
    tell $ indent path ++
           Style.styled Style.cyan s!"- {name}" ++ "\n"

where
  indent (path : Path) : String :=
    Style.indent (path.length * 2)

-- Spec reporter implementation
def specReporter : Reporter :=
  Base.defaultReporter initialState $ Base.defaultUpdate {
    getRunningItems := (·.runningItems),
    putRunningItems := fun items state => { state with runningItems := items },
    printFinishedItem := fun loc item => do
      match loc, item with
      | (path, name), RunningItem.RunningTest (some result) =>
        print path (PrintAction.PrintTest name result)
      | (path, name), RunningItem.RunningPending =>
        print path (PrintAction.PrintPending name)
      | (path, name), RunningItem.RunningSuite true =>
        print path (PrintAction.PrintSuite name)
      | _, _ => pure (),
    update := fun event => do
      match event with
      | Event.Suite Event.Sequential (path, name) => do
        print path (PrintAction.PrintSuite name)
      | Event.TestEnd (path, name) result => do
        state ← get
        if state.runningItems.find? (path, name) |>.isNone then
          print path (PrintAction.PrintTest name result)
      | Event.Pending (path, name) => do
        state ← get
        if state.runningItems.isEmpty then
          print path (PrintAction.PrintPending name)
      | Event.End results => do
        lift $ Base.defaultSummary results
      | _ => pure ()
  }

end Spec.Reporter.Spec
