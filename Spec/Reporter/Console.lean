import Spec.Reporter.Base
import Spec.Tree
import Spec.Result
import Spec.Event
import Spec.Style
import Spec.Summary

namespace Spec.Reporter.Console

-- Console reporter state
structure ConsoleState where
  runningItems : Std.HashMap TestLocator RunningItem
  lastPrintedSuitePath : Option Path
deriving Repr

-- Initial state
def initialState : ConsoleState := {
  runningItems := Std.HashMap.empty,
  lastPrintedSuitePath := none
}

-- Print actions
inductive PrintAction where
  | PrintTest : String → Result → PrintAction
  | PrintPending : String → PrintAction
deriving Repr

-- Print summary at the end
def printSummary (results : Array (Tree String Void Result)) : WriterT String IO Unit := do
  match Summary.summarize results with
  | Summary.count passed failed pending => do
    tell "\n"
    tell $ Style.styled Style.bold "Summary" ++ "\n"
    printPassedFailed passed failed
    printPending pending
    tell "\n"

where
  printPassedFailed (passed failed : Nat) : WriterT String IO Unit := do
    let total := passed + failed
    let testStr := if total == 1 then "test" else "tests"
    let amount := s!"{passed}/{total} {testStr} passed"
    let color := if failed > 0 then Style.red else Style.dim
    tell $ Style.styled color amount ++ "\n"

  printPending (pending : Nat) : WriterT String IO Unit := do
    if pending > 0 then
      let testStr := if pending == 1 then "test" else "tests"
      tell $ Style.styled Style.yellow s!"{pending} {testStr} pending" ++ "\n"

-- Print function with path tracking
def print (path : Path) (action : PrintAction) :
    StateT ConsoleState (WriterT String IO) Unit := do
  state ← get
  match state.lastPrintedSuitePath with
  | some p =>
    if p != path then do
      tell $ Style.styled (Style.bold ++ Style.magenta)
        (String.intercalate " » " (Tree.parentSuiteName path)) ++ "\n"
      set { state with lastPrintedSuitePath := some path }
  | none => do
    tell $ Style.styled (Style.bold ++ Style.magenta)
      (String.intercalate " » " (Tree.parentSuiteName path)) ++ "\n"
    set { state with lastPrintedSuitePath := some path }

  match action with
  | PrintAction.PrintTest name (Result.Success _) => do
    tell $ "  " ++ Style.styled Style.green "✓ " ++ Style.styled Style.dim name ++ "\n"
  | PrintAction.PrintTest name (Result.Failure err) => do
    tell $ "  " ++ Style.styled Style.red s!"✗ {name}:" ++ "\n"
    tell "\n"
    tell $ "  " ++ Style.styled Style.red err.toString ++ "\n"
  | PrintAction.PrintPending name => do
    tell $ "  " ++ Style.styled Style.cyan s!"~ {name}" ++ "\n"

-- Console reporter implementation
def consoleReporter : Reporter :=
  Base.defaultReporter initialState $ Base.defaultUpdate {
    getRunningItems := (·.runningItems),
    putRunningItems := fun items state => { state with runningItems := items },
    printFinishedItem := fun loc item => do
      match loc, item with
      | (path, name), RunningItem.RunningTest (some result) =>
        print path (PrintAction.PrintTest name result)
      | (path, name), RunningItem.RunningPending =>
        print path (PrintAction.PrintPending name)
      | _, _ => pure (),
    update := fun event => do
      match event with
      | Event.testEnd (path, name) result => do
        state ← get
        if state.runningItems.find? (path, name) |>.isNone then
          print path (PrintAction.PrintTest name result)
      | Event.pending (path, name) => do
        state ← get
        if state.runningItems.isEmpty then
          print path (PrintAction.PrintPending name)
      | Event.end results => do
        lift $ printSummary results
      | _ => pure ()
  }

end Spec.Reporter.Console
