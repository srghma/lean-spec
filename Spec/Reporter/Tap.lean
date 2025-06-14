-- Spec/Reporter/Tap.lean
import Spec.Reporter.Base
import Spec.Result
import Spec.Event
import Spec.Summary

namespace Spec.Reporter.Tap

-- TAP reporter state (test counter)
abbrev TapReporterState := Nat

-- Escape title for TAP format (remove # characters)
def escTitle (s : String) : String :=
  s.replace "#" ""

-- Escape message for TAP format (indent all lines)
def escMsg (s : String) : String :=
  s.splitOn "\n"
  |>.map ("  " ++ ·)
  |> String.intercalate "\n"

-- TAP reporter implementation
def tapReporter : Reporter :=
  Base.defaultReporter (1 : TapReporterState) fun event => do
    match event with
    | Event.start nTests => do
      lift $ tell s!"1..{nTests}\n"
    | Event.pending (_, name) => do
      n ← get
      lift $ tell s!"ok {n} {escTitle name} # SKIP -\n"
      set (n + 1)
    | Event.testEnd (_, name) (Result.Success _ _) => do
      n ← get
      lift $ tell s!"ok {n} {escTitle name}\n"
      set (n + 1)
    | Event.testEnd (_, name) (Result.Failure err) => do
      n ← get
      lift $ tell s!"not ok {n} {escTitle name}\n"
      lift $ tell $ escMsg err.toString ++ "\n"
      match err.getStackTrace with
      | some stack =>
        let indentedStack := stack.splitOn "\n"
          |>.map ("    " ++ ·)
          |> String.intercalate "\n"
        lift $ tell $ indentedStack ++ "\n"
      | none => pure ()
      set (n + 1)s
    | Event.end results => do
      match Summary.summarize results with
      | Summary.Count passed failed pending => do
        lift $ tell s!"# tests {failed + passed + pending}\n"
        lift $ tell s!"# pass {passed + pending}\n"
        lift $ tell s!"# fail {failed}\n"
    | _ =>
      pure ()

end Spec.Reporter.Tap
