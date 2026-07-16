module
public import Spec.Events

@[expose] public section

namespace Spec.Reporter.TeamCity

def escape (s : String) : String :=
  s.replace "|" "||"
    |>.replace "'" "|'"
    |>.replace "\n" "|n"
    |>.replace "\r" "|r"
    |>.replace "[" "|["
    |>.replace "]" "|]"

def property (key value : String) : String := s!" {key}='{escape value}'"

def message (event : String) (props : List (String × String) := []) : String :=
  "##teamcity[" ++ event ++ String.join (props.map (fun (k, v) => property k v)) ++ "]"

def testCount (count : Nat) : String :=
  message "testCount" [("count", toString count)]

def suiteStarted (name : String) : String :=
  message "testSuiteStarted" [("name", name)]

def suiteFinished (name : String) : String :=
  message "testSuiteFinished" [("name", name)]

def testStarted (name : String) : String :=
  message "testStarted" [("name", name)]

def testIgnored (name : String) : String :=
  message "testIgnored" [("name", name)]

def testFinished (name : String) : String :=
  message "testFinished" [("name", name)]

def testFinishedIn (name : String) (durationMs : Nat) : String :=
  message "testFinished" [("name", name), ("duration", toString durationMs)]

def testFailed (name : String) (err : String) : String :=
  message "testFailed" [("name", name), ("message", err)]

def pathPrefix (path : Array String) : List (Array String) :=
  let rec go (i : Nat) (acc : List (Array String)) : List (Array String) :=
    if i < path.size then
      go (i + 1) (acc ++ [path.extract 0 (i + 1)])
    else
      acc
  go 0 []

def teamcityReporter : ReporterBuilder := fun _ => do
  let lastPath ← IO.mkRef (#[] : Array String)
  pure
    { start := fun total => IO.println (testCount total)
    , reportItem := fun res => do
        let prev ← lastPath.get
        let common := prev.zip res.path |>.takeWhile (fun (a, b) => a == b) |>.size
        let closeCount := prev.size - common
        for i in [0:closeCount] do
          let idx := prev.size - 1 - i
          IO.println (suiteFinished prev[idx]!)
        let openPath := res.path.extract common res.path.size
        for p in pathPrefix openPath do
          IO.println (suiteStarted (String.intercalate " » " p.toList))
        lastPath.set res.path
        let title := res.name
        match res.outcome with
        | .success =>
          IO.println (testStarted title)
          IO.println (testFinishedIn title res.durationMs)
        | .pending =>
          IO.println (testStarted title)
          IO.println (testIgnored title)
          IO.println (testFinished title)
        | .failure err =>
          IO.println (testStarted title)
          IO.println (testFailed title err)
          IO.println (testFinished title)
    , reportSummary := fun _ _ _ => do
        let prev ← lastPath.get
        for i in [0:prev.size] do
          let idx := prev.size - 1 - i
          IO.println (suiteFinished prev[idx]!)
        pure () }

end Spec.Reporter.TeamCity
