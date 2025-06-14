-- Spec/Reporter/TeamCity.lean
import Spec.Reporter.Base
import Spec.Tree
import Spec.Result
import Spec.Event
import Spec.Console

namespace Spec.Reporter.TeamCity

-- Service message structure
structure ServiceMessage (α : Type) where
  name : String
  nodeId : String
  parentNodeId : Option String
  extra : α
deriving Repr

-- Common service message types
abbrev WithMessage := ServiceMessage { message : String }
abbrev WithDuration := ServiceMessage { duration : Float }
abbrev BasicMessage := ServiceMessage Unit

-- Escape special characters for TeamCity format
def escape (s : String) : String :=
  s.replace "|" "||"
   |>.replace "\n" "|n"
   |>.replace "\r" "|r"
   |>.replace "[" "|["
   |>.replace "]" "|]"
   |>.replace "'" "|'"

-- Property formatting helper
def property (key value : String) : String :=
  s!" {key}='{escape value}'"

-- TeamCity message formatter
def teamcity (event : String) (msg : ServiceMessage α) (rest : String := "") : String :=
  s!"##teamcity[{event}" ++
  property "name" msg.name ++
  property "nodeId" msg.nodeId ++
  property "parentNodeId" (msg.parentNodeId.getD "0") ++
  rest ++
  "]"

-- Specific TeamCity commands
def testCount (count : Nat) : String :=
  s!"##teamcity[testCount count='{count}']"

def testSuiteStarted (msg : BasicMessage) : String :=
  teamcity "testSuiteStarted" msg

def testSuiteFinished (msg : BasicMessage) : String :=
  teamcity "testSuiteFinished" msg

def testStarted (msg : BasicMessage) : String :=
  teamcity "testStarted" msg

def testIgnored (msg : BasicMessage) : String :=
  teamcity "testIgnored" msg

def testFinished (msg : BasicMessage) : String :=
  teamcity "testFinished" msg

def testFinishedIn (msg : WithDuration) : String :=
  teamcity "testFinished" ⟨msg.name, msg.nodeId, msg.parentNodeId, ()⟩
    (property "duration" (toString msg.extra.duration.toInt))

def testFailed (msg : WithMessage) (err : Exception) : String :=
  let message := err.toString
  let isEquals := message.contains "≠"

  if isEquals then
    -- Parse expected/actual from comparison failure
    let parts := message.splitOn "≠"
    let expected := (parts.get? 0).getD ""
      |>.trim |>.dropRightWhile (· = ' ')
      |> readString
    let actual := (parts.get? 1).getD ""
      |>.trim |>.drop 1
      |> readString

    teamcity "testFailed" ⟨msg.name, msg.nodeId, msg.parentNodeId, ()⟩
      (property "type" "comparisonFailure" ++
       property "details" (err.getStackTrace.getD "") ++
       property "message" message ++
       property "expected" expected ++
       property "actual" actual)
  else
    teamcity "testFailed" ⟨msg.name, msg.nodeId, msg.parentNodeId, ()⟩
      (property "message" msg.extra.message)

where
  readString (s : String) : String :=
    if s.startsWith "\"" && s.endsWith "\"" then
      s.drop 1 |>.dropRight 1 |>.replace "\\n" "\n"
    else s

-- Create service message from test locator
def serviceMessage (loc : TestLocator) : BasicMessage :=
  let (path, name) := loc
  let nodeId := idFromPath path
  let parentNodeId := Tree.parentSuite path
    |>.map (·.1)
    |>.map idFromPath
  ⟨name, nodeId, parentNodeId, ()⟩

-- Add duration to service message
def withDuration (duration : Float) (msg : BasicMessage) : WithDuration :=
  ⟨msg.name, msg.nodeId, msg.parentNodeId, { duration }⟩

-- Add message to service message
def withMessage (message : String) (msg : BasicMessage) : WithMessage :=
  ⟨msg.name, msg.nodeId, msg.parentNodeId, { message }⟩

-- Generate ID from path
def idFromPath (path : Path) : String :=
  path.toList.map (fun ⟨idx, name⟩ =>
    s!"{idx}:{name.getD ""}")
  |> String.intercalate ","

-- TeamCity reporter implementation
def teamcityReporter : Reporter :=
  Base.defaultReporter () fun event => do
    match event with
    | Event.Suite _ loc => do
      lift $ tell $ testSuiteStarted (serviceMessage loc) ++ "\n"
    | Event.SuiteEnd loc => do
      lift $ tell $ testSuiteFinished (serviceMessage loc) ++ "\n"
    | Event.Test _ loc => do
      lift $ tell $ testStarted (serviceMessage loc) ++ "\n"
    | Event.Pending loc => do
      let attrs := serviceMessage loc
      lift $ tell $ testStarted attrs ++ "\n"
      lift $ tell $ testIgnored attrs ++ "\n"
      lift $ tell $ testFinished attrs ++ "\n"
    | Event.TestEnd loc (Result.Success speed duration) => do
      lift $ tell $ testFinishedIn
        (serviceMessage loc |> withDuration duration.toFloat) ++ "\n"
    | Event.TestEnd loc (Result.Failure error) => do
      let attrs := serviceMessage loc |> withMessage error.toString
      lift $ tell $ testFailed attrs error ++ "\n"
      lift $ tell $ testFinished (serviceMessage loc) ++ "\n"
    | Event.End _ =>
      pure ()
    | Event.Start count => do
      lift $ tell $ testCount count ++ "\n"

end Spec.Reporter.TeamCity
