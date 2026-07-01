module
prelude
public import Spec.Reporter

@[expose] public section

namespace Spec

def reporterBuilderOfName (name : String) : Option Spec.ReporterBuilder :=
  match name.toLower with
  | "console" => some Spec.Reporter.Console.consoleReporter
  | "dot" => some Spec.Reporter.Dot.dotReporter
  | "spec" => some Spec.Reporter.Spec.specReporter
  | "tap" => some Spec.Reporter.Tap.tapReporter
  | "teamcity" => some Spec.Reporter.TeamCity.teamcityReporter
  | _ => none

def reporterBuildersForNames (names : List String) : List Spec.ReporterBuilder :=
  names.filterMap reporterBuilderOfName

def defaultReporterBuilders : List Spec.ReporterBuilder :=
  [Spec.Reporter.Console.consoleReporter]
