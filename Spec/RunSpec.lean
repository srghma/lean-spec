module
public import Spec.Tree
public import Spec.Runner
public import Spec.SelectReporter

@[expose] public section

namespace Spec

def runSpecFromArgs (args : List String) (spec : Spec.Spec) : IO Bool := do
  let cfg := Spec.parseArgs args
  let builders :=
    if cfg.reporterNames.isEmpty then
      defaultReporterBuilders
    else
      reporterBuildersForNames cfg.reporterNames
  Spec.runSpecWith cfg builders spec

def runSpecFromArgsAndReturnExitCode (args : List String) (spec : Spec.Spec) : IO UInt32 := do
  let failed ← runSpecFromArgs args spec
  return if failed then 1 else 0

end Spec
