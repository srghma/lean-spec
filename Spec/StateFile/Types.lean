module

public import Std.Data.HashMap.Basic
public import Std.Data.HashSet.Basic

@[expose] public section

namespace Spec

structure Timing where
  previousRuns : Nat
  costMs : Nat

abbrev Timings := Std.HashMap String Timing

structure LastRunState where
  failures : Std.HashSet String
  timings : Timings

def emptyLastRunState : LastRunState :=
  { failures := Std.HashSet.emptyWithCapacity, timings := Std.HashMap.emptyWithCapacity }

def timingFor (timings : Timings) (name : String) : Option Timing :=
  timings[name]?

end Spec
end
