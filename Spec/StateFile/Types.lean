module

public import Std.Data.HashSet.Basic
public import Std.Data.TreeMap.Basic
public import Std.Data.TreeSet.Basic

@[expose] public section

namespace Spec

structure Timing where
  previousRuns : Nat
  costMs : Nat

abbrev Timings := Std.TreeMap String Timing

structure LastRunState where
  failures : Std.TreeSet String
  timings : Timings
  suiteTiming? : Option Timing := none

def emptyLastRunState : LastRunState :=
  { failures := ∅, timings := ∅ }

def timingFor (timings : Timings) (name : String) : Option Timing :=
  timings[name]?

end Spec
end
