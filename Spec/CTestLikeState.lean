module

public import Spec.StateFile.ParserReader
public import Spec.StateFile.PrinterWriter

@[expose] public section

namespace Spec

structure ScheduledLeaf where
  leaf : Leaf Unit
  failed : Bool
  timing? : Option Timing

/-- CTest-style order: previous failures first, then descending historical cost. -/
def orderByTiming (leaves : Array ScheduledLeaf) : Array (Leaf Unit) :=
  (leaves.mergeSort fun a b =>
    if a.failed && !b.failed then
      true
    else if b.failed && !a.failed then
      false
    else
      match a.timing?, b.timing? with
      | some aTiming, some bTiming => aTiming.costMs > bTiming.costMs
      | some _, none => true
      | none, some _ => false
      | none, none => false).map (·.leaf)

end Spec
end
