module
import Std.Internal.Async
public import Std.Time.Time.Unit.Millisecond

@[expose] public section

namespace Spec

open Std.Internal.IO.Async

inductive MaybeTimedOut (α : Type)
  | timedOut
  | notTimedOut (result : α)
  deriving Repr, BEq, DecidableEq, Hashable, Inhabited, Ord

namespace MaybeTimedOut

def isTimedOut : MaybeTimedOut α → Bool
  | .timedOut => true
  | .notTimedOut _ => false

end MaybeTimedOut

def timeout (maxDuration : Std.Time.Millisecond.Offset) (action : BaseIO a)
    (prio := Task.Priority.default) : BaseIO (Task (MaybeTimedOut a)) := do
  let promise ← IO.Promise.new
  let action' : BaseIO Unit := do
    promise.resolve (MaybeTimedOut.notTimedOut (<- action))
  let _task1 ← action'.asTask (prio := prio)
  let sleepTask : BaseIO Unit := do
    IO.sleep maxDuration.toInt.toNat.toUInt32
    promise.resolve MaybeTimedOut.timedOut
  let _task2 ← sleepTask.asTask (prio := prio)
  return promise.result!

def measureAction (action : BaseIO α) : BaseIO (Nat × α) := do
  let start ← IO.monoMsNow
  let r ← action
  let end_ ← IO.monoMsNow
  return (end_ - start, r)

def executeWithTimeoutAndMeasureTime
  (maxDuration : Std.Time.Millisecond.Offset)
  (action : BaseIO a) :
  BaseIO (Task (MaybeTimedOut (Nat × a))) :=
  timeout maxDuration (measureAction action)

def executeWithMTimeoutAndMeasureTime
  (maxDuration? : Option Std.Time.Millisecond.Offset)
  (action : BaseIO a) :
  BaseIO (Task (MaybeTimedOut (Nat × a))) := do
  match maxDuration? with
  | .some maxDuration => executeWithTimeoutAndMeasureTime maxDuration action
  | .none => (return MaybeTimedOut.notTimedOut (← measureAction action)).asTask

end Spec
end
